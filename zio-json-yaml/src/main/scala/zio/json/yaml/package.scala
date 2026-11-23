package zio.json

import org.yaml.snakeyaml.DumperOptions.{ NonPrintableStyle, ScalarStyle }
import org.yaml.snakeyaml.emitter.Emitter
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{ ScalarNode, _ }
import org.yaml.snakeyaml.reader.StreamReader
import org.yaml.snakeyaml.resolver.Resolver
import org.yaml.snakeyaml.serializer._
import org.yaml.snakeyaml.Yaml
import zio.Chunk
import zio.json.ast.Json
import zio.json.yaml.internal.YamlValueConstruction

import java.io.{ StringReader, StringWriter }
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.util.matching.Regex

/**
 * YAML support for ZIO JSON, built on SnakeYAML.
 *
 * Import `zio.json.yaml._` to get `toYaml` and `toYamlAST` on a `Json` or on any value that has a `JsonEncoder`, and
 * `fromYaml` on a `String`. Values go through the `Json` AST: they are encoded to a `Json` and then written as YAML, and
 * YAML text is parsed to a `Json` and then decoded.
 *
 * {{{
 * import zio.json._
 * import zio.json.yaml._
 *
 * final case class Server(host: String, port: Int)
 * object Server {
 *   implicit val codec: JsonCodec[Server] = DeriveJsonCodec.gen
 * }
 *
 * Server("localhost", 8080).toYaml()
 * // Right("host: localhost\nport: 8080\n")
 *
 * "host: example.com\nport: 443\n".fromYaml[Server]
 * // Right(Server(example.com,443))
 * }}}
 *
 * @see [[https://zio.dev/zio-json/ ZIO JSON]]
 */
package object yaml {

  /**
   * Adds `toYaml` and `toYamlAST` to a [[zio.json.ast.Json]].
   */
  implicit final class JsonOps(private val json: Json) extends AnyVal {
    /**
     * Writes this value as YAML text, formatted according to `options`.
     *
     * The text ends with a line break. The result is `Left` when SnakeYAML fails to write the document.
     *
     * Objects become YAML mappings and arrays become sequences. Integer-valued numbers are written through `Long`, so values
     * outside the `Long` range are not preserved. Null fields of objects are dropped when `options.dropNulls` is set, which is
     * the default.
     *
     * {{{
     * import zio.json.ast.Json
     *
     * val json = Json.Obj(
     *   "name"  -> Json.Str("Ada"),
     *   "langs" -> Json.Arr(Json.Str("en"), Json.Str("fr"))
     * )
     *
     * json.toYaml()
     * // Right("name: Ada\nlangs:\n  - en\n  - fr\n")
     * }}}
     *
     * @param options
     *   How to format the YAML; `YamlOptions.default` when omitted
     * @return
     *   The YAML text, or the SnakeYAML error
     */
    def toYaml(options: YamlOptions = YamlOptions.default): Either[YAMLException, String] = {
      val yamlNode = toYamlAST(options)

      try {
        val dumperOptions = options.newDumperOptions()
        dumperOptions.setIndent(options.indentation)
        dumperOptions.setIndicatorIndent(options.sequenceIndentation)
        options.maxScalarWidth match {
          case Some(width) =>
            dumperOptions.setWidth(width)
            dumperOptions.setSplitLines(true)
          case _ =>
            dumperOptions.setSplitLines(false)
        }
        dumperOptions.setLineBreak(options.lineBreak)
        dumperOptions.setIndentWithIndicator(options.indentWithIndicator)

        val resolver   = new Resolver
        val output     = new StringWriter()
        val serializer = new Serializer(new Emitter(output, dumperOptions), resolver, dumperOptions, yamlNode.getTag)
        serializer.open()
        try {
          serializer.serialize(yamlNode)
        } finally {
          serializer.close()
        }

        Right(output.toString)
      } catch {
        case error: YAMLException => Left(error)
      }
    }

    /**
     * Converts this value to a SnakeYAML node tree, according to `options`.
     *
     * Use it to take over serialization with SnakeYAML directly. `toYaml` writes this tree.
     */
    def toYamlAST(options: YamlOptions = YamlOptions.default): Node = jsonToYaml(json, options)
  }

  /**
   * Adds `toYaml` and `toYamlAST` to any value that has a `JsonEncoder`.
   */
  implicit final class EncoderYamlOps[A](private val a: A) extends AnyVal {
    /**
     * Encodes this value to a `Json` with the implicit `JsonEncoder` and writes it as YAML text, formatted according to `options`.
     *
     * The result is `Left` with an error message when encoding fails or when writing the YAML fails.
     *
     * {{{
     * import zio.json.ast.Json
     *
     * Json.Arr(Json.Num(1), Json.Num(2)).toYaml()
     * // Right("  - 1\n  - 2\n")
     * }}}
     *
     * @param options
     *   How to format the YAML; `YamlOptions.default` when omitted
     */
    def toYaml(options: YamlOptions = YamlOptions.default)(implicit A: JsonEncoder[A]): Either[String, String] =
      a.toJsonAST.flatMap(_.toYaml(options).left.map(_.getMessage))
    /**
     * Encodes this value to a `Json` with the implicit `JsonEncoder` and converts it to a SnakeYAML node tree, according to
     * `options`.
     *
     * The result is `Left` with an error message when encoding fails.
     */
    def toYamlAST(options: YamlOptions = YamlOptions.default)(implicit A: JsonEncoder[A]): Either[String, Node] =
      a.toJsonAST.map(_.toYamlAST(options))
  }

  /**
   * Adds `fromYaml` to a `String` that holds YAML text.
   */
  implicit final class DecoderYamlOps(private val raw: String) extends AnyVal {
    /**
     * Parses this text as one YAML document and decodes it to an `A` with the implicit `JsonDecoder`.
     *
     * Anchors and aliases are resolved. Plain scalars are typed by the YAML core rules, for example `~` is null, `0x10` is 16 and
     * `2020-01-01` stays a string. The result is `Left` with an error message when the text is not valid YAML, when it holds more
     * than one document, when a mapping has a key that is not a scalar, or when decoding fails.
     *
     * {{{
     * import zio.json.ast.Json
     *
     * """
     * name: Ada
     * langs: [en, fr]
     * """.fromYaml[Json]
     * // Right({"name":"Ada","langs":["en","fr"]})
     *
     * "- 1\n- 2\n".fromYaml[List[Int]]
     * // Right(List(1, 2))
     * }}}
     */
    def fromYaml[A](implicit decoder: JsonDecoder[A]): Either[String, A] =
      try {
        val yaml = new Yaml().compose(new StringReader(raw))
        decoder.fromJsonAST(yamlToJson(yaml))
      } catch {
        case NonFatal(e) => Left(e.getMessage)
      }
  }

  private val multiline: Regex = "[\n\u0085\u2028\u2029]".r

  private final def jsonToYaml(json: Json, options: YamlOptions): Node =
    json match {
      case Json.Obj(fields) =>
        val finalFields =
          if (options.dropNulls) {
            fields.filter { case (_, value) =>
              value match {
                case Json.Null => false
                case _         => true
              }
            }
          } else {
            fields
          }
        new MappingNode(
          Tag.MAP,
          finalFields.map { case (key, value) =>
            new NodeTuple(
              new ScalarNode(Tag.STR, key, null, null, options.keyStyle(key)),
              jsonToYaml(value, options)
            )
          }.toList.asJava,
          options.flowStyle(json)
        )
      case Json.Arr(elements) =>
        new SequenceNode(
          Tag.SEQ,
          elements.map(jsonToYaml(_, options)).toList.asJava,
          options.flowStyle(json)
        )
      case Json.Bool(value) =>
        new ScalarNode(Tag.BOOL, value.toString, null, null, options.scalarStyle(json))
      case Json.Str(value) =>
        if (options.nonPrintableStyle == NonPrintableStyle.BINARY && !StreamReader.isPrintable(value)) {
          new ScalarNode(
            Tag.BINARY,
            Base64.getEncoder.encodeToString(value.getBytes(StandardCharsets.UTF_8)),
            null,
            null,
            ScalarStyle.LITERAL
          )
        } else {
          val isMultiLine = multiline.findFirstIn(value).isDefined
          val style       = options.scalarStyle(json)
          val finalStyle  = if (style == ScalarStyle.PLAIN && isMultiLine) ScalarStyle.LITERAL else style
          new ScalarNode(Tag.STR, value, null, null, finalStyle)
        }
      case Json.Num(value) =>
        val stripped = value.stripTrailingZeros()
        if (stripped.scale() <= 0) {
          new ScalarNode(Tag.INT, stripped.longValue.toString, null, null, options.scalarStyle(json))
        } else {
          new ScalarNode(Tag.FLOAT, stripped.toString, null, null, options.scalarStyle(json))
        }
      case Json.Null =>
        new ScalarNode(Tag.NULL, "null", null, null, options.scalarStyle(json))
    }

  private final def yamlToJson(yaml: Node): Json = {
    val construction = new YamlValueConstruction

    def loop(node: Node): Json =
      node match {
        case scalar: ScalarNode =>
          construction.toJavaValue(scalar) match {
            case null                     => Json.Null
            case s: String                => Json.Str(s)
            case b: java.lang.Boolean     => Json.Bool(b)
            case i: java.lang.Integer     => Json.Num(i)
            case l: java.lang.Long        => Json.Num(l)
            case bi: java.math.BigInteger => Json.Num(scala.math.BigDecimal(bi))
            case f: java.lang.Float       => Json.Num(f)
            case d: java.lang.Double      => Json.Num(d)
            case arr: Array[Byte]         => Json.Str(new String(arr, StandardCharsets.UTF_8))
            case _                        => Json.Str(scalar.getValue)
          }

        case sequence: SequenceNode =>
          Json.Arr(
            Chunk.fromIterable(
              sequence.getValue.asScala
                .map(yamlToJson)
            )
          )
        case mapping: MappingNode =>
          Json.Obj(
            Chunk.fromIterable(
              construction
                .processMappingNode(mapping)
                .getValue
                .asScala
                .map { tuple =>
                  val keyStr = tuple.getKeyNode match {
                    case scalarKey: ScalarNode => scalarKey.getValue
                    case _                     => throw new YAMLException("Mapping key is not scalar")
                  }
                  val jsonValue = loop(tuple.getValueNode)
                  keyStr -> jsonValue
                }
            )
          )
        case _ =>
          throw new YAMLException(s"Unsupported node type: ${node.getClass.getSimpleName}")
      }

    loop(yaml)
  }
}
