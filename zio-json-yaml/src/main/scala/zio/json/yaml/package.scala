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
import scala.util.Try
import scala.util.matching.Regex

package object yaml {

  implicit final class JsonOps(private val json: Json) extends AnyVal {
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
          case None =>
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

    def toYamlAST(options: YamlOptions = YamlOptions.default): Node = jsonToYaml(json, options)
  }

  implicit final class EncoderYamlOps[A](private val a: A) extends AnyVal {
    def toYaml(options: YamlOptions = YamlOptions.default)(implicit A: JsonEncoder[A]): Either[String, String] =
      a.toJsonAST.flatMap(_.toYaml(options).left.map(_.getMessage))
    def toYamlAST(options: YamlOptions = YamlOptions.default)(implicit A: JsonEncoder[A]): Either[String, Node] =
      a.toJsonAST.map(_.toYamlAST(options))
  }

  implicit final class DecoderYamlOps(private val raw: String) extends AnyVal {
    def fromYaml[A](implicit decoder: JsonDecoder[A]): Either[String, A] =
      Try {
        val yaml = new Yaml().compose(new StringReader(raw))
        yamlToJson(yaml)
      }.toEither.left
        .map(_.getMessage)
        .flatMap(decoder.fromJsonAST(_))
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
