package zio.json

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Chooses what [[jsonDerive]] generates: a codec (`JsonDeriveConfig.Codec`, the default), only an encoder
 * (`JsonDeriveConfig.Encoder`) or only a decoder (`JsonDeriveConfig.Decoder`). The macro reads the static type of the
 * argument, so pass one of the three objects directly; for any other expression of type `JsonDeriveConfig` it prints a
 * warning and generates a codec.
 */
sealed abstract class JsonDeriveConfig

/** The three values of [[JsonDeriveConfig]]. */
object JsonDeriveConfig {

  /**
   * Makes [[jsonDerive]] generate an implicit `JsonCodec`, named `codecFor` and the type name, such as `codecForBook`.
   */
  case object Codec extends JsonDeriveConfig

  /**
   * Makes [[jsonDerive]] generate only an implicit `JsonEncoder`, named `encode` and the type name, such as
   * `encodeBook`.
   */
  case object Encoder extends JsonDeriveConfig

  /**
   * Makes [[jsonDerive]] generate only an implicit `JsonDecoder`, named `decode` and the type name, such as
   * `decodeBook`.
   */
  case object Decoder extends JsonDeriveConfig
}

/**
 * A Scala 2 macro annotation that adds an implicit codec, encoder or decoder to the companion object of the annotated
 * case class or sealed trait, so you do not write the `DeriveJsonCodec.gen` line yourself. The generated member calls
 * `DeriveJsonCodec.gen`, `DeriveJsonEncoder.gen` or `DeriveJsonDecoder.gen`, depending on `config`, so the derivation
 * annotations on the type and the `JsonCodecConfiguration` in scope apply as usual. When the type has no companion, the
 * macro creates one. For a type with type parameters it generates an `implicit def` that takes an encoder or a decoder
 * (or both, for a codec) for each type parameter.
 *
 * It needs `-Ymacro-annotations` on Scala 2.13 or the macro paradise compiler plugin on Scala 2.12. Without them the
 * annotation is ignored, no instance is generated, and the first use fails to compile with a missing implicit. On
 * another kind of class, compilation fails with
 * `Invalid annotation target: must be a case class or a sealed trait/class`. Write the annotation as `@jsonDerive`
 * after an import: `@zio.json.jsonDerive` fails with `Unsupported macroname supplied to @zio.json.jsonDerive`. The
 * module is not published for Scala 3; use a `derives` clause there.
 *
 * {{{
 * // no-check: needs -Ymacro-annotations
 * @jsonDerive final case class Book(title: String, pages: Int)
 *
 * Book("Dune", 412).toJson                          // {"title":"Dune","pages":412}
 * """{"title":"Dune","pages":412}""".fromJson[Book] // Right(Book(Dune,412))
 * Book.codecForBook                                 // the generated JsonCodec[Book]
 * }}}
 *
 * @param config
 *   what to generate, see [[JsonDeriveConfig]]
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
class jsonDerive(
  val config: JsonDeriveConfig = JsonDeriveConfig.Codec
) extends scala.annotation.StaticAnnotation {

  /** The macro that expands the annotation. The compiler calls it; it is not called directly. */
  def macroTransform(annottees: Any*): Any = macro DeriveCodecMacros.jsonCodecAnnotationMacro
}

// TODO aparo: Restore when it will be possible to define functions to transform object keys
//class snakeCaseJsonDerive(
//  config: JsonDeriveConfig = JsonDeriveConfig.Codec
//) extends scala.annotation.StaticAnnotation {
//  def macroTransform(annottees: Any*): Any = macro DeriveCodecMacros.jsonCodecAnnotationMacro
//}
//
//class kebabCaseJsonDerive(
//  config: JsonDeriveConfig = JsonDeriveConfig.Codec
//) extends scala.annotation.StaticAnnotation {
//  def macroTransform(annottees: Any*): Any = macro DeriveCodecMacros.jsonCodecAnnotationMacro
//}

private[json] final class DeriveCodecMacros(val c: blackbox.Context) {
  import c.universe._

  def jsonCodecAnnotationMacro(annottees: Tree*): Tree = constructJsonCodec(annottees: _*)

  private[this] def isSealed(clsDef: ClassDef): Boolean = clsDef.mods.hasFlag(Flag.SEALED)

  private[this] def isCaseClassOrSealed(clsDef: ClassDef) =
    clsDef.mods.hasFlag(Flag.CASE) || isSealed(clsDef)

  private[this] final def constructJsonCodec(annottees: Tree*): Tree = annottees match {
    case List(clsDef: ClassDef) if isCaseClassOrSealed(clsDef) =>
      q"""
       $clsDef
       object ${clsDef.name.toTermName} {
         ${codec(clsDef)}
       }
       """
    case List(
          clsDef: ClassDef,
          q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        ) if isCaseClassOrSealed(clsDef) =>
      q"""
       $clsDef
       object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
         ..$objDefs
         ${codec(clsDef)}
       }
       """
    case _ => c.abort(c.enclosingPosition, "Invalid annotation target: must be a case class or a sealed trait/class")
  }

  private[this] val DecoderClass = typeOf[JsonDecoder[_]].typeSymbol.asType
  private[this] val EncoderClass = typeOf[JsonEncoder[_]].typeSymbol.asType
  private[this] val CodecClass   = typeOf[JsonCodec[_]].typeSymbol.asType

  private[this] val macroName: Tree =
    c.prefix.tree match {
      case Apply(Select(New(name), _), _) => name
      case _                              => c.abort(c.enclosingPosition, "Unexpected macro application")
    }

  private[this] val (codecStyle: JsonCodecStyle, codecType: JsonCodecType) = {
    val style: JsonCodecStyle = macroName match {
      case Ident(TypeName("snakeCaseJsonDerive")) => JsonCodecStyle.SnakeCaseJsonCodec
      case Ident(TypeName("kebabCaseJsonDerive")) => JsonCodecStyle.KebabCaseJsonCodec
      case Ident(TypeName("jsonDerive"))          => JsonCodecStyle.DefaultJsonCodec
      case _                                      => c.abort(c.enclosingPosition, s"Unsupported macroname supplied to @$macroName")
    }

    c.prefix.tree match {
      case q"new ${`macroName`}()"              => (style, JsonCodecType.Codec)
      case q"new ${`macroName`}(config = $cfg)" => (style, codecFrom(c.typecheck(cfg)))
      case q"new ${`macroName`}($cfg)"          => (style, codecFrom(c.typecheck(cfg)))
      case _                                    => c.abort(c.enclosingPosition, s"Unsupported arguments supplied to @$macroName")
    }
  }

  private[this] def codecFrom(tree: Tree): JsonCodecType =
    tree.tpe.dealias match {
      case t if t.toString == "zio.json.JsonDeriveConfig.Codec.type" =>
        JsonCodecType.Codec
      case t if t.toString == "zio.json.JsonDeriveConfig.Decoder.type" =>
        JsonCodecType.DecodeOnly
      case t if t.toString == "zio.json.JsonDeriveConfig.Encoder.type" =>
        JsonCodecType.EncodeOnly
      case t =>
        c.warning(
          c.enclosingPosition,
          s"Couldn't determine type of configuration $t; will produce both encoder and decoder"
        )
        JsonCodecType.Codec
    }

  private[this] def codec(clsDef: ClassDef): Tree = {
    val tpname                    = clsDef.name
    val tparams                   = clsDef.tparams
    val decoderName               = TermName("decode" + tpname.decodedName)
    val encoderName               = TermName("encode" + tpname.decodedName)
    val codecName                 = TermName("codecFor" + tpname.decodedName)
    val (decoder, encoder, codec) = if (tparams.isEmpty) {
      val Type = tpname
      (
        q"""implicit val $decoderName: $DecoderClass[$Type] = _root_.zio.json.DeriveJsonDecoder.gen[$Type]""",
        q"""implicit val $encoderName: $EncoderClass[$Type] = _root_.zio.json.DeriveJsonEncoder.gen[$Type]""",
        q"""implicit val $codecName: $CodecClass[$Type] = _root_.zio.json.DeriveJsonCodec.gen[$Type]"""
      )
    } else {
      val tparamNames                                              = tparams.map(_.name)
      def mkImplicitParams(prefix: String, typeSymbol: TypeSymbol) =
        tparamNames.map {
          var i = 0
          tparamName =>
            val paramName = TermName(s"$prefix$i")
            i += 1
            val paramType = tq"$typeSymbol[$tparamName]"
            q"$paramName: $paramType"
        }
      val decodeParams = mkImplicitParams("decode", DecoderClass)
      val encodeParams = mkImplicitParams("encode", EncoderClass)
      val Type         = tq"$tpname[..$tparamNames]"
      (
        q"""implicit def $decoderName[..$tparams](implicit ..$decodeParams): $DecoderClass[$Type] =
           _root_.zio.json.DeriveJsonDecoder.gen[$Type]""",
        q"""implicit def $encoderName[..$tparams](implicit ..$encodeParams): $EncoderClass[$Type] =
           _root_.zio.json.DeriveJsonEncoder.gen[$Type]""",
        q"""implicit def $codecName[..$tparams](implicit
            ..${decodeParams ++ encodeParams}
          ): $CodecClass[$Type] =
            _root_.zio.json.DeriveJsonCodec.gen[$Type]"""
      )
    }
    codecType match {
      case JsonCodecType.Codec      => codec
      case JsonCodecType.DecodeOnly => decoder
      case JsonCodecType.EncodeOnly => encoder
    }
  }
}

private sealed trait JsonCodecStyle
private object JsonCodecStyle {
  case object DefaultJsonCodec   extends JsonCodecStyle
  case object SnakeCaseJsonCodec extends JsonCodecStyle
  case object KebabCaseJsonCodec extends JsonCodecStyle
}

private sealed trait JsonCodecType
private object JsonCodecType {
  case object Codec      extends JsonCodecType
  case object DecodeOnly extends JsonCodecType
  case object EncodeOnly extends JsonCodecType
}
