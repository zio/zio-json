package zio.json

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Define a config for derivation macro
 */
sealed abstract class JsonDeriveConfig

object JsonDeriveConfig {
  // Derive a JsonCodec
  case object Codec extends JsonDeriveConfig

  // Derive only a JsonEncoder
  case object Encoder extends JsonDeriveConfig

  // Derive only a JsonDecoder
  case object Decoder extends JsonDeriveConfig
}

class jsonDerive(
  val config: JsonDeriveConfig = JsonDeriveConfig.Codec
) extends scala.annotation.StaticAnnotation {
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
    val tpname      = clsDef.name
    val tparams     = clsDef.tparams
    val decoderName = TermName("decode" + tpname.decodedName)
    val encoderName = TermName("encode" + tpname.decodedName)
    val codecName   = TermName("codecFor" + tpname.decodedName)
    val (decoder, encoder, codec) = if (tparams.isEmpty) {
      val Type = tpname
      (
        q"""implicit val $decoderName: $DecoderClass[$Type] = _root_.zio.json.DeriveJsonDecoder.gen[$Type]""",
        q"""implicit val $encoderName: $EncoderClass[$Type] = _root_.zio.json.DeriveJsonEncoder.gen[$Type]""",
        q"""implicit val $codecName: $CodecClass[$Type] = _root_.zio.json.DeriveJsonCodec.gen[$Type]"""
      )
    } else {
      val tparamNames = tparams.map(_.name)
      def mkImplicitParams(prefix: String, typeSymbol: TypeSymbol) =
        tparamNames.zipWithIndex.map { case (tparamName, i) =>
          val paramName = TermName(s"$prefix$i")
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
