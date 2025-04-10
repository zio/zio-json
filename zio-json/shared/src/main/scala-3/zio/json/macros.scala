package zio.json

import scala.annotation.*
import magnolia1.*
import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import zio.Chunk
import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.{ FieldEncoder, Lexer, RecordingReader, RetractReader, StringMatrix, Write }

import scala.annotation._
import scala.collection.Factory
import scala.collection.mutable
import scala.language.experimental.macros

/**
 * If used on a case class field, determines the name of the JSON field.
 * Defaults to the case class field name.
 */
final case class jsonField(name: String) extends Annotation

/**
 * If used on a case class field, determines the alternative names of the JSON field.
 */
final case class jsonAliases(alias: String, aliases: String*) extends Annotation

/**
 * Empty option fields will be encoded as `null`.
 */
final class jsonExplicitNull extends Annotation

/**
 * When disabled keys with empty collections will be omitted from the JSON.
 */
final case class jsonExplicitEmptyCollections(encoding: Boolean = true, decoding: Boolean = true) extends Annotation

/**
 * If used on a sealed class, will determine the name of the field for
 * disambiguating classes.
 *
 * The default is to not use a typehint field and instead
 * have an object with a single key that is the class name.
 *
 * Note that using a discriminator is less performant, uses more memory, and may
 * be prone to DOS attacks that are impossible with the default encoding. In
 * addition, there is slightly less type safety when using custom product
 * encoders (which must write an unenforced object type). Only use this option
 * if you must model an externally defined schema.
 */
final case class jsonDiscriminator(name: String) extends Annotation
// TODO a strategy where the constructor is inferred from the field names, only
// valid if there is no ambiguity in the types of fields for all case classes.
// Such a strategy cannot be implemented with Magnolia because the SealedTrait
// does not provide a mechanism for obtaining the CaseClass associated to the
// Subtype.

sealed trait JsonMemberFormat extends (String => String)

case class CustomCase(f: String => String) extends JsonMemberFormat {
  override def apply(memberName: String): String = f(memberName)
}
case object SnakeCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '_')
}
case object CamelCase extends JsonMemberFormat {
  override def apply(memberName: String): String =
    jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = false)
}

case object PascalCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = true)
}
case object KebabCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '-')
}
case object IdentityFormat extends JsonMemberFormat {
  override def apply(memberName: String): String = memberName
}

/** zio-json version 0.3.0 formats. abc123Def -> abc_123_def */
object ziojson_03 {
  case object SnakeCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '_')
  }
  case object KebabCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '-')
  }
}

/**
 * If used on a case class, determines the strategy of member names
 * transformation during serialization and deserialization. Four common
 * strategies are provided above and a custom one to support specific use cases.
 */
final case class jsonMemberNames(format: JsonMemberFormat) extends Annotation
private[json] object jsonMemberNames {

  import java.lang.Character._

  def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String =
    if (s.indexOf('_') == -1 && s.indexOf('-') == -1) {
      if (s.isEmpty) s
      else {
        val ch = s.charAt(0)
        val fixedCh =
          if (toPascal) toUpperCase(ch)
          else toLowerCase(ch)
        s"$fixedCh${s.substring(1)}"
      }
    } else {
      val len             = s.length
      val sb              = new StringBuilder(len)
      var i               = 0
      var isPrecedingDash = toPascal
      while (i < len) isPrecedingDash = {
        val ch = s.charAt(i)
        i += 1
        (ch == '_' || ch == '-') || {
          val fixedCh =
            if (isPrecedingDash) toUpperCase(ch)
            else toLowerCase(ch)
          sb.append(fixedCh)
          false
        }
      }
      sb.toString
    }

  def enforceSnakeOrKebabCase(s: String, separator: Char): String = {
    val len                      = s.length
    val sb                       = new StringBuilder(len << 1)
    var i                        = 0
    var isPrecedingNotUpperCased = false
    while (i < len) isPrecedingNotUpperCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (!isUpperCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingNotUpperCased || i > 1 && i < len && !isUpperCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

  def enforceSnakeOrKebabCaseSeparateNumbers(s: String, separator: Char): String = {
    val len                   = s.length
    val sb                    = new StringBuilder(len << 1)
    var i                     = 0
    var isPrecedingLowerCased = false
    while (i < len) isPrecedingLowerCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (isLowerCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingLowerCased || i > 1 && i < len && isLowerCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

}

/**
 * If used on a case class will determine the type hint value for disambiguating
 * sealed traits. Defaults to the short type name.
 */
final case class jsonHint(name: String) extends Annotation

/**
 * If used on a sealed class will determine the strategy of type hint value transformation for disambiguating
 * classes during serialization and deserialization. Same strategies are provided as for [[jsonMemberNames]].
 */
final case class jsonHintNames(format: JsonMemberFormat) extends Annotation

/**
 * If used on a case class, will exit early if any fields are in the JSON that
 * do not correspond to field names in the case class.
 *
 * This adds extra protections against a DOS attacks but means that changes in
 * the schema will result in a hard error rather than silently ignoring those
 * fields.
 *
 * Cannot be combined with `@jsonDiscriminator` since it is considered an extra
 * field from the perspective of the case class.
 */
final class jsonNoExtraFields extends Annotation

/**
 * If used on a case class field, will exclude it from the resulting JSON.
 */
final class jsonExclude extends Annotation

private class CaseObjectDecoder[Typeclass[*], A](val ctx: CaseClass[Typeclass, A], no_extra: Boolean)
    extends CollectionJsonDecoder[A] {
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
    if (no_extra) {
      Lexer.char(trace, in, '{')
      Lexer.char(trace, in, '}')
    } else Lexer.skipValue(trace, in)
    ctx.rawConstruct(Nil)
  }

  override def unsafeDecodeMissing(trace: List[JsonError]): A = ctx.rawConstruct(Nil)

  override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
    json match {
      case _: Json.Obj | Json.Null => ctx.rawConstruct(Nil)
      case _                       => Lexer.error("expected object", trace)
    }
}

sealed class JsonDecoderDerivation(config: JsonCodecConfiguration) extends Derivation[JsonDecoder] { self =>
  def join[A](ctx: CaseClass[Typeclass, A]): JsonDecoder[A] = {
    val nameTransform: String => String =
      ctx.annotations.collectFirst { case jsonMemberNames(format) => format }.getOrElse(config.fieldNameMapping)
    val no_extra = ctx.annotations.collectFirst {
      case _: jsonNoExtraFields => ()
    }.isDefined || !config.allowExtraFields
    if (ctx.params.isEmpty) new CaseObjectDecoder(ctx, no_extra)
    else {
      var splitIndex = -1
      val (names, aliases): (Array[String], Array[(String, Int)]) = {
        val names = new Array[String](ctx.params.size)
        val aliasesBuilder = Array.newBuilder[(String, Int)]
        ctx.params.foreach {
          var idx = 0
          p =>
            names(idx) = p.annotations.collectFirst { case jsonField(name) => name }.getOrElse(nameTransform(p.label))
            aliasesBuilder ++= p.annotations.flatMap {
              case jsonAliases(alias, aliases*) => (alias +: aliases).map(_ -> idx)
              case _ => Seq.empty
            }
            idx += 1
            if (splitIndex < 0 && idx + aliasesBuilder.length > 64) splitIndex = idx - 1
        }
        val aliases = aliasesBuilder.result()
        val allFieldNames = names ++ aliases.map(_._1)
        if (allFieldNames.length != allFieldNames.distinct.length) {
          val aliasNames = aliases.map(_._1)
          val collisions = aliasNames
            .filter(alias => names.contains(alias) || aliases.count { case (a, _) => a == alias } > 1)
            .distinct
          val msg = s"Field names and aliases in case class ${ctx.typeInfo.full} must be distinct, " +
            s"alias(es) ${collisions.mkString(",")} collide with a field or another alias"
          throw new AssertionError(msg)
        }
        (names, aliases)
      }
      if (splitIndex < 0) {
        new CollectionJsonDecoder[A] {
          private val len = names.length
          private val matrix = new StringMatrix(names, aliases)
          private val spans = names.map(JsonError.ObjectAccess(_))
          private val defaults = IArray.genericWrapArray(ctx.params.map(_.evaluateDefault.orNull)).toArray
          private lazy val tcs =
            IArray.genericWrapArray(ctx.params.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
          private lazy val namesMap = (names.zipWithIndex ++ aliases).toMap
          private val explicitEmptyCollections =
            ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
              a.decoding
            }.getOrElse(config.explicitEmptyCollections.decoding)
          private val missingValueDecoder =
            if (explicitEmptyCollections) {
              lazy val missingValueDecoders = tcs.map { d =>
                if (allowMissingValueDecoder(d)) d
                else null
              }
              (idx: Int, trace: List[JsonError]) => {
                val trace_ = spans(idx) :: trace
                val decoder = missingValueDecoders(idx)
                if (decoder eq null) Lexer.error("missing", trace_)
                decoder.unsafeDecodeMissing(trace_)
              }
            } else {
              (idx: Int, trace: List[JsonError]) => tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
            }

          @tailrec
          private def allowMissingValueDecoder(d: JsonDecoder[_]): Boolean = d match {
            case _: OptionJsonDecoder[_]     => true
            case _: CollectionJsonDecoder[_] => !explicitEmptyCollections
            case d: MappedJsonDecoder[_] => allowMissingValueDecoder(d.underlying)
            case _                       => true
          }

          override def unsafeDecodeMissing(trace: List[JsonError]): A = {
            val ps  = new Array[Any](len)
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(ps)
          }

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            val ps = new Array[Any](len)
            if (Lexer.firstField(trace, in))
              while({
                val idx = Lexer.field(trace, in, matrix)
                if (idx >= 0) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) = if ((default eq null) || in.nextNonWhitespace() != 'n' && {
                      in.retract()
                      true
                    }) tcs(idx).unsafeDecode(spans(idx) :: trace, in)
                    else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') default()
                    else Lexer.error("expected 'null'", spans(idx) :: trace)
                  } else Lexer.error("duplicate", trace)
                } else if (no_extra) Lexer.error("invalid extra field", trace)
                else Lexer.skipValue(trace, in)
                Lexer.nextField(trace, in)
              }) ()
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(ps)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                val ps = new Array[Any](len)
                o.fields.foreach { kv =>
                  namesMap.get(kv._1) match {
                    case Some(idx) =>
                      if (ps(idx) == null) {
                        val default = defaults(idx)
                        ps(idx) =
                          if ((default ne null) && (kv._2 eq Json.Null)) default()
                          else tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, kv._2)
                      } else Lexer.error("duplicate", trace)
                    case _ =>
                      if (no_extra) Lexer.error("invalid extra field", trace)
                  }
                }
                var idx = 0
                while (idx < len) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) =
                      if (default ne null) default()
                      else missingValueDecoder(idx, trace)
                  }
                  idx += 1
                }
                ctx.rawConstruct(ps)
              case _ => Lexer.error("expected object", trace)
            }
        }
      } else {
        val (names1, names2) = names.splitAt(splitIndex)
        val aliases1 = aliases.filter(kv => kv._2 <= splitIndex)
        val aliases2 = aliases.collect { case (k, v) if v > splitIndex =>
          (k, v - splitIndex)
        }
        new CollectionJsonDecoder[A] {
          private val len = names.length
          private val matrix1 = new StringMatrix(names1, aliases1)
          private val matrix2 = new StringMatrix(names2, aliases2)
          private val spans = names.map(JsonError.ObjectAccess(_))
          private val defaults = IArray.genericWrapArray(ctx.params.map(_.evaluateDefault.orNull)).toArray
          private lazy val tcs =
            IArray.genericWrapArray(ctx.params.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
          private lazy val namesMap = (names.zipWithIndex ++ aliases).toMap
          private val explicitEmptyCollections =
            ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
              a.decoding
            }.getOrElse(config.explicitEmptyCollections.decoding)
          private val missingValueDecoder =
            if (explicitEmptyCollections) {
              lazy val missingValueDecoders = tcs.map { d =>
                if (allowMissingValueDecoder(d)) d
                else null
              }
              (idx: Int, trace: List[JsonError]) => {
                val trace_ = spans(idx) :: trace
                val decoder = missingValueDecoders(idx)
                if (decoder eq null) Lexer.error("missing", trace_)
                decoder.unsafeDecodeMissing(trace_)
              }
            } else {
              (idx: Int, trace: List[JsonError]) => tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
            }

          @tailrec
          private def allowMissingValueDecoder(d: JsonDecoder[_]): Boolean = d match {
            case _: OptionJsonDecoder[_]     => true
            case _: CollectionJsonDecoder[_] => !explicitEmptyCollections
            case d: MappedJsonDecoder[_] => allowMissingValueDecoder(d.underlying)
            case _                       => true
          }

          override def unsafeDecodeMissing(trace: List[JsonError]): A = {
            val ps  = new Array[Any](len)
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(ps)
          }

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            val ps = new Array[Any](len)
            if (Lexer.firstField(trace, in))
              while({
                val idx = Lexer.field128(trace, in, matrix1, matrix2)
                if (idx >= 0) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) = if ((default eq null) || in.nextNonWhitespace() != 'n' && {
                      in.retract()
                      true
                    }) tcs(idx).unsafeDecode(spans(idx) :: trace, in)
                    else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') default()
                    else Lexer.error("expected 'null'", spans(idx) :: trace)
                  } else Lexer.error("duplicate", trace)
                } else if (no_extra) Lexer.error("invalid extra field", trace)
                else Lexer.skipValue(trace, in)
                Lexer.nextField(trace, in)
              }) ()
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(ps)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                val ps = new Array[Any](len)
                o.fields.foreach { kv =>
                  namesMap.get(kv._1) match {
                    case Some(idx) =>
                      if (ps(idx) == null) {
                        val default = defaults(idx)
                        ps(idx) =
                          if ((default ne null) && (kv._2 eq Json.Null)) default()
                          else tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, kv._2)
                      } else Lexer.error("duplicate", trace)
                    case _ =>
                      if (no_extra) Lexer.error("invalid extra field", trace)
                  }
                }
                var idx = 0
                while (idx < len) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) =
                      if (default ne null) default()
                      else missingValueDecoder(idx, trace)
                  }
                  idx += 1
                }
                ctx.rawConstruct(ps)
              case _ => Lexer.error("expected object", trace)
            }
        }
      }
    }
  }

  def split[A](ctx: SealedTrait[JsonDecoder, A]): JsonDecoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names: Array[String] = IArray.genericWrapArray(ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeInfo.short))
    }).toArray
    if (names.distinct.length != names.length) {
      val collisions = names.groupBy(identity).collect { case (n, ns) if ns.lengthCompare(1) > 0 => n }
      throw new AssertionError(s"Case names in ADT ${ctx.typeInfo.full} must be distinct, " +
        s"name(s) ${collisions.mkString(",")} are duplicated")
    }
    val (names1, names2) = names.splitAt(64)
    val matrix1 = new StringMatrix(names1)
    val matrix2 =
      if (names2.isEmpty) null
      else new StringMatrix(names2)
    lazy val tcs: Array[JsonDecoder[Any]] =
      IArray.genericWrapArray(ctx.subtypes.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    lazy val namesMap: Map[String, Int] = names.zipWithIndex.toMap
    val discrim =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)
    lazy val isEnumeration = config.enumValuesAsStrings &&
      (ctx.isEnum && ctx.subtypes.forall(_.typeclass.isInstanceOf[CaseObjectDecoder[?, ?]]) ||
        !ctx.isEnum && ctx.subtypes.forall(_.isObject))
    if (discrim.isEmpty && isEnumeration) {
      if (names.length <= 64) {
        new JsonDecoder[A] {
          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val idx = Lexer.enumeration(trace, in, matrix1)
            if (idx >= 0) tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
            else Lexer.error("invalid enumeration value", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case s: Json.Str => namesMap.get(s.value) match {
                case Some(idx) => tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
                case _         => Lexer.error("invalid enumeration value", trace)
              }
              case _ => Lexer.error("expected string", trace)
            }
        }
      } else {
        new JsonDecoder[A] {
          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val idx = Lexer.enumeration128(trace, in, matrix1, matrix2)
            if (idx >= 0) tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
            else Lexer.error("invalid enumeration value", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case s: Json.Str => namesMap.get(s.value) match {
                case Some(idx) => tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
                case _         => Lexer.error("invalid enumeration value", trace)
              }
              case _ => Lexer.error("expected string", trace)
            }
        }
      }
    } else if (discrim.isEmpty) {
      // We're not allowing extra fields in this encoding
      if (names.length <= 64) {
        new JsonDecoder[A] {
          private val spans = names.map(JsonError.ObjectAccess(_))

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            if (Lexer.firstField(trace, in)) {
              val idx = Lexer.field(trace, in, matrix1)
              if (idx >= 0) {
                val a = tcs(idx).unsafeDecode(spans(idx) :: trace, in).asInstanceOf[A]
                Lexer.char(trace, in, '}')
                a
              } else Lexer.error("invalid disambiguator", trace)
            } else Lexer.error("expected non-empty object", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj if o.fields.length == 1 =>
                val keyValue = o.fields(0)
                namesMap.get(keyValue._1) match {
                  case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, keyValue._2).asInstanceOf[A]
                  case _         => Lexer.error("invalid disambiguator", trace)
                }
              case _ => Lexer.error("expected single field object", trace)
            }
        }
      } else {
        new JsonDecoder[A] {
          private val spans = names.map(JsonError.ObjectAccess(_))

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            if (Lexer.firstField(trace, in)) {
              val idx = Lexer.field128(trace, in, matrix1, matrix2)
              if (idx >= 0) {
                val a = tcs(idx).unsafeDecode(spans(idx) :: trace, in).asInstanceOf[A]
                Lexer.char(trace, in, '}')
                a
              } else Lexer.error("invalid disambiguator", trace)
            } else Lexer.error("expected non-empty object", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj if o.fields.length == 1 =>
                val keyValue = o.fields(0)
                namesMap.get(keyValue._1) match {
                  case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, keyValue._2).asInstanceOf[A]
                  case _         => Lexer.error("invalid disambiguator", trace)
                }
              case _ => Lexer.error("expected single field object", trace)
            }
        }
      }
    } else {
      if (names.length <= 64) {
        new JsonDecoder[A] {
          private val hintfield = discrim.get
          private val hintmatrix = new StringMatrix(Array(hintfield))
          private val spans = names.map(JsonError.Message(_))

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val in_ = RecordingReader(in)
            Lexer.char(trace, in_, '{')
            if (Lexer.firstField(trace, in_)) {
              while ({
                if (Lexer.field(trace, in_, hintmatrix) >= 0) {
                  val idx = Lexer.enumeration(trace, in_, matrix1)
                  if (idx >= 0) {
                    in_.rewind()
                    return tcs(idx).unsafeDecode(spans(idx) :: trace, in_).asInstanceOf[A]
                  } else Lexer.error("invalid disambiguator", trace)
                } else Lexer.skipValue(trace, in_)
                Lexer.nextField(trace, in_)
              }) ()
            }
            Lexer.error(s"missing hint '$hintfield'", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                o.fields.collectFirst { case kv if kv._1 == hintfield && kv._2.isInstanceOf[Json.Str] =>
                  kv._2.asInstanceOf[Json.Str].value
                } match {
                  case Some(name) =>
                    namesMap.get(name) match {
                      case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, json).asInstanceOf[A]
                      case _         => Lexer.error("invalid disambiguator", trace)
                    }
                  case _ => Lexer.error(s"missing hint '$hintfield'", trace)
                }
              case _ => Lexer.error("expected object", trace)
            }
        }
      } else {
        new JsonDecoder[A] {
          private val hintfield = discrim.get
          private val hintmatrix = new StringMatrix(Array(hintfield))
          private val spans = names.map(JsonError.Message(_))

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val in_ = RecordingReader(in)
            Lexer.char(trace, in_, '{')
            if (Lexer.firstField(trace, in_)) {
              while ({
                if (Lexer.field(trace, in_, hintmatrix) >= 0) {
                  val idx = Lexer.enumeration128(trace, in_, matrix1, matrix2)
                  if (idx >= 0) {
                    in_.rewind()
                    return tcs(idx).unsafeDecode(spans(idx) :: trace, in_).asInstanceOf[A]
                  } else Lexer.error("invalid disambiguator", trace)
                } else Lexer.skipValue(trace, in_)
                Lexer.nextField(trace, in_)
              }) ()
            }
            Lexer.error(s"missing hint '$hintfield'", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                o.fields.collectFirst { case kv if kv._1 == hintfield && kv._2.isInstanceOf[Json.Str] =>
                  kv._2.asInstanceOf[Json.Str].value
                } match {
                  case Some(name) =>
                    namesMap.get(name) match {
                      case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, json).asInstanceOf[A]
                      case _         => Lexer.error("invalid disambiguator", trace)
                    }
                  case _ => Lexer.error(s"missing hint '$hintfield'", trace)
                }
              case _ => Lexer.error("expected object", trace)
            }
        }
      }
    }
  }

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

  // FIXME: remove in the next major version
  private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
    def apply(i: Int): Any = p(i)
    def length: Int        = p.length
  }
}

private lazy val caseObjectEncoder = new JsonEncoder[Any] {
  override def isEmpty(a: Any): Boolean = true

  def unsafeEncode(a: Any, indent: Option[Int], out: Write): Unit = out.write("{}")

  override final def toJsonAST(a: Any): Either[String, Json] = new Right(Json.Obj.empty)
}

object DeriveJsonDecoder extends JsonDecoderDerivation(JsonCodecConfiguration.default) { self =>
  inline def gen[A](using config: JsonCodecConfiguration, mirror: Mirror.Of[A]) = {
    val derivation = new JsonDecoderDerivation(config)
    derivation.derived[A]
  }

  // FIXME: remove in the next major version
  private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
    def apply(i: Int): Any = p(i)
    def length: Int        = p.length
  }
}

sealed class JsonEncoderDerivation(config: JsonCodecConfiguration) extends Derivation[JsonEncoder] { self =>
  def join[A](ctx: CaseClass[Typeclass, A]): JsonEncoder[A] =
    if (ctx.params.isEmpty) caseObjectEncoder.narrow[A]
    else {
      val nameTransform =
        ctx.annotations.collectFirst { case jsonMemberNames(format) => format }.getOrElse(config.fieldNameMapping)
      val explicitNulls = config.explicitNulls || ctx.annotations.exists(_.isInstanceOf[jsonExplicitNull])
      val explicitEmptyCollections = ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
        a.encoding
      }.getOrElse(config.explicitEmptyCollections.encoding)
      val params = IArray.genericWrapArray(ctx.params.filterNot { param =>
        param.annotations.collectFirst { case _: jsonExclude => () }.isDefined
      }).toArray
      new JsonEncoder[A] {
        private lazy val fields = params.map { p =>
          FieldEncoder(
            p = p,
            name = p.annotations.collectFirst { case jsonField(name) => name }.getOrElse(nameTransform(p.label)),
            encoder = p.typeclass.asInstanceOf[JsonEncoder[Any]],
            withExplicitNulls = explicitNulls || p.annotations.exists(_.isInstanceOf[jsonExplicitNull]),
            withExplicitEmptyCollections = p.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
              a.encoding
            }.getOrElse(explicitEmptyCollections)
          )
        }

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          val fields = this.fields
          var idx = 0
          var comma = false
          while (idx < fields.length) {
            val field = fields(idx)
            idx += 1
            val p = field.p.deref(a)
            if (field.skip(p)) ()
            else {
              if (comma) out.write(',')
              else comma = true
              JsonEncoder.pad(indent_, out)
              out.write(if (indent eq None) field.encodedName else field.prettyEncodedName)
              field.encoder.unsafeEncode(p, indent_, out)
            }
          }
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          val fields = this.fields
          var buf = new Array[(String, Json)](fields.length)
          var i, idx = 0
          while (idx < fields.length) {
            val field = fields(idx)
            idx += 1
            val p = field.p.deref(a)
            if (field.skip(p)) ()
            else {
              field.encoder.toJsonAST(p) match {
                case Right(value) =>
                  buf(i) = (field.name, value)
                  i += 1
                case left =>
                  return left
              }
            }
          }
          if (i != buf.length) buf = java.util.Arrays.copyOf(buf, i)
          new Right(Json.Obj(Chunk.fromArray(buf)))
        }
      }
    }

  def split[A](ctx: SealedTrait[JsonEncoder, A]): JsonEncoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names: Array[String] = IArray.genericWrapArray(ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeInfo.short))
    }).toArray
    val encodedNames: Array[String] = names.map(name => JsonEncoder.string.encodeJson(name, None).toString)
    lazy val tcs =
      IArray.genericWrapArray(ctx.subtypes.map(_.typeclass)).toArray.asInstanceOf[Array[JsonEncoder[Any]]]
    val discrim =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)
    lazy val isEnumeration = config.enumValuesAsStrings &&
      (ctx.isEnum && ctx.subtypes.forall(_.typeclass == caseObjectEncoder) ||
        !ctx.isEnum && ctx.subtypes.forall(_.isObject))
    if (discrim.isEmpty && isEnumeration) {
      new JsonEncoder[A] {
        private val casts = IArray.genericWrapArray(ctx.subtypes.map(_.cast)).toArray

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          out.write(encodedNames(idx))
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          new Right(new Json.Str(names(idx)))
        }
      }
    } else if (discrim.isEmpty) {
      new JsonEncoder[A] {
        private val casts = IArray.genericWrapArray(ctx.subtypes.map(_.cast)).toArray

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          out.write(encodedNames(idx))
          if (indent eq None) out.write(':')
          else out.write(" : ")
          tcs(idx).unsafeEncode(a, indent_, out)
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override def toJsonAST(a: A): Either[String, Json] = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          tcs(idx).toJsonAST(a).map(inner => new Json.Obj(Chunk(names(idx) -> inner)))
        }
      }
    } else {
      new JsonEncoder[A] {
        private val casts = IArray.genericWrapArray(ctx.subtypes.map(_.cast)).toArray
        private val hintFieldName = discrim.get
        private val encodedHintFieldName = JsonEncoder.string.encodeJson(hintFieldName, None).toString

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          out.write(encodedHintFieldName)
          if (indent eq None) out.write(':')
          else out.write(" : ")
          out.write(encodedNames(idx))
          // whitespace is always off by 2 spaces at the end, probably not worth fixing
          tcs(idx).unsafeEncode(a, indent, new DeriveJsonEncoder.NestedWriter(out, indent_))
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          tcs(idx).toJsonAST(a).flatMap {
            case o: Json.Obj =>
              val hintField = hintFieldName -> new Json.Str(names(idx))
              new Right(new Json.Obj(hintField +: o.fields)) // hint field is always first
            case _ =>
              new Left("expected object")
          }
        }
      }
    }
  }
}

object DeriveJsonEncoder extends JsonEncoderDerivation(JsonCodecConfiguration.default) { self =>
  inline def gen[A](using config: JsonCodecConfiguration, mirror: Mirror.Of[A]) = {
    val derivation = new JsonEncoderDerivation(config)
    derivation.derived[A]
  }

  // intercepts the first `{` of a nested writer and discards it. We also need to
  // inject a `,` unless an empty object `{}` has been written.
  private[json] final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
    private var state = 2

    @inline def write(c: Char): Unit =
      if (state == 0) out.write(c)
      else nonZeroStateWrite(c)

    @noinline private def nonZeroStateWrite(c: Char): Unit = {
      if (c != ' ' && c != '\n') {
        if (state == 2) {
          if (c == '{') state = 1
        } else {
          state = 0
          if (c != '}') {
            out.write(',')
            JsonEncoder.pad(indent, out)
          }
          out.write(c)
        }
      }
    }

    @inline def write(s: String): Unit =
      if (state == 0) out.write(s)
      else nonZeroStateWrite(s)

    @noinline private def nonZeroStateWrite(s: String): Unit = {
      var i = 0
      while (i < s.length) {
        val c = s.charAt(i)
        if (c != ' ' && c != '\n') {
          if (state == 2) {
            if (c == '{') state = 1
          } else {
            state = 0
            if (c != '}') {
              out.write(',')
              JsonEncoder.pad(indent, out)
            }
            while (i < s.length) {
              out.write(s.charAt(i))
              i += 1
            }
            return
          }
        }
        i += 1
      }
    }

    @inline override def write(cs: Array[Char], from: Int, to: Int): Unit =
      if (state == 0) out.write(cs, from, to)
      else nonZeroStateWrite(cs, from, to)

    @noinline def nonZeroStateWrite(cs: Array[Char], from: Int, to: Int): Unit = {
      var i = from
      while (i < to) {
        val c = cs(i)
        if (c != ' ' && c != '\n') {
          if (state == 2) {
            if (c == '{') state = 1
          } else {
            state = 0
            if (c != '}') {
              out.write(',')
              JsonEncoder.pad(indent, out)
            }
            out.write(cs, i, to)
            return
          }
        }
        i += 1
      }
    }

    @inline override def write(c1: Char, c2: Char): Unit =
      if (state == 0) out.write(c1, c2)
      else {
        nonZeroStateWrite(c1)
        nonZeroStateWrite(c2)
      }

    @inline override def write(c1: Char, c2: Char, c3: Char): Unit =
      if (state == 0) out.write(c1, c2, c3)
      else {
        nonZeroStateWrite(c1)
        nonZeroStateWrite(c2)
        nonZeroStateWrite(c3)
      }

    @inline override def write(c1: Char, c2: Char, c3: Char, c4: Char): Unit =
      if (state == 0) out.write(c1, c2, c3, c4)
      else {
        nonZeroStateWrite(c1)
        nonZeroStateWrite(c2)
        nonZeroStateWrite(c3)
        nonZeroStateWrite(c4)
      }

    @inline override def write(c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): Unit =
      if (state == 0) out.write(c1, c2, c3, c4, c5)
      else {
        nonZeroStateWrite(c1)
        nonZeroStateWrite(c2)
        nonZeroStateWrite(c3)
        nonZeroStateWrite(c4)
        nonZeroStateWrite(c5)
      }

    @inline override def write(s: Short): Unit =
      if (state == 0) out.write(s)
      else {
        nonZeroStateWrite((s & 0xff).toChar)
        nonZeroStateWrite((s >> 8).toChar)
      }

    @inline override def write(s1: Short, s2: Short): Unit =
      if (state == 0) out.write(s1, s2)
      else {
        nonZeroStateWrite((s1 & 0xff).toChar)
        nonZeroStateWrite((s1 >> 8).toChar)
        nonZeroStateWrite((s2 & 0xff).toChar)
        nonZeroStateWrite((s2 >> 8).toChar)
      }

    @inline override def write(s1: Short, s2: Short, s3: Short): Unit =
      if (state == 0) out.write(s1, s2, s3)
      else {
        nonZeroStateWrite((s1 & 0xff).toChar)
        nonZeroStateWrite((s1 >> 8).toChar)
        nonZeroStateWrite((s2 & 0xff).toChar)
        nonZeroStateWrite((s2 >> 8).toChar)
        nonZeroStateWrite((s3 & 0xff).toChar)
        nonZeroStateWrite((s3 >> 8).toChar)
      }

    @inline override def write(s1: Short, s2: Short, s3: Short, s4: Short): Unit =
      if (state == 0) out.write(s1, s2, s3, s4)
      else {
        nonZeroStateWrite((s1 & 0xff).toChar)
        nonZeroStateWrite((s1 >> 8).toChar)
        nonZeroStateWrite((s2 & 0xff).toChar)
        nonZeroStateWrite((s2 >> 8).toChar)
        nonZeroStateWrite((s3 & 0xff).toChar)
        nonZeroStateWrite((s3 >> 8).toChar)
        nonZeroStateWrite((s4 & 0xff).toChar)
        nonZeroStateWrite((s4 >> 8).toChar)
      }
  }
}

object DeriveJsonCodec {
  inline def gen[A](using mirror: Mirror.Of[A], config: JsonCodecConfiguration) = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]

    JsonCodec(encoder, decoder)
  }
}
