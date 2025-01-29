package zio.json

import magnolia1._
import zio.Chunk
import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.{ FieldEncoder, Lexer, RetractReader, StringMatrix, Write }

import scala.annotation._
import scala.language.experimental.macros

/**
 * If used on a case class field, determines the name of the JSON field. Defaults to the case class field name.
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
 * If used on a sealed class, will determine the name of the field for disambiguating classes.
 *
 * The default is to not use a typehint field and instead have an object with a single key that is the class name.
 *
 * Note that using a discriminator is less performant, uses more memory, and may be prone to DOS attacks that are
 * impossible with the default encoding. In addition, there is slightly less type safety when using custom product
 * encoders (which must write an unenforced object type). Only use this option if you must model an externally defined
 * schema.
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
 * If used on a case class, determines the strategy of member names transformation during serialization and
 * deserialization. Four common strategies are provided above and a custom one to support specific use cases.
 */
final case class jsonMemberNames(format: JsonMemberFormat) extends Annotation
private[json] object jsonMemberNames {

  /**
   * ~~Stolen~~ Borrowed from jsoniter-scala by Andriy Plokhotnyuk (he even granted permission for this, imagine that!)
   */

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
 * If used on a case class will determine the type hint value for disambiguating sealed traits. Defaults to the short
 * type name.
 */
final case class jsonHint(name: String) extends Annotation

/**
 * If used on a sealed class will determine the strategy of type hint value transformation for disambiguating classes
 * during serialization and deserialization. Same strategies are provided as for [[jsonMemberNames]].
 */
final case class jsonHintNames(format: JsonMemberFormat) extends Annotation

/**
 * If used on a case class, will exit early if any fields are in the JSON that do not correspond to field names in the
 * case class.
 *
 * This adds extra protections against a DOS attacks but means that changes in the schema will result in a hard error
 * rather than silently ignoring those fields.
 *
 * Cannot be combined with `@jsonDiscriminator` since it is considered an extra field from the perspective of the case
 * class.
 */
final class jsonNoExtraFields extends Annotation

/**
 * If used on a case class field, will exclude it from the resulting JSON.
 */
final class jsonExclude extends Annotation

object DeriveJsonDecoder {
  type Typeclass[A] = JsonDecoder[A]

  def join[A](ctx: CaseClass[JsonDecoder, A])(implicit config: JsonCodecConfiguration): JsonDecoder[A] = {
    val (transformNames, nameTransform): (Boolean, String => String) =
      ctx.annotations.collectFirst { case jsonMemberNames(format) => format }
        .orElse(Some(config.fieldNameMapping))
        .filter(_ != IdentityFormat)
        .map(true -> _)
        .getOrElse(false -> identity _)

    val no_extra = ctx.annotations.collectFirst { case _: jsonNoExtraFields =>
      ()
    }.isDefined || !config.allowExtraFields

    if (ctx.parameters.isEmpty)
      new CollectionJsonDecoder[A] {
        override def unsafeDecodeMissing(trace: List[JsonError]): A = ctx.rawConstruct(Nil)

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          if (no_extra) {
            Lexer.char(trace, in, '{')
            Lexer.char(trace, in, '}')
          } else {
            Lexer.skipValue(trace, in)
          }
          ctx.rawConstruct(Nil)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(_) => ctx.rawConstruct(Nil)
            case Json.Null   => ctx.rawConstruct(Nil)
            case _           => Lexer.error("Not an object", trace)
          }
      }
    else
      new CollectionJsonDecoder[A] {
        private[this] val (names, aliases): (Array[String], Array[(String, Int)]) = {
          val names          = new Array[String](ctx.parameters.size)
          val aliasesBuilder = Array.newBuilder[(String, Int)]
          ctx.parameters.foreach {
            var idx = 0
            p =>
              names(idx) = p.annotations.collectFirst { case jsonField(name) => name }
                .getOrElse(if (transformNames) nameTransform(p.label) else p.label)
              aliasesBuilder ++= p.annotations.flatMap {
                case jsonAliases(alias, aliases @ _*) => (alias +: aliases).map(_ -> idx)
                case _                                => Seq.empty
              }
              idx += 1
          }
          val aliases       = aliasesBuilder.result()
          val allFieldNames = names ++ aliases.map(_._1)
          if (allFieldNames.length != allFieldNames.distinct.length) {
            val aliasNames = aliases.map(_._1)
            val collisions = aliasNames
              .filter(alias => names.contains(alias) || aliases.count { case (a, _) => a == alias } > 1)
              .distinct
            val msg = s"Field names and aliases in case class ${ctx.typeName.full} must be distinct, " +
              s"alias(es) ${collisions.mkString(",")} collide with a field or another alias"
            throw new AssertionError(msg)
          }
          (names, aliases)
        }
        private[this] val len      = names.length
        private[this] val matrix   = new StringMatrix(names, aliases)
        private[this] val spans    = names.map(JsonError.ObjectAccess)
        private[this] val defaults = ctx.parameters.map(_.evaluateDefault.orNull).toArray
        private[this] lazy val tcs =
          ctx.parameters.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
        private[this] lazy val namesMap = (names.zipWithIndex ++ aliases).toMap

        private[this] val explicitEmptyCollections =
          ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
            a.decoding
          }.getOrElse(config.explicitEmptyCollections.decoding)

        private[this] val missingValueDecoder =
          if (explicitEmptyCollections) {
            lazy val missingValueDecoders = tcs.map { d =>
              if (allowMissingValueDecoder(d)) d
              else null
            }
            (idx: Int, trace: List[JsonError]) => {
              val trace_  = spans(idx) :: trace
              val decoder = missingValueDecoders(idx)
              if (decoder eq null) Lexer.error("missing", trace_)
              decoder.unsafeDecodeMissing(trace_)
            }
          } else { (idx: Int, trace: List[JsonError]) =>
            tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
          }

        @tailrec
        private[this] def allowMissingValueDecoder(d: JsonDecoder[_]): Boolean = d match {
          case _: OptionJsonDecoder[_]     => true
          case _: CollectionJsonDecoder[_] => !explicitEmptyCollections
          case d: MappedJsonDecoder[_]     => allowMissingValueDecoder(d.underlying)
          case _                           => false
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
          ctx.rawConstruct(new ArraySeq(ps))
        }

        override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')

          // TODO it would be more efficient to have a solution that didn't box
          // primitives, but Magnolia does not expose an API for that. Adding
          // such a feature to Magnolia is the only way to avoid this, e.g. a
          // ctx.createMutableCons that specialises on the types (with some way
          // of noting that things have been initialised), which can be called
          // to instantiate the case class. Would also require JsonDecoder to be
          // specialised.
          val ps = new Array[Any](len)
          if (Lexer.firstField(trace, in))
            do {
              val idx = Lexer.field(trace, in, matrix)
              if (idx != -1) {
                if (ps(idx) != null) Lexer.error("duplicate", trace)
                val default = defaults(idx)
                ps(idx) =
                  if (
                    (default eq null) || in.nextNonWhitespace() != 'n' && {
                      in.retract()
                      true
                    }
                  ) tcs(idx).unsafeDecode(spans(idx) :: trace, in)
                  else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') default()
                  else Lexer.error("expected 'null'", spans(idx) :: trace)
              } else if (no_extra) Lexer.error("invalid extra field", trace)
              else Lexer.skipValue(trace, in)
            } while (Lexer.nextField(trace, in))
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

          ctx.rawConstruct(new ArraySeq(ps))
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(keyValues) =>
              val ps = new Array[Any](len)
              for ((key, value) <- keyValues) {
                namesMap.get(key) match {
                  case Some(idx) =>
                    if (ps(idx) != null) Lexer.error("duplicate", trace)
                    val default = defaults(idx)
                    ps(idx) =
                      if ((default ne null) && (value eq Json.Null)) default()
                      else tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, value)
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
              ctx.rawConstruct(new ArraySeq(ps))
            case _ => Lexer.error("Not an object", trace)
          }
      }
  }

  def split[A](ctx: SealedTrait[JsonDecoder, A])(implicit config: JsonCodecConfiguration): JsonDecoder[A] = {
    val jsonHintFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names = ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeName.short))
    }.toArray
    if (names.distinct.length != names.length) {
      val collisions = names.groupBy(identity).collect { case (n, ns) if ns.lengthCompare(1) > 0 => n }
      throw new AssertionError(
        s"Case names in ADT ${ctx.typeName.full} must be distinct, " +
          s"name(s) ${collisions.mkString(",")} are duplicated"
      )
    }
    val matrix        = new StringMatrix(names)
    lazy val tcs      = ctx.subtypes.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    lazy val namesMap = names.zipWithIndex.toMap

    def discrim =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)

    if (discrim.isEmpty)
      new JsonDecoder[A] {
        private[this] val spans = names.map(JsonError.ObjectAccess)

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          // we're not allowing extra fields in this encoding
          if (Lexer.firstField(trace, in)) {
            val idx = Lexer.field(trace, in, matrix)
            if (idx != -1) {
              val a = tcs(idx).unsafeDecode(spans(idx) :: trace, in).asInstanceOf[A]
              Lexer.char(trace, in, '}')
              a
            } else Lexer.error("invalid disambiguator", trace)
          } else Lexer.error("expected non-empty object", trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(chunk) if chunk.size == 1 =>
              val keyValue = chunk.head
              namesMap.get(keyValue._1) match {
                case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, keyValue._2).asInstanceOf[A]
                case _         => Lexer.error("Invalid disambiguator", trace)
              }
            case Json.Obj(_) => Lexer.error("Not an object with a single field", trace)
            case _           => Lexer.error("Not an object", trace)
          }
      }
    else
      new JsonDecoder[A] {
        private[this] val hintfield  = discrim.get
        private[this] val hintmatrix = new StringMatrix(Array(hintfield))
        private[this] val spans      = names.map(JsonError.Message)

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val in_ = internal.RecordingReader(in)
          Lexer.char(trace, in_, '{')
          if (Lexer.firstField(trace, in_))
            do {
              if (Lexer.field(trace, in_, hintmatrix) != -1) {
                val idx = Lexer.enumeration(trace, in_, matrix)
                if (idx != -1) {
                  in_.rewind()
                  return tcs(idx).unsafeDecode(spans(idx) :: trace, in_).asInstanceOf[A]
                } else Lexer.error("invalid disambiguator", trace)
              } else Lexer.skipValue(trace, in_)
            } while (Lexer.nextField(trace, in_))
          Lexer.error(s"missing hint '$hintfield'", trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(fields) =>
              fields.find { case (key, _) => key == hintfield } match {
                case Some((_, Json.Str(name))) =>
                  namesMap.get(name) match {
                    case Some(idx) => tcs(idx).unsafeFromJsonAST(trace, json).asInstanceOf[A]
                    case _         => Lexer.error("Invalid disambiguator", trace)
                  }
                case Some(_) => Lexer.error(s"Non-string hint '$hintfield'", trace)
                case _       => Lexer.error(s"Missing hint '$hintfield'", trace)
              }
            case _ => Lexer.error("Not an object", trace)
          }
      }
  }

  def gen[A]: JsonDecoder[A] = macro Magnolia.gen[A]
}

object DeriveJsonEncoder {
  type Typeclass[A] = JsonEncoder[A]

  def join[A](ctx: CaseClass[JsonEncoder, A])(implicit config: JsonCodecConfiguration): JsonEncoder[A] =
    if (ctx.parameters.isEmpty)
      new JsonEncoder[A] {

        override def isEmpty(a: A): Boolean = true

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write("{}")

        override final def toJsonAST(a: A): Either[String, Json] =
          Right(Json.Obj(Chunk.empty))
      }
    else
      new JsonEncoder[A] {
        private[this] val (transformNames, nameTransform): (Boolean, String => String) =
          ctx.annotations.collectFirst { case jsonMemberNames(format) => format }
            .orElse(Some(config.fieldNameMapping))
            .filter(_ != IdentityFormat)
            .map(true -> _)
            .getOrElse(false -> identity)
        private[this] val params = ctx.parameters
          .filter(p => p.annotations.collectFirst { case _: jsonExclude => () }.isEmpty)
          .toArray

        private[this] val explicitNulls =
          config.explicitNulls || ctx.annotations.exists(_.isInstanceOf[jsonExplicitNull])
        private[this] val explicitEmptyCollections =
          ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
            a.encoding
          }.getOrElse(config.explicitEmptyCollections.encoding)

        private[this] lazy val fields: Array[FieldEncoder[Any, Param[JsonEncoder, A]]] = params.map { p =>
          val name = p.annotations.collectFirst { case jsonField(name) =>
            name
          }.getOrElse(if (transformNames) nameTransform(p.label) else p.label)
          val withExplicitNulls = explicitNulls || p.annotations.exists(_.isInstanceOf[jsonExplicitNull])
          val withExplicitEmptyCollections = p.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
            a.encoding
          }.getOrElse(explicitEmptyCollections)
          new FieldEncoder(
            p,
            name,
            p.typeclass.asInstanceOf[JsonEncoder[Any]],
            withExplicitNulls = withExplicitNulls,
            withExplicitEmptyCollections = withExplicitEmptyCollections
          )
        }

        override def isEmpty(a: A): Boolean = fields.forall { field =>
          val paramValue = field.p.dereference(a)
          field.encoder.isEmpty(paramValue) || field.encoder.isNothing(paramValue)
        }

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          val fields     = this.fields
          var idx        = 0
          var prevFields = false // whether any fields have been written
          while (idx < fields.length) {
            val field   = fields(idx)
            val p       = field.p.dereference(a)
            val encoder = field.encoder
            if ({
              (field.flags: @switch) match {
                case 0 => !encoder.isEmpty(p) && !encoder.isNothing(p)
                case 1 => !encoder.isNothing(p)
                case 2 => !encoder.isEmpty(p)
                case _ => true
              }
            }) {
              // if we have at least one field already, we need a comma
              if (prevFields) {
                out.write(',')
                JsonEncoder.pad(indent_, out)
              }
              JsonEncoder.string.unsafeEncode(field.name, indent_, out)
              if (indent.isEmpty) out.write(':')
              else out.write(" : ")
              encoder.unsafeEncode(p, indent_, out)
              prevFields = true // record that we have at least one field so far
            }
            idx += 1
          }
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override final def toJsonAST(a: A): Either[String, Json] =
          fields
            .foldLeft[Either[String, Chunk[(String, Json)]]](Right(Chunk.empty)) { case (c, field) =>
              val param      = field.p
              val paramValue = field.p.dereference(a).asInstanceOf[param.PType]
              field.encodeOrDefault(paramValue)(
                () =>
                  c.flatMap { chunk =>
                    param.typeclass.toJsonAST(paramValue).map(value => chunk :+ field.name -> value)
                  },
                c
              )
            }
            .map(Json.Obj.apply)
      }

  def split[A](ctx: SealedTrait[JsonEncoder, A])(implicit config: JsonCodecConfiguration): JsonEncoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeName.short))
    }.toArray

    def discrim =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)

    if (discrim.isEmpty) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = ctx.split(a) { sub =>
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          JsonEncoder.string.unsafeEncode(names(sub.index), indent_, out)
          if (indent.isEmpty) out.write(':')
          else out.write(" : ")
          sub.typeclass.unsafeEncode(sub.cast(a), indent_, out)
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override def toJsonAST(a: A): Either[String, Json] =
          ctx.split(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).map { inner =>
              Json.Obj(
                Chunk(
                  names(sub.index) -> inner
                )
              )
            }
          }
      }
    } else {
      new JsonEncoder[A] {
        private[this] val hintfield = discrim.get

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = ctx.split(a) { sub =>
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          JsonEncoder.string.unsafeEncode(hintfield, indent_, out)
          if (indent.isEmpty) out.write(':')
          else out.write(" : ")
          JsonEncoder.string.unsafeEncode(names(sub.index), indent_, out)
          // whitespace is always off by 2 spaces at the end, probably not worth fixing
          val intermediate = new NestedWriter(out, indent_)
          sub.typeclass.unsafeEncode(sub.cast(a), indent, intermediate)
        }

        override def toJsonAST(a: A): Either[String, Json] =
          ctx.split(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).flatMap {
              case Json.Obj(fields) => Right(Json.Obj(fields :+ hintfield -> Json.Str(names(sub.index))))
              case _                => Left("Subtype is not encoded as an object")
            }
          }
      }
    }
  }

  def gen[A]: JsonEncoder[A] = macro Magnolia.gen[A]
}

// backcompat for 2.12, otherwise we'd use ArraySeq.unsafeWrapArray
private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
  def apply(i: Int): Any = p(i)
  def length: Int        = p.length
}

// intercepts the first `{` of a nested writer and discards it. We also need to
// inject a `,` unless an empty object `{}` has been written.
private[this] final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
  private[this] var state = 2

  def write(c: Char): Unit =
    if (state != 0) {
      if (c == ' ' || c == '\n') {
        ()
      } else if (state == 2 && c == '{') {
        state = 1
      } else if (state == 1) {
        state = 0
        if (c != '}') {
          out.write(',')
          JsonEncoder.pad(indent, out)
        }
        out.write(c)
      }
    } else out.write(c)

  def write(s: String): Unit =
    if (state != 0) {
      var i = 0
      while (i < s.length) {
        val c = s.charAt(i)
        if (c == ' ' || c == '\n') {
          ()
        } else if (state == 2 && c == '{') {
          state = 1
        } else if (state == 1) {
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
        i += 1
      }
    } else out.write(s)
}

object DeriveJsonCodec {
  def gen[A]: JsonCodec[A] = macro genBoth[A]

  import scala.reflect.macros._

  def genBoth[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val encoder = q"_root_.zio.json.DeriveJsonEncoder.gen[${c.weakTypeOf[T]}]"
    val decoder = q"_root_.zio.json.DeriveJsonDecoder.gen[${c.weakTypeOf[T]}]"

    q"_root_.zio.json.JsonCodec($encoder, $decoder)"
  }
}
