package zio.json

import zio.json.ast.Json
import scala.annotation.*
import magnolia1.*
import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import zio.Chunk

import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.ast.Json
import zio.json.internal.{ Lexer, RetractReader, StringMatrix, Write }

import scala.annotation._
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

/**
 * If used on a class extending a sealed class using `@jsonDiscriminator`, it will include the parent discriminator while encoding and decoding.
 */
final case class inheritDiscriminator() extends Annotation

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

  /**
   * ~~Stolen~~ Borrowed from jsoniter-scala by Andriy Plokhotnyuk
   * (he even granted permission for this, imagine that!)
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
    val len                   = s.length
    val sb                    = new StringBuilder(len << 1)
    var i                     = 0
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
    val len = s.length
    val sb = new StringBuilder(len << 1)
    var i = 0
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

// TODO: implement same configuration as for Scala 2 once this issue is resolved: https://github.com/softwaremill/magnolia/issues/296
object DeriveJsonDecoder extends Derivation[JsonDecoder] { self =>
  def join[A](ctx: CaseClass[Typeclass, A]): JsonDecoder[A] = {
    val (transformNames, nameTransform): (Boolean, String => String) =
      ctx.annotations.collectFirst { case jsonMemberNames(format) => format }
        .map(true -> _)
        .getOrElse(false -> identity)

    val no_extra = ctx
      .annotations
      .collectFirst { case _: jsonNoExtraFields => () }
      .isDefined

    val inheritedHint = ctx
      .annotations
      .collectFirst { case _: inheritDiscriminator => 
        ctx
          .inheritedAnnotations
          .collectFirst {
            case jsonDiscriminator(n) => n
          }.getOrElse(throw new Throwable("Not possible to use `inheritDiscriminator` annotation as there is no discriminator in parent class to inherit"))
        }


    val correctHint = inheritedHint
      .map { _ =>
        val jsonHintFormat: JsonMemberFormat =
          ctx.inheritedAnnotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(IdentityFormat)
        ctx
          .annotations.collectFirst { case jsonHint(name) =>
            name
          }.getOrElse(jsonHintFormat(ctx.typeInfo.short))
      }
    
    val numberOfParams = ctx.params.size + inheritedHint.map(_ => 1).getOrElse(0)

    if (numberOfParams == 0) {
      new JsonDecoder[A] {
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
            case _           => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
          }
      }
    } else {
      new JsonDecoder[A] {
        val (names, aliases): (Array[String], Array[(String, Int)]) = {
          val names = Array.ofDim[String](numberOfParams)
          val aliasesBuilder = Array.newBuilder[(String, Int)]
          ctx.params.zipWithIndex.foreach { (p, i) =>
            names(i) = p
              .annotations
              .collectFirst { case jsonField(name) => name }
              .getOrElse(if (transformNames) nameTransform(p.label) else p.label)
            aliasesBuilder ++= p
              .annotations
              .flatMap {
                case jsonAliases(alias, aliases*) => (alias +: aliases).map(_ -> i)
                case _ => Seq.empty
              }
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

          inheritedHint.map(hint => {names(names.length - 1) = hint; ()})
          (names, aliases)
        }

        val len:    Int              = names.length
        val matrix: StringMatrix     = new StringMatrix(names, aliases)
        val spans:  Array[JsonError] = names.map(JsonError.ObjectAccess(_))

        lazy val tcs: Array[JsonDecoder[Any]] =
          IArray.genericWrapArray(ctx.params.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]] ++ Array(JsonDecoder.string.asInstanceOf[JsonDecoder[Any]])

        lazy val defaults: Array[Option[Any]] =
          IArray.genericWrapArray(ctx.params.map(_.default)).toArray ++ Array(None)

        lazy val namesMap: Map[String, Int] =
          (names.zipWithIndex ++ aliases).toMap

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')

          val ps: Array[Any] = Array.ofDim(len)

          if (Lexer.firstField(trace, in))
            while({
              var trace_ = trace
              val field  = Lexer.field(trace, in, matrix)
              if (field != -1) {
                trace_ = spans(field) :: trace
                if (ps(field) != null)
                  throw UnsafeJson(JsonError.Message("duplicate") :: trace)
                if (defaults(field).isDefined) {
                  val opt = JsonDecoder.option(tcs(field)).unsafeDecode(trace_, in)
                  ps(field) = opt.getOrElse(defaults(field).get)
                } else
                  ps(field) = tcs(field).unsafeDecode(trace_, in)
              } else if (no_extra) {
                throw UnsafeJson(
                  JsonError.Message(s"invalid extra field") :: trace
                )
              } else
                Lexer.skipValue(trace_, in)

              Lexer.nextField(trace, in)
            }) ()

          var i = 0

          while (i < len) {
            if (ps(i) == null) {
              if (defaults(i).isDefined) {
                ps(i) = defaults(i).get
              } else {
                ps(i) = tcs(i).unsafeDecodeMissing(spans(i) :: trace)
              }
            }
            i += 1
          }

          val finalPs = if (inheritedHint.isEmpty) ps else {
            correctHint.map(hintValue => if (hintValue != ps.last) throw UnsafeJson(JsonError.Message(s"Hint should have been $hintValue") :: trace))
            ps.init
          }

          ctx.rawConstruct(new ArraySeq(finalPs))
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
          json match {
            case Json.Obj(fields) =>
              val ps: Array[Any] = Array.ofDim(len)

              if (aliases.nonEmpty) {
                val present = fields.map { case (key, _) => namesMap(key) }
                if (present.distinct.size != present.size) {
                  throw UnsafeJson(
                    JsonError.Message("duplicate") :: trace
                  )
                }
              }

              for ((key, value) <- fields) {
                namesMap.get(key) match {
                  case Some(field) =>
                    val trace_ = JsonError.ObjectAccess(key) :: trace
                    if (defaults(field).isDefined) {
                      val opt = JsonDecoder.option(tcs(field)).unsafeFromJsonAST(trace_, value)
                      ps(field) = opt.getOrElse(defaults(field).get)
                    } else {
                      ps(field) = tcs(field).unsafeFromJsonAST(trace_, value)
                    }
                  case None =>
                    inheritedHint match {
                      case Some(hint) if key == hint => 
                        value match {
                          case Json.Str(name) => correctHint.map(hintValue => if (hintValue != ps.last) throw UnsafeJson(JsonError.Message(s"Hint should have been $hintValue") :: trace))
                          case _ => throw UnsafeJson(JsonError.Message(s"Non-string hint '$hint'") :: trace)
                        }
                  
                      case _ =>
                        if (no_extra) {
                          throw UnsafeJson(
                            JsonError.Message(s"invalid extra field") :: trace
                          )
                        }
                    }
                    
                }
              }

              var i = 0
              while (i < len) {
                if (ps(i) == null) {
                  if (defaults(i).isDefined) {
                    ps(i) = defaults(i).get
                  } else {
                    ps(i) = tcs(i).unsafeDecodeMissing(JsonError.ObjectAccess(names(i)) :: trace)
                  }
                }
                i += 1
              }

              ctx.rawConstruct(new ArraySeq(ps))

            case _ => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
          }
        }
      }
    }
  }

  def split[A](ctx: SealedTrait[JsonDecoder, A]): JsonDecoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(IdentityFormat)
    val names: Array[String] = IArray.genericWrapArray(ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) =>
        name
      }.getOrElse(jsonHintFormat(p.typeInfo.short))
    }).toArray

    val matrix: StringMatrix = new StringMatrix(names)

    lazy val tcs: Array[JsonDecoder[Any]] =
      IArray.genericWrapArray(ctx.subtypes.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]

    lazy val namesMap: Map[String, Int] =
      names.zipWithIndex.toMap

    def discrim = ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }

    if (discrim.isEmpty) {
      // We're not allowing extra fields in this encoding
      new JsonDecoder[A] {
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')

          if (Lexer.firstField(trace, in)) {
            val field = Lexer.field(trace, in, matrix)

            if (field != -1) {
              val trace_ = spans(field) :: trace
              val a      = tcs(field).unsafeDecode(trace_, in).asInstanceOf[A]
              Lexer.char(trace, in, '}')
              a
            } else
              throw UnsafeJson(
                JsonError.Message("invalid disambiguator") :: trace
              )
          } else
            throw UnsafeJson(
              JsonError.Message("expected non-empty object") :: trace
            )
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
          json match {
            case Json.Obj(chunk) if chunk.size == 1 =>
              val (key, inner) = chunk.head
              namesMap.get(key) match {
                case Some(idx) => tcs(idx).unsafeFromJsonAST(JsonError.ObjectAccess(key) :: trace, inner).asInstanceOf[A]
                case None      => throw UnsafeJson(JsonError.Message("Invalid disambiguator") :: trace)
              }
            case Json.Obj(_) => throw UnsafeJson(JsonError.Message("Not an object with a single field") :: trace)
            case _           => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
          }
        }
      }
    } else {
      new JsonDecoder[A] {
        val hintfield               = discrim.get
        val hintmatrix              = new StringMatrix(Array(hintfield))
        val spans: Array[JsonError] = names.map(JsonError.Message(_))

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val in_ = zio.json.internal.RecordingReader(in)

          Lexer.char(trace, in_, '{')

          if (Lexer.firstField(trace, in_)) {
            while({
              if (Lexer.field(trace, in_, hintmatrix) != -1) {
                val field = Lexer.enumeration(trace, in_, matrix)

                if (field == -1) {
                  throw UnsafeJson(JsonError.Message(s"invalid disambiguator") :: trace)
                }

                in_.rewind()
                val trace_ = spans(field) :: trace

                return tcs(field).unsafeDecode(trace_, in_).asInstanceOf[A]
              } else {
                Lexer.skipValue(trace, in_)
              }

              Lexer.nextField(trace, in_)
            }) ()
          }

          throw UnsafeJson(JsonError.Message(s"missing hint '$hintfield'") :: trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
          json match {
            case Json.Obj(fields) =>
              fields.find { case (k, _) => k == hintfield } match {
                case Some((_, Json.Str(name))) =>
                  namesMap.get(name) match {
                    case Some(idx) => tcs(idx).unsafeFromJsonAST(JsonError.ObjectAccess(name) :: trace, json).asInstanceOf[A]
                    case None      => throw UnsafeJson(JsonError.Message("Invalid disambiguator") :: trace)
                  }
                case Some(_) =>
                  throw UnsafeJson(JsonError.Message(s"Non-string hint '$hintfield'") :: trace)
                case None =>
                  throw UnsafeJson(JsonError.Message(s"Missing hint '$hintfield'") :: trace)
              }
            case _ => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
          }
        }
      }
    }
  }

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

  // Backcompat for 2.12, otherwise we'd use ArraySeq.unsafeWrapArray
  private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
    def apply(i: Int): Any = p(i)
    def length: Int        = p.length
  }
}

object DeriveJsonEncoder extends Derivation[JsonEncoder] { self =>

  def join[A](ctx: CaseClass[Typeclass, A]): JsonEncoder[A] = {

    val inheritedHint = ctx
      .annotations
      .collectFirst { case _: inheritDiscriminator => 
        ctx
          .inheritedAnnotations
          .collectFirst {
            case jsonDiscriminator(n) => n
          }.getOrElse(throw new Throwable("Not possible to use `inheritDiscriminator` annotation as there is no discriminator in parent class to inherit"))
        }

    val correctHint = {
        val jsonHintFormat: JsonMemberFormat =
          ctx.inheritedAnnotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(IdentityFormat)
        ctx
          .annotations.collectFirst { case jsonHint(name) =>
            name
          }.getOrElse(jsonHintFormat(ctx.typeInfo.short))
      }
    
    val numberOfParams = ctx.params.size + inheritedHint.map(_ => 1).getOrElse(0)

    if (numberOfParams == 0) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
          out.write("{}")

        override final def toJsonAST(a: A): Either[String, Json] =
          Right(Json.Obj(Chunk.empty))
      }
    } else {
      new JsonEncoder[A] {
        val (transformNames, nameTransform): (Boolean, String => String) =
          ctx.annotations.collectFirst { case jsonMemberNames(format) => format }
            .map(true -> _)
            .getOrElse(false -> identity)

        val params = ctx
          .params
          .filterNot { param =>
            param
              .annotations
              .collectFirst {
                case _: jsonExclude => ()
              }
              .isDefined
          }

        val len = params.length

        val names =
          IArray.genericWrapArray(params
            .map { p =>
              p.annotations.collectFirst {
                case jsonField(name) => name
              }.getOrElse(if (transformNames) nameTransform(p.label) else p.label)
            })
            .toArray

        lazy val tcs: Array[JsonEncoder[Any]] =
            IArray.genericWrapArray(params.map(_.typeclass.asInstanceOf[JsonEncoder[Any]])).toArray

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          out.write("{")

          var indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)

          var i = 0
          var prevFields = false

          inheritedHint.map { hint =>

            JsonEncoder.string.unsafeEncode(hint, indent_, out)
            if (indent.isEmpty) out.write(":")
            else out.write(" : ")
            JsonEncoder.string.unsafeEncode(correctHint, indent_, out)

            prevFields = true  
          }

          while (i < len) {
            val tc = tcs(i)
            val p  = params(i).deref(a)

            if (! tc.isNothing(p)) {
              // if we have at least one field already, we need a comma
              if (prevFields) {
                if (indent.isEmpty) {
                  out.write(",")
                } else {
                  out.write(",")
                  JsonEncoder.pad(indent_, out)
                }
              }

              JsonEncoder.string.unsafeEncode(names(i), indent_, out)

              if (indent.isEmpty) {
                out.write(":")
              } else {
                out.write(" : ")
              }

              tc.unsafeEncode(p, indent_, out)
              prevFields = true // at least one field so far
            }

            i += 1
          }

          JsonEncoder.pad(indent, out)
          out.write("}")
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          ctx.params
            .foldLeft[Either[String, Chunk[(String, Json)]]](Right(Chunk.empty)) { case (c, param) =>
              val name = param.annotations.collectFirst { case jsonField(name) =>
                name
              }.getOrElse(nameTransform(param.label))
              c.flatMap { chunk =>
                param.typeclass.toJsonAST(param.deref(a)).map { value =>
                  if (value == Json.Null) chunk
                  else chunk :+ name -> value
                }
              }
            }
            .map { chunk =>
              inheritedHint match {
                case None => chunk
                case Some(hint) => chunk :+ hint -> Json.Str(correctHint)
              }
            }
            .map(Json.Obj.apply)
        }
      }
    }
  }

  def split[A](ctx: SealedTrait[JsonEncoder, A]): JsonEncoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(IdentityFormat)
    val discrim = ctx
      .annotations
      .collectFirst {
        case jsonDiscriminator(n) => n
      }

    if (discrim.isEmpty) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          ctx.choose(a) { sub =>
            val name = sub
              .annotations
              .collectFirst {
                case jsonHint(name) => name
              }.getOrElse(jsonHintFormat(sub.typeInfo.short))

            out.write("{")
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(name, indent_, out)

            if (indent.isEmpty) out.write(":")
            else out.write(" : ")

            sub.typeclass.unsafeEncode(sub.cast(a), indent_, out)
            JsonEncoder.pad(indent, out)

            out.write("}")
          }
        }

        final override def toJsonAST(a: A): Either[String, Json] = {
          ctx.choose(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).map { inner =>
              val name = sub
                .annotations
                .collectFirst {
                  case jsonHint(name) => name
                }.getOrElse(jsonHintFormat(sub.typeInfo.short))

              Json.Obj(
                Chunk(
                  name -> inner
                )
              )
            }
          }
        }
      }
    } else {
      val hintField = discrim.get

      def getName(annotations: Iterable[_], default: => String): String =
        annotations
          .collectFirst { case jsonHint(name) => name }
          .getOrElse(jsonHintFormat(default))

      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          ctx.choose(a) { sub =>
            out.write("{")
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(hintField, indent_, out)
            if (indent.isEmpty) out.write(":")
            else out.write(" : ")
            JsonEncoder.string.unsafeEncode(getName(sub.annotations, sub.typeInfo.short), indent_, out)

            // whitespace is always off by 2 spaces at the end, probably not worth fixing
            val intermediate = new NestedWriter(out, indent_)
            sub.typeclass.unsafeEncode(sub.cast(a), indent, intermediate)
          }
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          ctx.choose(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).flatMap {
              case Json.Obj(fields) => Right(Json.Obj(fields :+ hintField -> Json.Str(getName(sub.annotations, sub.typeInfo.short))))
              case _                => Left("Subtype is not encoded as an object")
            }
          }
        }
      }
    }
  }

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

  // intercepts the first `{` of a nested writer and discards it. We also need to
  // inject a `,` unless an empty object `{}` has been written.
  private[this] final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
    private[this] var first, second = true

    def write(c: Char): Unit = write(c.toString) // could be optimised

    def write(s: String): Unit =
      if (first || second) {
        var i = 0
        while (i < s.length) {
          val c = s.charAt(i)
          if (c == ' ' || c == '\n') {} else if (first && c == '{') {
            first = false
          } else if (second) {
            second = false
            if (c != '}') {
              out.write(',')
              JsonEncoder.pad(indent, out)
            }
            return out.write(s.substring(i))
          }
          i += 1
        }
      } else out.write(s)
    }
}

object DeriveJsonCodec {
  inline def gen[A](using mirror: Mirror.Of[A]) = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]

    JsonCodec(encoder, decoder)
  }
}
