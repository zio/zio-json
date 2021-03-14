package zio.json

import scala.annotation._
import scala.collection.mutable
import scala.language.experimental.macros

import magnolia._

import zio.Chunk
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.ast.Json
import zio.json.internal.{ Lexer, RetractReader, StringMatrix, Write }

/**
 * If used on a case class field, determines the name of the JSON field.
 * Defaults to the case class field name.
 */
final case class jsonField(name: String) extends Annotation

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
 * If used on a case class will determine the type hint value for disambiguating
 * sealed traits. Defaults to the short type name.
 */
final case class jsonHint(name: String) extends Annotation

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

object DeriveJsonDecoder {
  type Typeclass[A] = JsonDecoder[A]

  def combine[A](ctx: CaseClass[JsonDecoder, A]): JsonDecoder[A] = {
    val no_extra = ctx.annotations.collectFirst { case _: jsonNoExtraFields =>
      ()
    }.isDefined
    if (ctx.parameters.isEmpty)
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

        override final def fromJsonAST(json: Json): Either[String, A] =
          json match {
            case Json.Obj(_) => Right(ctx.rawConstruct(Nil))
            case Json.Null   => Right(ctx.rawConstruct(Nil))
            case _           => Left("Not an object")
          }
      }
    else
      new JsonDecoder[A] {
        val names: Array[String] = ctx.parameters.map { p =>
          p.annotations.collectFirst { case jsonField(name) =>
            name
          }.getOrElse(p.label)
        }.toArray
        val len: Int                = names.length
        val matrix: StringMatrix    = new StringMatrix(names)
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        lazy val tcs: Array[JsonDecoder[Any]] =
          ctx.parameters.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
        lazy val defaults: Array[Option[Any]] =
          ctx.parameters.map(_.default).toArray
        lazy val namesMap: Map[String, Int] = names.zipWithIndex.toMap

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')

          // TODO it would be more efficient to have a solution that didn't box
          // primitives, but Magnolia does not expose an API for that. Adding
          // such a feature to Magnolia is the only way to avoid this, e.g. a
          // ctx.createMutableCons that specialises on the types (with some way
          // of noting that things have been initialised), which can be called
          // to instantiate the case class. Would also require JsonDecoder to be
          // specialised.
          val ps: Array[Any] = Array.ofDim(len)

          if (Lexer.firstField(trace, in))
            do {
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
            } while (Lexer.nextField(trace, in))

          var i = 0
          while (i < len) {
            if (ps(i) == null) {
              if (defaults(i).isDefined)
                ps(i) = defaults(i).get
              else
                ps(i) = tcs(i).unsafeDecodeMissing(spans(i) :: trace)
            }
            i += 1
          }

          ctx.rawConstruct(new ArraySeq(ps))
        }

        override final def fromJsonAST(json: Json): Either[String, A] =
          json match {
            case Json.Obj(fields) =>
              val ps: Array[Any]           = Array.ofDim(len)
              var hasInvalidExtra: Boolean = false
              val failures                 = new mutable.LinkedHashSet[String]

              for ((key, value) <- fields) {
                namesMap.get(key) match {
                  case Some(field) =>
                    if (defaults(field).isDefined) {
                      val opt = JsonDecoder.option(tcs(field)).fromJsonAST(value)
                      ps(field) = opt.flatMap(_.toRight(Left(""))).getOrElse(defaults(field).get)
                    } else {
                      ps(field) = tcs(field).fromJsonAST(value) match {
                        case Left(error)  => failures += error; null
                        case Right(value) => value
                      }
                    }
                  case None =>
                    if (no_extra) {
                      hasInvalidExtra = true
                    }
                }
              }

              var i       = 0
              val missing = new mutable.LinkedHashSet[String]
              while (i < len) {
                if (ps(i) == null) {
                  if (defaults(i).isDefined) {
                    ps(i) = defaults(i).get
                  } else {
                    missing.add(names(i))
                  }
                }
                i += 1
              }

              if (hasInvalidExtra)
                Left("Invalid extra field")
              else if (failures.nonEmpty)
                Left(failures.mkString(", "))
              else if (missing.nonEmpty)
                Left(s"Missing fields: ${missing.mkString(", ")}")
              else
                Right(ctx.rawConstruct(new ArraySeq(ps)))

            case _ => Left("Not an object")
          }
      }
  }

  def dispatch[A](ctx: SealedTrait[JsonDecoder, A]): JsonDecoder[A] = {
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) =>
        name
      }.getOrElse(p.typeName.short)
    }.toArray
    val matrix: StringMatrix = new StringMatrix(names)
    lazy val tcs: Array[JsonDecoder[Any]] =
      ctx.subtypes.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    lazy val namesMap: Map[String, Int] = names.zipWithIndex.toMap

    def discrim = ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }
    if (discrim.isEmpty)
      new JsonDecoder[A] {
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          // we're not allowing extra fields in this encoding
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

        override final def fromJsonAST(json: Json): Either[String, A] =
          json match {
            case Json.Obj(chunk) if chunk.size == 1 =>
              val (key, inner) = chunk.head
              namesMap.get(key) match {
                case Some(idx) => tcs(idx).fromJsonAST(inner).map(_.asInstanceOf[A])
                case None      => Left("Invalid disambiguator")
              }
            case Json.Obj(_) => Left("Not an object with a single field")
            case _           => Left("Not an object")
          }
      }
    else
      new JsonDecoder[A] {
        val hintfield               = discrim.get
        val hintmatrix              = new StringMatrix(Array(hintfield))
        val spans: Array[JsonError] = names.map(JsonError.Message(_))

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val in_ = internal.RecordingReader(in)
          Lexer.char(trace, in_, '{')
          if (Lexer.firstField(trace, in_))
            do {
              if (Lexer.field(trace, in_, hintmatrix) != -1) {
                val field = Lexer.enum(trace, in_, matrix)
                if (field == -1)
                  throw UnsafeJson(
                    JsonError.Message(s"invalid disambiguator") :: trace
                  )
                in_.rewind()
                val trace_ = spans(field) :: trace
                return tcs(field).unsafeDecode(trace_, in_).asInstanceOf[A]
              } else
                Lexer.skipValue(trace, in_)
            } while (Lexer.nextField(trace, in_))

          throw UnsafeJson(
            JsonError.Message(s"missing hint '$hintfield'") :: trace
          )
        }

        override final def fromJsonAST(json: Json): Either[String, A] =
          json match {
            case Json.Obj(fields) =>
              fields.find { case (k, _) => k == hintfield } match {
                case Some((_, Json.Str(name))) =>
                  namesMap.get(name) match {
                    case Some(idx) => tcs(idx).fromJsonAST(json).map(_.asInstanceOf[A])
                    case None      => Left("Invalid disambiguator")
                  }
                case Some(_) =>
                  Left(s"Non-string hint '$hintfield'")
                case None =>
                  Left(s"Missing hint '$hintfield'")
              }
            case _ => Left("Not an object")
          }
      }
  }

  def gen[A]: JsonDecoder[A] = macro Magnolia.gen[A]
}

object DeriveJsonEncoder {
  type Typeclass[A] = JsonEncoder[A]

  def combine[A](ctx: CaseClass[JsonEncoder, A]): JsonEncoder[A] =
    if (ctx.parameters.isEmpty)
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write("{}")

        override final def toJsonAST(a: A): Either[String, Json] =
          Right(Json.Obj(Chunk.empty))
      }
    else
      new JsonEncoder[A] {
        val params = ctx.parameters.toArray
        val names: Array[String] = params.map { p =>
          p.annotations.collectFirst { case jsonField(name) =>
            name
          }.getOrElse(p.label)
        }
        lazy val tcs: Array[JsonEncoder[Any]] = params.map(p => p.typeclass.asInstanceOf[JsonEncoder[Any]])
        val len: Int                          = params.length
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var i = 0
          out.write("{")
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)

          var prevFields = false // whether any fields have been written
          while (i < len) {
            val tc = tcs(i)
            val p  = params(i).dereference(a)
            if (!tc.isNothing(p)) {
              // if we have at least one field already, we need a comma
              if (prevFields) {
                if (indent.isEmpty) out.write(",")
                else {
                  out.write(",")
                  JsonEncoder.pad(indent_, out)
                }
              }
              JsonEncoder.string.unsafeEncode(names(i), indent_, out)
              if (indent.isEmpty) out.write(":")
              else out.write(" : ")
              tc.unsafeEncode(p, indent_, out)
              prevFields = true // record that we have at least one field so far
            }
            i += 1
          }
          JsonEncoder.pad(indent, out)
          out.write("}")
        }

        override final def toJsonAST(a: A): Either[String, Json] =
          ctx.parameters
            .foldLeft[Either[String, Chunk[(String, Json)]]](Right(Chunk.empty)) { case (c, param) =>
              val name = param.annotations.collectFirst { case jsonField(name) =>
                name
              }.getOrElse(param.label)
              c.flatMap { chunk =>
                param.typeclass.toJsonAST(param.dereference(a)).map { value =>
                  if (value == Json.Null) chunk
                  else chunk :+ name -> value
                }
              }
            }
            .map(Json.Obj.apply)
      }

  def dispatch[A](ctx: SealedTrait[JsonEncoder, A]): JsonEncoder[A] = {
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) =>
        name
      }.getOrElse(p.typeName.short)
    }.toArray
    def discrim = ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }
    if (discrim.isEmpty)
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = ctx.dispatch(a) { sub =>
          out.write("{")
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          JsonEncoder.string.unsafeEncode(names(sub.index), indent_, out)
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          sub.typeclass.unsafeEncode(sub.cast(a), indent_, out)
          JsonEncoder.pad(indent, out)
          out.write("}")
        }

        override def toJsonAST(a: A): Either[String, Json] =
          ctx.dispatch(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).map { inner =>
              Json.Obj(
                Chunk(
                  names(sub.index) -> inner
                )
              )
            }
          }
      }
    else
      new JsonEncoder[A] {
        val hintfield = discrim.get
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = ctx.dispatch(a) { sub =>
          out.write("{")
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          JsonEncoder.string.unsafeEncode(hintfield, indent_, out)
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          JsonEncoder.string.unsafeEncode(names(sub.index), indent_, out)

          // whitespace is always off by 2 spaces at the end, probably not worth fixing
          val intermediate = new NestedWriter(out, indent_)
          sub.typeclass.unsafeEncode(sub.cast(a), indent, intermediate)
        }

        override def toJsonAST(a: A): Either[String, Json] =
          ctx.dispatch(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).flatMap {
              case Json.Obj(fields) => Right(Json.Obj(fields :+ hintfield -> Json.Str(names(sub.index))))
              case _                => Left("Subtype is not encoded as an object")
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

object DeriveJsonCodec {
  def gen[A]: JsonCodec[A] = macro Magnolia.gen[A]

  type Typeclass[A] = JsonCodec[A]

  def combine[A](ctx: CaseClass[JsonCodec, A]): JsonCodec[A] = {
    val encoder = DeriveJsonEncoder.combine(ctx.asInstanceOf[CaseClass[JsonEncoder, A]])
    val decoder = DeriveJsonDecoder.combine(ctx.asInstanceOf[CaseClass[JsonDecoder, A]])

    JsonCodec(encoder, decoder)
  }

  def dispatch[A](ctx: SealedTrait[JsonCodec, A]): JsonCodec[A] = {
    val encoder = DeriveJsonEncoder.dispatch(ctx.asInstanceOf[SealedTrait[JsonEncoder, A]])
    val decoder = DeriveJsonDecoder.dispatch(ctx.asInstanceOf[SealedTrait[JsonDecoder, A]])

    JsonCodec(encoder, decoder)
  }
}
