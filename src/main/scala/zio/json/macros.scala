package zio.json

import scala.annotation._
import scala.reflect.macros.whitebox

import magnolia._

import zio.Chunk
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.internal.{ Lexer, RetractReader, StringMatrix }

/**
 * If used on a case class field, determines the name of the JSON field.
 * Defaults to the case class field name.
 */
final case class field(name: String) extends Annotation

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
final case class discriminator(name: String) extends Annotation
// TODO a strategy where the constructor is inferred from the field names, only
// valid if there is no ambiguity in the types of fields for all case classes.
// Such a strategy cannot be implemented with Magnolia because the SealedTrait
// does not provide a mechanism for obtaining the CaseClass associated to the
// Subtype.

/**
 * If used on a case class will determine the typehint value for disambiguating
 * sealed traits. Defaults to the short type name.
 */
final case class hint(name: String) extends Annotation

/**
 * If used on a case class, will exit early if any fields are in the JSON that
 * do not correspond to field names in the case class.
 *
 * This adds extra protections against a DOS attacks but means that changes in
 * the schema will result in a hard error rather than silently ignoring those
 * fields.
 *
 * Cannot be comibned with `@discriminator` since it is considered an extra
 * field from the perspective of the case class.
 */
final class no_extra_fields extends Annotation

object DeriveJsonDecoder {
  type Typeclass[+A] = JsonDecoder[A]

  def combine[A](ctx: CaseClass[JsonDecoder, A]): JsonDecoder[A] = {
    val no_extra = ctx.annotations.collectFirst {
      case _: no_extra_fields => ()
    }.isDefined
    if (ctx.parameters.isEmpty)
      new JsonDecoder[A] {
        def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A = {
          if (no_extra) {
            Lexer.char(trace, in, '{')
            Lexer.char(trace, in, '}')
          } else {
            Lexer.skipValue(trace, in)
          }
          ctx.rawConstruct(Nil)
        }
      }
    else
      new JsonDecoder[A] {
        val names: Array[String] = ctx.parameters.map { p =>
          p.annotations.collectFirst {
            case field(name) => name
          }.getOrElse(p.label)
        }.toArray
        val len: Int                = names.length
        val matrix: StringMatrix    = new StringMatrix(names)
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        lazy val tcs: Array[JsonDecoder[Any]] =
          ctx.parameters.map(_.typeclass).toArray

        def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A = {
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
                val field_ = names(field)
                trace_ = trace :+ spans(field)
                if (ps(field) != null)
                  throw UnsafeJson(trace :+ JsonError.Message("duplicate"))
                ps(field) = tcs(field).unsafeDecode(trace_, in)
              } else if (no_extra) {
                throw UnsafeJson(
                  trace :+ JsonError.Message(s"invalid extra field")
                )
              } else
                Lexer.skipValue(trace_, in)
            } while (Lexer.nextField(trace, in))

          var i = 0
          while (i < len) {
            if (ps(i) == null)
              ps(i) = tcs(i).unsafeDecodeMissing(trace :+ spans(i))
            i += 1
          }

          ctx.rawConstruct(new ArraySeq(ps))
        }
      }
  }

  def dispatch[A](ctx: SealedTrait[JsonDecoder, A]): JsonDecoder[A] = {
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst {
        case hint(name) => name
      }.getOrElse(p.typeName.short)
    }.toArray
    val len: Int             = names.length
    val matrix: StringMatrix = new StringMatrix(names)
    lazy val tcs: Array[JsonDecoder[Any]] =
      ctx.subtypes.map(_.typeclass).toArray

    def discrim = ctx.annotations.collectFirst { case discriminator(n) => n }
    if (discrim.isEmpty)
      new JsonDecoder[A] {
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          // we're not allowing extra fields in this encoding
          if (Lexer.firstField(trace, in)) {
            val field = Lexer.field(trace, in, matrix)
            if (field != -1) {
              val field_ = names(field)
              val trace_ = trace :+ spans(field)
              val a      = tcs(field).unsafeDecode(trace_, in).asInstanceOf[A]
              Lexer.char(trace, in, '}')
              return a
            } else
              throw UnsafeJson(
                trace :+ JsonError.Message("invalid disambiguator")
              )
          } else
            throw UnsafeJson(
              trace :+ JsonError.Message("expected non-empty object")
            )
        }
      }
    else
      new JsonDecoder[A] {
        val hintfield               = discrim.get
        val hintmatrix              = new StringMatrix(Array(hintfield))
        val spans: Array[JsonError] = names.map(JsonError.Message(_))

        def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A = {
          val in_ = internal.RecordingReader(in)
          Lexer.char(trace, in_, '{')
          if (Lexer.firstField(trace, in_))
            do {
              if (Lexer.field(trace, in_, hintmatrix) != -1) {
                val field = Lexer.enum(trace, in_, matrix)
                if (field == -1)
                  throw UnsafeJson(
                    trace :+ JsonError.Message(s"invalid disambiguator")
                  )
                in_.rewind()
                val trace_ = trace :+ spans(field)
                return tcs(field).unsafeDecode(trace_, in_).asInstanceOf[A]
              } else
                Lexer.skipValue(trace, in_)
            } while (Lexer.nextField(trace, in_))

          throw UnsafeJson(
            trace :+ JsonError.Message(s"missing hint '$hintfield'")
          )
        }
      }
  }

  def gen[A]: JsonDecoder[A] = macro Magnolia.gen[A]
}

object DeriveJsonEncoder {
  type Typeclass[-A] = JsonEncoder[A]

  def combine[A](ctx: CaseClass[JsonEncoder, A]): JsonEncoder[A] =
    if (ctx.parameters.isEmpty)
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = out.write("{}")
      }
    else
      new JsonEncoder[A] {
        val params = ctx.parameters.toArray
        val names: Array[String] = params.map { p =>
          p.annotations.collectFirst {
            case field(name) => name
          }.getOrElse(p.label)
        }
        lazy val tcs: Array[JsonEncoder[Any]] = params.map(p => p.typeclass.asInstanceOf[JsonEncoder[Any]])
        val len: Int                          = params.length
        def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = {
          var i = 0
          out.write("{")
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)

          while (i < len) {
            val tc = tcs(i)
            val p  = params(i).dereference(a)
            if (!tc.isNothing(p)) {
              if (i > 0) {
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
            }
            i += 1
          }
          JsonEncoder.pad(indent, out)
          out.write("}")
        }
      }

  def dispatch[A](ctx: SealedTrait[JsonEncoder, A]): JsonEncoder[A] = {
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst {
        case hint(name) => name
      }.getOrElse(p.typeName.short)
    }.toArray
    def discrim = ctx.annotations.collectFirst { case discriminator(n) => n }
    if (discrim.isEmpty)
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = ctx.dispatch(a) { sub =>
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
      }
    else
      new JsonEncoder[A] {
        val hintfield = discrim.get
        def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit = ctx.dispatch(a) { sub =>
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
private[this] final class NestedWriter(out: java.io.Writer, indent: Option[Int]) extends java.io.Writer {
  def close(): Unit               = out.close()
  def flush(): Unit               = out.flush()
  private[this] var first, second = true
  def write(cs: Array[Char], from: Int, len: Int): Unit =
    if (first || second) {
      var i = 0
      while (i < len) {
        val c = cs(from + i)
        if (c == ' ' || c == '\n') {} else if (first && c == '{') {
          first = false
        } else if (second) {
          second = false
          if (c != '}') {
            out.append(",")
            JsonEncoder.pad(indent, out)
          }
          return out.write(cs, from + i, len - i)
        }
        i += 1
      }
    } else out.write(cs, from, len)
}
