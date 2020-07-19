package zio.json

import scala.annotation.Annotation
import scala.reflect.macros.whitebox

import magnolia._

import zio.json.Decoder.{ JsonError, UnsafeJson }
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
 * Note that using a discriminator is less performant, uses more memory, and
 * may be prone to DOS attacks that are impossible with the default encoding.
 * Only use this option if you must model an externally defined schema.
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

object MagnoliaDecoder {
  type Typeclass[A] = Decoder[A]

  def combine[A](ctx: CaseClass[Decoder, A]): Decoder[A] = {
    val no_extra = ctx.annotations.collectFirst {
      case _: no_extra_fields => ()
    }.isDefined
    if (ctx.parameters.isEmpty)
      new Decoder[A] {
        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
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
      new Decoder[A] {
        val names: Array[String] = ctx.parameters.map { p =>
          p.annotations.collectFirst {
            case field(name) => name
          }.getOrElse(p.label)
        }.toArray
        val len: Int                = names.length
        val matrix: StringMatrix    = new StringMatrix(names)
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        lazy val tcs: Array[Decoder[Any]] =
          ctx.parameters.map(_.typeclass.widen[Any]).toArray

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')

          // TODO it would be more efficient to have a solution that didn't box
          // primitives, but Magnolia does not expose an API for that. Adding
          // such a feature to Magnolia is the only way to avoid this, e.g. a
          // ctx.createMutableCons that specialises on the types (with some way
          // of noting that things have been initialised), which can be called
          // to instantiate the case class. Would also require Decoder to be
          // specialised.
          val ps: Array[Any] = Array.ofDim(len)

          if (Lexer.firstObject(trace, in))
            do {
              var trace_ = trace
              val field  = Lexer.field(trace, in, matrix)
              if (field != -1) {
                val field_ = names(field)
                trace_ = spans(field) :: trace
                if (ps(field) != null)
                  throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                ps(field) = tcs(field).unsafeDecode(trace_, in)
              } else if (no_extra) {
                throw UnsafeJson(
                  JsonError.Message(s"invalid extra field") :: trace
                )
              } else
                Lexer.skipValue(trace_, in)
            } while (Lexer.nextObject(trace, in))

          var i = 0
          while (i < len) {
            if (ps(i) == null)
              ps(i) = tcs(i).unsafeDecodeMissing(spans(i) :: trace)
            i += 1
          }

          ctx.rawConstruct(new ArraySeq(ps))
        }
      }
  }

  def dispatch[A](ctx: SealedTrait[Decoder, A]): Decoder[A] = {
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst {
        case hint(name) => name
      }.getOrElse(p.typeName.short)
    }.toArray
    val len: Int             = names.length
    val matrix: StringMatrix = new StringMatrix(names)
    lazy val tcs: Array[Decoder[Any]] =
      ctx.subtypes.map(_.typeclass.widen[Any]).toArray

    def discrim = ctx.annotations.collectFirst { case discriminator(n) => n }
    if (discrim.isEmpty)
      new Decoder[A] {
        val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))
        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          // we're not allowing extra fields in this encoding
          if (Lexer.firstObject(trace, in)) {
            val field = Lexer.field(trace, in, matrix)
            if (field != -1) {
              val field_ = names(field)
              val trace_ = spans(field) :: trace
              val a      = tcs(field).unsafeDecode(trace_, in).asInstanceOf[A]
              Lexer.char(trace, in, '}')
              return a
            } else
              throw UnsafeJson(
                JsonError.Message("invalid disambiguator") :: trace
              )
          } else
            throw UnsafeJson(
              JsonError.Message("expected non-empty object") :: trace
            )
        }
      }
    else
      new Decoder[A] {
        val hintfield               = discrim.getOrElse("type")
        val hintmatrix              = new StringMatrix(Array(hintfield))
        val spans: Array[JsonError] = names.map(JsonError.Message(_))

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val in_ = internal.RecordingReader(in)
          Lexer.char(trace, in_, '{')
          if (Lexer.firstObject(trace, in_))
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
            } while (Lexer.nextObject(trace, in_))

          throw UnsafeJson(
            JsonError.Message(s"missing hint '$hintfield'") :: trace
          )
        }
      }
  }

  def gen[A]: Decoder[A] = macro Magnolia.gen[A]
}

// backcompat for 2.12, otherwise we'd use ArraySeq.unsafeWrapArray
private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
  def apply(i: Int): Any = p(i)
  def length: Int        = p.length
}
