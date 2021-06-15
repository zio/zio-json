package zio.json

import zio.json.ast.Json
import scala.annotation.*
import magnolia.*
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

/**
 * If used on a case class field, will exclude it from the resulting JSON.
 */
final class jsonExclude extends Annotation

object DeriveJsonEncoder extends Derivation[JsonEncoder] { self =>
  def join[A](ctx: CaseClass[Typeclass, A]): JsonEncoder[A] =
    if (ctx.params.isEmpty) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
          out.write("{}")

        override final def toJsonAST(a: A): Either[String, Json] =
          Right(Json.Obj(Chunk.empty))
      }
    } else {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
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
            ctx
              .params
              .map { p =>
                p.annotations.collectFirst {
                  case jsonField(name) => name
                }.getOrElse(p.label)
              }
              .toArray

          val tcs: Array[JsonEncoder[Any]] =
            params.map(_.typeclass.asInstanceOf[JsonEncoder[Any]]).toArray

          out.write("{")

          var indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)

          var i = 0
          var prevFields = false

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
              }.getOrElse(param.label)
              c.flatMap { chunk =>
                param.typeclass.toJsonAST(param.deref(a)).map { value =>
                  if (value == Json.Null) chunk
                  else chunk :+ name -> value
                }
              }
            }
            .map(Json.Obj.apply)
        }
      }
    }

  def split[A](ctx: SealedTrait[JsonEncoder, A]): JsonEncoder[A] = {
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
              }.getOrElse(sub.typeInfo.short)

            out.write("{")
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(name, indent_, out)

            if (indent.isEmpty) {
              out.write(":")
            } else {
              out.write(" : ")
            }

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
                }.getOrElse(sub.typeInfo.short)

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
          .getOrElse(default)

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
