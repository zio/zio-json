package zio.json

import scala.annotation._
import scala.collection.mutable
import scala.util.control.NoStackTrace
import zio.json.internal._
import Decoder.JsonError

import scala.reflect.ClassTag

// convenience to match the circe api
object parser {

  /**
   * Attempts to decode the raw JSON string as an `A`.
   *
   * On failure a human readable message is returned using a jq friendly
   * format. For example the error
   * `.rows[0].elements[0].distance.value(missing)"` tells us the location of a
   * missing field named "value". We can use part of the error message in the
   * `jq` command line tool for further inspection, e.g.
   *
   * {{{jq '.rows[0].elements[0].distance' input.json}}}
   */
  def decode[A](str: CharSequence)(implicit D: Decoder[A]): Either[String, A] =
    D.decodeJson(str)
}

trait Decoder[A] { self =>
  // note that the string may not be fully consumed
  final def decodeJson(str: CharSequence): Either[String, A] =
    try Right(unsafeDecode(Nil, new FastStringReader(str)))
    catch {
      case Decoder.UnsafeJson(trace) => Left(JsonError.render(trace))
      case _: internal.UnexpectedEnd => Left("unexpected end of input")
    }

  // scalaz-deriving style MonadError combinators
  final def widen[B >: A]: Decoder[B]                 = self.asInstanceOf[Decoder[B]]
  final def xmap[B](f: A => B, g: B => A): Decoder[B] = map(f)
  final def map[B](f: A => B): Decoder[B] =
    new Decoder[B] {
      override def unsafeDecodeMissing(trace: List[JsonError]): B =
        f(self.unsafeDecodeMissing(trace))
      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in))
    }
  final def emap[B](f: A => Either[String, B]): Decoder[B] =
    new Decoder[B] {
      override def unsafeDecodeMissing(trace: List[JsonError]): B =
        f(self.unsafeDecodeMissing(trace)) match {
          case Left(err) =>
            throw Decoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }
      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in)) match {
          case Left(err) =>
            throw Decoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }
    }

  // The unsafe* methods are internal and should only be used by generated
  // decoders and web frameworks.
  //
  // They are unsafe because they are non-total and use mutable references.
  //
  // We could use a ReaderT[List[JsonError]] but that would bring in
  // dependencies and overhead, so we pass the trace context manually.
  def unsafeDecodeMissing(trace: List[JsonError]): A =
    throw Decoder.UnsafeJson(JsonError.Message("missing") :: trace)

  def unsafeDecode(trace: List[JsonError], in: RetractReader): A
}

object Decoder extends GeneratedTuples with DecoderLowPriority1 with DecoderLowPriority2 {
  def apply[A](implicit a: Decoder[A]): Decoder[A] = a

  // Design note: we could require the position in the stream here to improve
  // debugging messages. But the cost would be that the RetractReader would need
  // to keep track and any wrappers would need to preserve the position. It may
  // still be desirable to do this but at the moment it is not necessary.
  final case class UnsafeJson(trace: List[JsonError])
      extends Exception("if you see this a dev made a mistake using Decoder")
      with NoStackTrace

  /* Allows a human readable string to be generated for decoding failures. */
  sealed abstract class JsonError
  object JsonError {
    def render(trace: List[JsonError]): String =
      trace.reverse.map {
        case Message(txt)        => s"($txt)"
        case ArrayAccess(i)      => s"[$i]"
        case ObjectAccess(field) => s".$field"
        case SumType(cons)       => s"{$cons}"
      }.mkString
    final case class Message(txt: String)        extends JsonError
    final case class ArrayAccess(i: Int)         extends JsonError
    final case class ObjectAccess(field: String) extends JsonError
    final case class SumType(cons: String)       extends JsonError
  }

  implicit val string: Decoder[String] = new Decoder[String] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): String =
      Lexer.string(trace, in).toString
  }
  implicit val boolean: Decoder[Boolean] = new Decoder[Boolean] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Boolean =
      Lexer.boolean(trace, in)
  }

  implicit val char: Decoder[Char] = string.emap {
    case str if str.length == 1 => Right(str(0))
    case other                  => Left("expected one character")
  }
  implicit val symbol: Decoder[Symbol] = string.map(Symbol(_))

  implicit val byte: Decoder[Byte]   = number(Lexer.byte)
  implicit val short: Decoder[Short] = number(Lexer.short)
  implicit val int: Decoder[Int]     = number(Lexer.int)
  implicit val long: Decoder[Long]   = number(Lexer.long)
  implicit val biginteger: Decoder[java.math.BigInteger] = number(
    Lexer.biginteger
  )
  implicit val float: Decoder[Float]   = number(Lexer.float)
  implicit val double: Decoder[Double] = number(Lexer.double)
  implicit val bigdecimal: Decoder[java.math.BigDecimal] = number(
    Lexer.bigdecimal
  )
  // numbers decode from numbers or strings for maximum compatibility
  private[this] def number[A](
    f: (List[JsonError], RetractReader) => A
  ): Decoder[A] =
    new Decoder[A] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
        (in.nextNonWhitespace(): @switch) match {
          case '"' =>
            val i = f(trace, in)
            Lexer.charOnly(trace, in, '"')
            i
          case _ =>
            in.retract()
            f(trace, in)
        }
    }

  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.
  implicit def option[A](implicit A: Decoder[A]): Decoder[Option[A]] =
    new Decoder[Option[A]] {
      private[this] val ull: Array[Char] = "ull".toCharArray
      override def unsafeDecodeMissing(trace: List[JsonError]): Option[A] =
        Option.empty
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[A] =
        (in.nextNonWhitespace(): @switch) match {
          case 'n' =>
            Lexer.readChars(trace, in, ull, "null")
            None
          case _ =>
            in.retract()
            Some(A.unsafeDecode(trace, in))
        }
    }

  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  implicit def either[A, B](
    implicit
    A: Decoder[A],
    B: Decoder[B]
  ): Decoder[Either[A, B]] =
    new Decoder[Either[A, B]] {
      val names: Array[String] =
        Array("a", "Left", "left", "b", "Right", "right")
      val matrix: StringMatrix    = new StringMatrix(names)
      val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))

      def unsafeDecode(
        trace: List[JsonError],
        in: RetractReader
      ): Either[A, B] = {
        Lexer.char(trace, in, '{')

        val values: Array[Any] = Array.ofDim(2)

        if (Lexer.firstObject(trace, in))
          do {
            val field = Lexer.field(trace, in, matrix)
            if (field == -1) Lexer.skipValue(trace, in)
            else {
              val trace_ = spans(field) :: trace
              if (field < 3) {
                if (values(0) != null)
                  throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                values(0) = A.unsafeDecode(trace_, in)
              } else {
                if (values(1) != null)
                  throw UnsafeJson(JsonError.Message("duplicate") :: trace_)
                values(1) = B.unsafeDecode(trace_, in)
              }
            }
          } while (Lexer.nextObject(trace, in))

        if (values(0) == null && values(1) == null)
          throw UnsafeJson(JsonError.Message("missing fields") :: trace)
        if (values(0) != null && values(1) != null)
          throw UnsafeJson(
            JsonError.Message("ambiguous either, both present") :: trace
          )
        if (values(0) != null)
          Left(values(0).asInstanceOf[A])
        else Right(values(1).asInstanceOf[B])
      }
    }

  private[json] def builder[A, T[_]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.ReusableBuilder[A, T[A]]
  )(implicit A: Decoder[A]): T[A] = {
    Lexer.char(trace, in, '[')
    var i: Int = 0
    if (Lexer.firstArray(trace, in)) do {
      val trace_ = JsonError.ArrayAccess(i) :: trace
      builder += A.unsafeDecode(trace_, in)
      i += 1
    } while (Lexer.nextArray(trace, in))
    builder.result()
  }

  implicit def list[A: Decoder]: Decoder[List[A]] = new Decoder[List[A]] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): List[A] =
      builder(trace, in, new mutable.ListBuffer[A])
  }

  implicit def vector[A](implicit A: Decoder[A]): Decoder[Vector[A]] =
    list[A].map(_.toVector)

  implicit def seq[A](implicit A: Decoder[A]): Decoder[Seq[A]] =
    list[A].map(_.toSeq)

  implicit def set[A](implicit A: Decoder[A]): Decoder[Set[A]] =
    list[A].map(_.toSet)

  implicit def array[A:ClassTag](implicit A: Decoder[A]): Decoder[Array[A]] =
    new Decoder[Array[A]] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Array[A] = {
        var builder = Array[A]()
        Lexer.char(trace, in, '[')
        var trace_ = trace
        if (Lexer.firstArray(trace, in)) do {
          trace_ = JsonError.ArrayAccess(builder.length) :: trace
          builder = builder :+ A.unsafeDecode(trace_, in)
        } while (Lexer.nextArray(trace_, in))
        builder
      }
    }

  // not implicit because this overlaps with decoders for lists of tuples
  def keylist[K, A](
    implicit
    K: FieldDecoder[K],
    A: Decoder[A]
  ): Decoder[List[(K, A)]] =
    new Decoder[List[(K, A)]] {
      def unsafeDecode(
        trace: List[JsonError],
        in: RetractReader
      ): List[(K, A)] = {
        val builder = new mutable.ListBuffer[(K, A)]
        Lexer.char(trace, in, '{')
        if (Lexer.firstObject(trace, in))
          do {
            val field  = Lexer.string(trace, in).toString
            val trace_ = JsonError.ObjectAccess(field) :: trace
            Lexer.char(trace_, in, ':')
            val value = A.unsafeDecode(trace_, in)
            builder += ((K.unsafeDecodeField(trace_, field), value))
          } while (Lexer.nextObject(trace, in))
        builder.result()
      }
    }

  implicit def sortedmap[K: FieldDecoder, V: Decoder](
    implicit
    O: Ordering[K]
  ): Decoder[collection.SortedMap[K, V]] =
    keylist[K, V].map(lst => collection.SortedMap.apply(lst: _*))
}

private[json] trait DecoderLowPriority1 {
  this: Decoder.type =>

  // allows SortedMap to be found
  implicit def dict[K: FieldDecoder, V: Decoder]: Decoder[Map[K, V]] =
    keylist[K, V].map(_.toMap)
}

private[json] trait DecoderLowPriority2 {
  this: Decoder.type =>

  import scala.collection.generic.CanBuildFrom
  implicit def cbf[T[_], A: Decoder](
    implicit
    CBF: CanBuildFrom[List[A], A, T[A]]
  ): Decoder[T[A]] = list[A].map(lst => CBF(lst).result())

}

/** When decoding a JSON Object, we only allow the keys that implement this interface. */
trait FieldDecoder[A] { self =>
  final def widen[B >: A]: FieldDecoder[B] =
    self.asInstanceOf[FieldDecoder[B]]
  final def xmap[B](f: A => B, g: B => A): FieldDecoder[B] = map(f)
  final def map[B](f: A => B): FieldDecoder[B] =
    new FieldDecoder[B] {
      def unsafeDecodeField(trace: List[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in))
    }
  final def emap[B](f: A => Either[String, B]): FieldDecoder[B] =
    new FieldDecoder[B] {
      def unsafeDecodeField(trace: List[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in)) match {
          case Left(err) =>
            throw Decoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }
    }

  def unsafeDecodeField(trace: List[JsonError], in: String): A
}
object FieldDecoder {
  implicit val string: FieldDecoder[String] = new FieldDecoder[String] {
    def unsafeDecodeField(trace: List[JsonError], in: String): String = in
  }
}
