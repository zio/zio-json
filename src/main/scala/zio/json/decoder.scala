package zio.json

import scala.annotation._
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.NoStackTrace

import JsonDecoder.JsonError

import zio.json.internal._
import zio.{ Chunk, ZIO }
import zio.blocking._
import zio.stream.ZStream

trait JsonDecoder[+A] { self =>
  final def <>[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] = self.orElse(that)

  final def <+>[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] = self.orElseEither(that)

  final def <*>[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = self.zip(that)

  final def *>[B](that: => JsonDecoder[B]): JsonDecoder[B] = self.zipWith(that)((_, b) => b)

  final def <*[B](that: => JsonDecoder[B]): JsonDecoder[A] = self.zipWith(that)((a, _) => a)

  // note that the string may not be fully consumed
  final def decodeJson(str: CharSequence): Either[String, A] =
    try Right(unsafeDecode(Chunk.empty, new FastStringReader(str)))
    catch {
      case JsonDecoder.UnsafeJson(trace) => Left(JsonError.render(trace))
      case _: internal.UnexpectedEnd => Left("unexpected end of input")
    }

  final def decodeJsonStream[R <: Blocking](stream: ZStream[R, Throwable, Char]): ZIO[R, Throwable, A] =
    stream.toReader.use { reader =>
      effectBlocking {
        try unsafeDecode(Chunk.empty, new zio.json.internal.WithRetractReader(reader))
        catch {
          case JsonDecoder.UnsafeJson(trace) => throw new Exception(JsonError.render(trace))
          case _: internal.UnexpectedEnd => throw new Exception("unexpected end of input")
        }
      }
    }

  final def orElse[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] =
    new JsonDecoder[A1] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A1 = {
        val in2 = new zio.json.internal.WithRecordingReader(in, 0)

        try self.unsafeDecode(trace, in2)
        catch {
          case JsonDecoder.UnsafeJson(_) =>
            in2.rewind()
            that.unsafeDecode(trace, in2)

          case _: internal.UnexpectedEnd =>
            in2.rewind()
            that.unsafeDecode(trace, in2)
        }
      }
    }

  final def orElseEither[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    self.map(Left(_)).orElse(that.map(Right(_)))

  // scalaz-deriving style MonadError combinators
  final def map[B](f: A => B): JsonDecoder[B] =
    new JsonDecoder[B] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in))
    }

  final def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B] =
    new JsonDecoder[B] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in)) match {
          case Left(err) =>
            throw JsonDecoder.UnsafeJson(trace :+ JsonError.Message(err))
          case Right(b) => b
        }
    }

  final def zip[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = JsonDecoder.tuple2(this, that)

  final def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] =
    self.zip(that).map(f.tupled)

  // The unsafe* methods are internal and should only be used by generated
  // decoders and web frameworks.
  //
  // They are unsafe because they are non-total and use mutable references.
  //
  // We could use a ReaderT[List[JsonError]] but that would bring in
  // dependencies and overhead, so we pass the trace context manually.
  private[zio] def unsafeDecodeMissing(trace: Chunk[JsonError]): A =
    throw JsonDecoder.UnsafeJson(trace :+ JsonError.Message("missing"))

  private[zio] def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A
}

object JsonDecoder extends GeneratedTupleDecoders with DecoderLowPriority0 {
  def apply[A](implicit a: JsonDecoder[A]): JsonDecoder[A] = a

  // Design note: we could require the position in the stream here to improve
  // debugging messages. But the cost would be that the RetractReader would need
  // to keep track and any wrappers would need to preserve the position. It may
  // still be desirable to do this but at the moment it is not necessary.
  final case class UnsafeJson(trace: Chunk[JsonError])
      extends Exception("if you see this a dev made a mistake using JsonDecoder")
      with NoStackTrace

  /* Allows a human readable string to be generated for decoding failures. */
  sealed abstract class JsonError
  object JsonError {
    def render(trace: Chunk[JsonError]): String =
      trace.map {
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

  implicit val string: JsonDecoder[String] = new JsonDecoder[String] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): String =
      Lexer.string(trace, in).toString
  }
  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder[Boolean] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Boolean =
      Lexer.boolean(trace, in)
  }

  implicit val char: JsonDecoder[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case other                  => Left("expected one character")
  }
  implicit val symbol: JsonDecoder[Symbol] = string.map(Symbol(_))

  implicit val byte: JsonDecoder[Byte]   = number(Lexer.byte)
  implicit val short: JsonDecoder[Short] = number(Lexer.short)
  implicit val int: JsonDecoder[Int]     = number(Lexer.int)
  implicit val long: JsonDecoder[Long]   = number(Lexer.long)
  implicit val bigInteger: JsonDecoder[java.math.BigInteger] = number(
    Lexer.bigInteger
  )
  implicit val float: JsonDecoder[Float]   = number(Lexer.float)
  implicit val double: JsonDecoder[Double] = number(Lexer.double)
  implicit val bigDecimal: JsonDecoder[java.math.BigDecimal] = number(
    Lexer.bigDecimal
  )
  // numbers decode from numbers or strings for maximum compatibility
  private[this] def number[A](
    f: (Chunk[JsonError], RetractReader) => A
  ): JsonDecoder[A] =
    new JsonDecoder[A] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A =
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
  implicit def option[A](implicit A: JsonDecoder[A]): JsonDecoder[Option[A]] =
    new JsonDecoder[Option[A]] {
      private[this] val ull: Array[Char] = "ull".toCharArray
      override def unsafeDecodeMissing(trace: Chunk[JsonError]): Option[A] =
        Option.empty
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Option[A] =
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
    A: JsonDecoder[A],
    B: JsonDecoder[B]
  ): JsonDecoder[Either[A, B]] =
    new JsonDecoder[Either[A, B]] {
      val names: Array[String] =
        Array("a", "Left", "left", "b", "Right", "right")
      val matrix: StringMatrix    = new StringMatrix(names)
      val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))

      def unsafeDecode(
        trace: Chunk[JsonError],
        in: RetractReader
      ): Either[A, B] = {
        Lexer.char(trace, in, '{')

        val values: Array[Any] = Array.ofDim(2)

        if (Lexer.firstField(trace, in))
          do {
            val field = Lexer.field(trace, in, matrix)
            if (field == -1) Lexer.skipValue(trace, in)
            else {
              val trace_ = trace :+ spans(field)
              if (field < 3) {
                if (values(0) != null)
                  throw UnsafeJson(trace_ :+ JsonError.Message("duplicate"))
                values(0) = A.unsafeDecode(trace_, in)
              } else {
                if (values(1) != null)
                  throw UnsafeJson(trace_ :+ JsonError.Message("duplicate"))
                values(1) = B.unsafeDecode(trace_, in)
              }
            }
          } while (Lexer.nextField(trace, in))

        if (values(0) == null && values(1) == null)
          throw UnsafeJson(trace :+ JsonError.Message("missing fields"))
        if (values(0) != null && values(1) != null)
          throw UnsafeJson(
            trace :+ JsonError.Message("ambiguous either, both present")
          )
        if (values(0) != null)
          Left(values(0).asInstanceOf[A])
        else Right(values(1).asInstanceOf[B])
      }
    }

  private[json] def builder[A, T[_]](
    trace: Chunk[JsonError],
    in: RetractReader,
    builder: mutable.Builder[A, T[A]]
  )(implicit A: JsonDecoder[A]): T[A] = {
    Lexer.char(trace, in, '[')
    var i: Int = 0
    if (Lexer.firstArrayElement(trace, in)) do {
      val trace_ = trace :+ JsonError.ArrayAccess(i)
      builder += A.unsafeDecode(trace_, in)
      i += 1
    } while (Lexer.nextArrayElement(trace, in))
    builder.result()
  }

}

private[json] trait DecoderLowPriority0 extends DecoderLowPriority1 { this: JsonDecoder.type =>
  implicit def chunk[A: JsonDecoder]: JsonDecoder[Chunk[A]] = new JsonDecoder[Chunk[A]] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Chunk[A] =
      builder(trace, in, zio.ChunkBuilder.make[A]())
  }

  implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] = new JsonDecoder[List[A]] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): List[A] =
      builder(trace, in, new mutable.ListBuffer[A])
  }

  implicit def vector[A: JsonDecoder]: JsonDecoder[Vector[A]] = new JsonDecoder[Vector[A]] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Vector[A] =
      builder(trace, in, new immutable.VectorBuilder[A]).toVector
  }

  implicit def hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] =
    list[A].map(lst => immutable.HashSet(lst: _*))

  implicit def hashMap[K: FieldJsonDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    keyValueChunk[K, V].map(lst => immutable.HashMap(lst: _*))

  implicit def sortedMap[K: FieldJsonDecoder: Ordering, V: JsonDecoder]: JsonDecoder[collection.SortedMap[K, V]] =
    keyValueChunk[K, V].map(lst => collection.SortedMap.apply(lst: _*))

  implicit def sortedSet[A: Ordering: JsonDecoder]: JsonDecoder[immutable.SortedSet[A]] =
    list[A].map(lst => immutable.SortedSet(lst: _*))

}

// We have a hierarchy of implicits for two reasons:
//
// 1. the compiler searches each scope and returns early if it finds a match.
//    This means that it is faster to put more complex derivation rules (that
//    are unlikely to be commonly used) into a lower priority scope, allowing
//    simple things like primitives to match fast.
//
// 2. sometimes we want to have overlapping instances with a more specific /
//    optimised instances, and a fallback for the more general case that would
//    otherwise conflict in a lower priority scope. A good example of this is to
//    have specialised decoders for collection types, falling back to BuildFrom.
private[json] trait DecoderLowPriority1 {
  this: JsonDecoder.type =>

  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] = list[A]

  // not implicit because this overlaps with decoders for lists of tuples
  def keyValueChunk[K, A](
    implicit
    K: FieldJsonDecoder[K],
    A: JsonDecoder[A]
  ): JsonDecoder[Chunk[(K, A)]] =
    new JsonDecoder[Chunk[(K, A)]] {
      def unsafeDecode(
        trace: Chunk[JsonError],
        in: RetractReader
      ): Chunk[(K, A)] = {
        val builder = zio.ChunkBuilder.make[(K, A)]()
        Lexer.char(trace, in, '{')
        if (Lexer.firstField(trace, in))
          do {
            val field  = Lexer.string(trace, in).toString
            val trace_ = trace :+ JsonError.ObjectAccess(field)
            Lexer.char(trace_, in, ':')
            val value = A.unsafeDecode(trace_, in)
            builder += ((K.unsafeDecodeField(trace_, field), value))
          } while (Lexer.nextField(trace, in))
        builder.result()
      }
    }

  implicit def map[K: FieldJsonDecoder, V: JsonDecoder]: JsonDecoder[Map[K, V]] =
    keyValueChunk[K, V].map(lst => Map.apply(lst: _*))

  // TODO these could be optimised...
  implicit def set[A: JsonDecoder]: JsonDecoder[Set[A]] =
    list[A].map(lst => immutable.HashSet(lst: _*))
}

/** When decoding a JSON Object, we only allow the keys that implement this interface. */
trait FieldJsonDecoder[+A] { self =>
  final def map[B](f: A => B): FieldJsonDecoder[B] =
    new FieldJsonDecoder[B] {
      def unsafeDecodeField(trace: Chunk[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in))
    }
  final def mapOrFail[B](f: A => Either[String, B]): FieldJsonDecoder[B] =
    new FieldJsonDecoder[B] {
      def unsafeDecodeField(trace: Chunk[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in)) match {
          case Left(err) =>
            throw JsonDecoder.UnsafeJson(trace :+ JsonError.Message(err))
          case Right(b) => b
        }
    }

  def unsafeDecodeField(trace: Chunk[JsonError], in: String): A
}
object FieldJsonDecoder {
  implicit val string: FieldJsonDecoder[String] = new FieldJsonDecoder[String] {
    def unsafeDecodeField(trace: Chunk[JsonError], in: String): String = in
  }
}
