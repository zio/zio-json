/*
 * Copyright 2019-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.json

import zio.json.ast.Json
import zio.json.internal._
import zio.json.javatime.parsers
import zio.json.uuid.UUIDParser
import zio.{ Chunk, NonEmptyChunk }

import java.util.UUID
import scala.annotation._
import scala.collection.immutable.{ LinearSeq, ListSet, TreeSet }
import scala.collection.{ immutable, mutable }
import scala.util.control.NoStackTrace

/**
 * A `JsonDecoder[A]` instance has the ability to decode JSON to values of type `A`, potentially failing with an error
 * if the JSON content does not encode a value of the given type.
 */
trait JsonDecoder[A] extends JsonDecoderPlatformSpecific[A] {
  self =>

  /**
   * An alias for [[JsonDecoder#orElse]].
   */
  final def <>[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] = self.orElse(that)

  /**
   * An alias for [[JsonDecoder#orElseEither]].
   */
  final def <+>[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] = self.orElseEither(that)

  /**
   * An alias for [[JsonDecoder#zip]].
   */
  final def <*>[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = self.zip(that)

  /**
   * An alias for [[JsonDecoder#zipRight]].
   */
  final def *>[B](that: => JsonDecoder[B]): JsonDecoder[B] = self.zipRight(that)

  /**
   * An alias for [[JsonDecoder#zipLeft]].
   */
  final def <*[B](that: => JsonDecoder[B]): JsonDecoder[A] = self.zipLeft(that)

  /**
   * Attempts to decode a value of type `A` from the specified `CharSequence`, but may fail with a human-readable error
   * message if the provided text does not encode a value of this type.
   *
   * Note: This method may not entirely consume the specified character sequence.
   */
  final def decodeJson(str: CharSequence): Either[String, A] =
    try Right(unsafeDecode(Nil, new FastStringReader(str)))
    catch {
      case JsonDecoder.UnsafeJson(trace) => Left(JsonError.render(trace))
      case _: UnexpectedEnd              => Left("Unexpected end of input")
      case _: StackOverflowError         => Left("Unexpected structure")
    }

  /**
   * Returns this decoder but widened to the its given super-type
   */
  final def widen[B >: A]: JsonDecoder[B] = self.asInstanceOf[JsonDecoder[B]]

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics: such that if this
   * codec fails, the specified codec will be tried instead. This method may be unsafe from a security perspective: it
   * can use more memory than hand coded alternative and so lead to DOS.
   *
   * For example, in the case of an alternative between `Int` and `Boolean`, a hand coded alternative would look like:
   *
   * ```
   * val decoder: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
   * case 't' | 'f' => JsonDecoder[Boolean].widen
   * case c         => JsonDecoder[Int].widen
   * }
   * ```
   */
  final def orElse[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] =
    new JsonDecoder[A1] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): A1 = {
        val in2 = new zio.json.internal.WithRecordingReader(in, 64)

        try self.unsafeDecode(trace, in2)
        catch {
          case JsonDecoder.UnsafeJson(_) =>
            in2.rewind()
            that.unsafeDecode(trace, in2)

          case _: UnexpectedEnd =>
            in2.rewind()
            that.unsafeDecode(trace, in2)
        }
      }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A1 =
        try self.unsafeFromJsonAST(trace, json)
        catch {
          case JsonDecoder.UnsafeJson(_) | _: UnexpectedEnd => that.unsafeFromJsonAST(trace, json)
        }

      override def unsafeDecodeMissing(trace: List[JsonError]): A1 =
        try self.unsafeDecodeMissing(trace)
        catch {
          case _: Throwable => that.unsafeDecodeMissing(trace)
        }

    }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics: such that if this
   * codec fails, the specified codec will be tried instead.
   */
  final def orElseEither[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    self.map(Left(_)).orElse(that.map(Right(_)))

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function.
   */
  final def map[B](f: A => B): JsonDecoder[B] =
    new JsonDecoder[B] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): B =
        f(self.unsafeFromJsonAST(trace, json))

      override def unsafeDecodeMissing(trace: List[JsonError]): B =
        f(self.unsafeDecodeMissing(trace))

    }

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function, which may itself decide to fail
   * with some type of error.
   */
  final def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B] =
    new JsonDecoder[B] {

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in)) match {
          case Left(err) =>
            throw JsonDecoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): B =
        f(self.unsafeFromJsonAST(trace, json)) match {
          case Left(err) => throw JsonDecoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b)  => b
        }

      override def unsafeDecodeMissing(trace: List[JsonError]): B =
        f(self.unsafeDecodeMissing(trace)) match {
          case Left(err) =>
            throw JsonDecoder.UnsafeJson(JsonError.Message(err) :: trace)
          case Right(b) => b
        }

    }

  /**
   * Returns a new codec that combines this codec and the specified codec into a single codec that decodes a tuple of
   * the values decoded by the respective codecs.
   */
  final def zip[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = JsonDecoder.tuple2(this, that)

  /**
   * Zips two codecs, but discards the output on the right hand side.
   */
  final def zipLeft[B](that: => JsonDecoder[B]): JsonDecoder[A] = self.zipWith(that)((a, _) => a)

  /**
   * Zips two codecs, but discards the output on the left hand side.
   */
  final def zipRight[B](that: => JsonDecoder[B]): JsonDecoder[B] = self.zipWith(that)((_, b) => b)

  /**
   * Zips two codecs into one, transforming the outputs of zip codecs by the specified function.
   */
  final def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] =
    self.zip(that).map(f.tupled)

  def unsafeDecodeMissing(trace: List[JsonError]): A =
    throw JsonDecoder.UnsafeJson(JsonError.Message("missing") :: trace)

  /**
   * Low-level, unsafe method to decode a value or throw an exception. This method should not be called in application
   * code, although it can be implemented for user-defined data structures.
   */
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A

  def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
    unsafeDecode(trace, new FastStringReader(Json.encoder.encodeJson(json, None)))

  /**
   * Decode a value from an already parsed Json AST.
   *
   * The default implementation encodes the Json to a byte stream and uses decode to parse that. Override to provide a
   * more performant implementation.
   */
  final def fromJsonAST(json: Json): Either[String, A] =
    try Right(unsafeFromJsonAST(Nil, json))
    catch {
      case JsonDecoder.UnsafeJson(trace) => Left(JsonError.render(trace))
      case _: UnexpectedEnd              => Left("Unexpected end of input")
      case _: StackOverflowError         => Left("Unexpected structure")
    }

}

object JsonDecoder extends GeneratedTupleDecoders with DecoderLowPriority1 with JsonDecoderVersionSpecific {
  type JsonError = zio.json.JsonError
  val JsonError = zio.json.JsonError

  def apply[A](implicit a: JsonDecoder[A]): JsonDecoder[A] = a

  /**
   * Design note: we could require the position in the stream here to improve debugging messages. But the cost would be
   * that the RetractReader would need to keep track and any wrappers would need to preserve the position. It may still
   * be desirable to do this but at the moment it is not necessary.
   */
  final case class UnsafeJson(trace: List[JsonError])
      extends Exception("If you see this, a developer made a mistake using JsonDecoder")
      with NoStackTrace

  def peekChar[A](partialFunction: PartialFunction[Char, JsonDecoder[A]]): JsonDecoder[A] = new JsonDecoder[A] {

    override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
      val c = in.nextNonWhitespace()
      if (partialFunction.isDefinedAt(c)) {
        in.retract()
        partialFunction(c).unsafeDecode(trace, in)
      } else {
        throw UnsafeJson(JsonError.Message(s"missing case in `peekChar` for '${c}''") :: trace)
      }
    }
  }

  def suspend[A](decoder0: => JsonDecoder[A]): JsonDecoder[A] =
    new JsonDecoder[A] {
      lazy val decoder = decoder0

      override def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
        decoder.unsafeDecode(trace, in)

      override def unsafeDecodeMissing(trace: List[JsonError]): A = decoder.unsafeDecodeMissing(trace)

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = decoder.unsafeFromJsonAST(trace, json)
    }

  implicit val string: JsonDecoder[String] = new JsonDecoder[String] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): String =
      Lexer.string(trace, in).toString

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): String =
      json match {
        case Json.Str(value) => value
        case _               => throw UnsafeJson(JsonError.Message("Not a string value") :: trace)
      }
  }

  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder[Boolean] {

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Boolean =
      Lexer.boolean(trace, in)

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Boolean =
      json match {
        case Json.Bool(value) => value
        case _                => throw UnsafeJson(JsonError.Message("Not a bool value") :: trace)
      }
  }

  implicit val char: JsonDecoder[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case _                      => Left("expected one character")
  }
  implicit val symbol: JsonDecoder[Symbol] = string.map(Symbol(_))

  implicit val byte: JsonDecoder[Byte]                       = number(Lexer.byte, _.byteValueExact())
  implicit val short: JsonDecoder[Short]                     = number(Lexer.short, _.shortValueExact())
  implicit val int: JsonDecoder[Int]                         = number(Lexer.int, _.intValueExact())
  implicit val long: JsonDecoder[Long]                       = number(Lexer.long, _.longValueExact())
  implicit val bigInteger: JsonDecoder[java.math.BigInteger] = number(Lexer.bigInteger, _.toBigIntegerExact)
  implicit val scalaBigInt: JsonDecoder[BigInt]              = bigInteger.map(x => x)
  implicit val float: JsonDecoder[Float]                     = number(Lexer.float, _.floatValue())
  implicit val double: JsonDecoder[Double]                   = number(Lexer.double, _.doubleValue())
  implicit val bigDecimal: JsonDecoder[java.math.BigDecimal] = number(Lexer.bigDecimal, identity)
  implicit val scalaBigDecimal: JsonDecoder[BigDecimal]      = bigDecimal.map(x => x)

  // numbers decode from numbers or strings for maximum compatibility
  private[this] def number[A](
    f: (List[JsonError], RetractReader) => A,
    fromBigDecimal: java.math.BigDecimal => A
  ): JsonDecoder[A] =
    new JsonDecoder[A] {

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

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
        json match {
          case Json.Num(value) =>
            try fromBigDecimal(value)
            catch {
              case exception: ArithmeticException => throw UnsafeJson(JsonError.Message(exception.getMessage) :: trace)
            }
          case Json.Str(value) =>
            val reader = new FastStringReader(value)
            try f(List.empty, reader)
            finally reader.close()
          case _ => throw UnsafeJson(JsonError.Message("Not a number or a string") :: trace)
        }
    }

  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.
  implicit def option[A](implicit A: JsonDecoder[A]): JsonDecoder[Option[A]] =
    new JsonDecoder[Option[A]] { self =>
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

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Option[A] =
        json match {
          case Json.Null => None
          case _         => Some(A.unsafeFromJsonAST(trace, json))
        }
    }

  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  implicit def either[A, B](implicit
    A: JsonDecoder[A],
    B: JsonDecoder[B]
  ): JsonDecoder[Either[A, B]] =
    new JsonDecoder[Either[A, B]] {

      val names: Array[String] =
        Array("a", "Left", "left", "b", "Right", "right")
      val matrix: StringMatrix    = new StringMatrix(names)
      val spans: Array[JsonError] = names.map(JsonError.ObjectAccess)

      def unsafeDecode(
        trace: List[JsonError],
        in: RetractReader
      ): Either[A, B] = {
        Lexer.char(trace, in, '{')

        val values: Array[Any] = Array.ofDim(2)

        if (Lexer.firstField(trace, in))
          while ({
            {
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
            }; Lexer.nextField(trace, in)
          }) ()

        if (values(0) == null && values(1) == null)
          throw UnsafeJson(JsonError.Message("missing fields") :: trace)
        if (values(0) != null && values(1) != null)
          throw UnsafeJson(
            JsonError.Message("ambiguous either, zip present") :: trace
          )
        if (values(0) != null)
          Left(values(0).asInstanceOf[A])
        else Right(values(1).asInstanceOf[B])
      }
    }

  private[json] def builder[A, T[_]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[A, T[A]]
  )(implicit A: JsonDecoder[A]): T[A] = {
    Lexer.char(trace, in, '[')
    var i: Int = 0
    if (Lexer.firstArrayElement(in)) while ({
      {
        val trace_ = JsonError.ArrayAccess(i) :: trace
        builder += A.unsafeDecode(trace_, in)
        i += 1
      }; Lexer.nextArrayElement(trace, in)
    }) ()
    builder.result()
  }

  private[json] def keyValueBuilder[K, V, T[X, Y] <: Iterable[(X, Y)]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[(K, V), T[K, V]]
  )(implicit K: JsonFieldDecoder[K], V: JsonDecoder[V]): T[K, V] = {
    Lexer.char(trace, in, '{')
    if (Lexer.firstField(trace, in))
      while ({
        {
          val field  = Lexer.string(trace, in).toString
          val trace_ = JsonError.ObjectAccess(field) :: trace
          Lexer.char(trace_, in, ':')
          val value = V.unsafeDecode(trace_, in)
          builder += ((K.unsafeDecodeField(trace_, field), value))
        }; Lexer.nextField(trace, in)
      }) ()
    builder.result()
  }

  // use this instead of `string.mapOrFail` in supertypes (to prevent class initialization error at runtime)
  private[json] def mapStringOrFail[A](f: String => Either[String, A]): JsonDecoder[A] =
    new JsonDecoder[A] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
        f(string.unsafeDecode(trace, in)) match {
          case Left(err)    => throw UnsafeJson(JsonError.Message(err) :: trace)
          case Right(value) => value
        }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
        f(string.unsafeFromJsonAST(trace, json)) match {
          case Left(err)    => throw UnsafeJson(JsonError.Message(err) :: trace)
          case Right(value) => value
        }
    }
}

private[json] trait DecoderLowPriority1 extends DecoderLowPriority2 {
  this: JsonDecoder.type =>

  implicit def array[A: JsonDecoder: reflect.ClassTag]: JsonDecoder[Array[A]] =
    new JsonDecoder[Array[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Array[A] = Array.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Array[A] =
        builder(trace, in, Array.newBuilder[A])
    }

  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] =
    new JsonDecoder[Seq[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Seq[A] =
        Seq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Seq[A] =
        builder(trace, in, immutable.Seq.newBuilder[A])
    }

  implicit def chunk[A: JsonDecoder]: JsonDecoder[Chunk[A]] =
    new JsonDecoder[Chunk[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Chunk[A] = Chunk.empty

      val decoder = JsonDecoder[A]
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[A] =
        builder(trace, in, zio.ChunkBuilder.make[A]())

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Chunk[A] =
        json match {
          case Json.Arr(elements) =>
            elements.zipWithIndex.map { case (json, i) =>
              decoder.unsafeFromJsonAST(JsonError.ArrayAccess(i) :: trace, json)
            }
          case _ => throw UnsafeJson(JsonError.Message("Not an array") :: trace)
        }
    }

  implicit def nonEmptyChunk[A: JsonDecoder]: JsonDecoder[NonEmptyChunk[A]] =
    chunk[A].mapOrFail(NonEmptyChunk.fromChunk(_).toRight("Chunk was empty"))

  implicit def indexedSeq[A: JsonDecoder]: JsonDecoder[IndexedSeq[A]] =
    new JsonDecoder[IndexedSeq[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): IndexedSeq[A] = IndexedSeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): IndexedSeq[A] =
        builder(trace, in, IndexedSeq.newBuilder[A])
    }

  implicit def linearSeq[A: JsonDecoder]: JsonDecoder[immutable.LinearSeq[A]] =
    new JsonDecoder[immutable.LinearSeq[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.LinearSeq[A] =
        immutable.LinearSeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): LinearSeq[A] =
        builder(trace, in, immutable.LinearSeq.newBuilder[A])
    }

  implicit def listSet[A: JsonDecoder]: JsonDecoder[immutable.ListSet[A]] =
    new JsonDecoder[immutable.ListSet[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ListSet[A] =
        immutable.ListSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): ListSet[A] =
        builder(trace, in, immutable.ListSet.newBuilder[A])
    }

  implicit def treeSet[A: JsonDecoder: Ordering]: JsonDecoder[immutable.TreeSet[A]] =
    new JsonDecoder[immutable.TreeSet[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.TreeSet[A] =
        immutable.TreeSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): TreeSet[A] =
        builder(trace, in, immutable.TreeSet.newBuilder[A])
    }

  implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] =
    new JsonDecoder[List[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): List[A] = List.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): List[A] =
        builder(trace, in, new mutable.ListBuffer[A])
    }

  implicit def vector[A: JsonDecoder]: JsonDecoder[Vector[A]] =
    new JsonDecoder[Vector[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Vector[A] =
        Vector.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Vector[A] =
        builder(trace, in, immutable.Vector.newBuilder[A])
    }

  implicit def set[A: JsonDecoder]: JsonDecoder[Set[A]] =
    new JsonDecoder[Set[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Set[A] = Set.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Set[A] =
        builder(trace, in, Set.newBuilder[A])
    }

  implicit def hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] =
    new JsonDecoder[immutable.HashSet[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.HashSet[A] =
        immutable.HashSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.HashSet[A] =
        builder(trace, in, immutable.HashSet.newBuilder[A])
    }

  implicit def map[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[Map[K, V]] =
    new JsonDecoder[Map[K, V]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Map[K, V] = Map.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Map[K, V] =
        keyValueBuilder(trace, in, Map.newBuilder[K, V])
    }

  implicit def hashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    new JsonDecoder[immutable.HashMap[K, V]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.HashMap[K, V] =
        immutable.HashMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.HashMap[K, V] =
        keyValueBuilder(trace, in, immutable.HashMap.newBuilder[K, V])
    }

  implicit def mutableMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[mutable.Map[K, V]] =
    new JsonDecoder[mutable.Map[K, V]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): mutable.Map[K, V] = mutable.Map.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): mutable.Map[K, V] =
        keyValueBuilder(trace, in, mutable.Map.newBuilder[K, V])
    }

  implicit def sortedSet[A: Ordering: JsonDecoder]: JsonDecoder[immutable.SortedSet[A]] =
    new JsonDecoder[immutable.SortedSet[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.SortedSet[A] =
        immutable.SortedSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.SortedSet[A] =
        builder(trace, in, immutable.SortedSet.newBuilder[A])
    }

  implicit def sortedMap[K: JsonFieldDecoder: Ordering, V: JsonDecoder]: JsonDecoder[collection.SortedMap[K, V]] =
    new JsonDecoder[collection.SortedMap[K, V]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): collection.SortedMap[K, V] =
        collection.SortedMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): collection.SortedMap[K, V] =
        keyValueBuilder(trace, in, collection.SortedMap.newBuilder[K, V])
    }

  implicit def listMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.ListMap[K, V]] =
    new JsonDecoder[immutable.ListMap[K, V]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ListMap[K, V] =
        immutable.ListMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.ListMap[K, V] =
        keyValueBuilder(trace, in, immutable.ListMap.newBuilder[K, V])
    }
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
private[json] trait DecoderLowPriority2 extends DecoderLowPriority3 {
  this: JsonDecoder.type =>

  implicit def iterable[A: JsonDecoder]: JsonDecoder[Iterable[A]] =
    new JsonDecoder[Iterable[A]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Iterable[A] = Iterable.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Iterable[A] =
        builder(trace, in, immutable.Iterable.newBuilder[A])
    }

  // not implicit because this overlaps with decoders for lists of tuples
  def keyValueChunk[K, A](implicit
    K: JsonFieldDecoder[K],
    A: JsonDecoder[A]
  ): JsonDecoder[Chunk[(K, A)]] =
    new JsonDecoder[Chunk[(K, A)]] {

      override def unsafeDecodeMissing(trace: List[JsonError]): Chunk[(K, A)] =
        Chunk.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[(K, A)] =
        keyValueBuilder[K, A, ({ type lambda[X, Y] = Chunk[(X, Y)] })#lambda](
          trace,
          in,
          zio.ChunkBuilder.make[(K, A)]()
        )
    }

}

private[json] trait DecoderLowPriority3 extends DecoderLowPriority4 {
  this: JsonDecoder.type =>

  import java.time.{ DateTimeException, _ }
  import java.time.format.DateTimeParseException
  import java.time.zone.ZoneRulesException

  implicit val dayOfWeek: JsonDecoder[DayOfWeek] = mapStringOrFail(s => parseJavaTime(DayOfWeek.valueOf, s.toUpperCase))
  implicit val duration: JsonDecoder[Duration]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseDuration, _))
  implicit val instant: JsonDecoder[Instant]     = mapStringOrFail(parseJavaTime(parsers.unsafeParseInstant, _))
  implicit val localDate: JsonDecoder[LocalDate] = mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalDate, _))

  implicit val localDateTime: JsonDecoder[LocalDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalDateTime, _))

  implicit val localTime: JsonDecoder[LocalTime] = mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalTime, _))
  implicit val month: JsonDecoder[Month]         = mapStringOrFail(s => parseJavaTime(Month.valueOf, s.toUpperCase))
  implicit val monthDay: JsonDecoder[MonthDay]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseMonthDay, _))

  implicit val offsetDateTime: JsonDecoder[OffsetDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseOffsetDateTime, _))

  implicit val offsetTime: JsonDecoder[OffsetTime] = mapStringOrFail(parseJavaTime(parsers.unsafeParseOffsetTime, _))
  implicit val period: JsonDecoder[Period]         = mapStringOrFail(parseJavaTime(parsers.unsafeParsePeriod, _))
  implicit val year: JsonDecoder[Year]             = mapStringOrFail(parseJavaTime(parsers.unsafeParseYear, _))
  implicit val yearMonth: JsonDecoder[YearMonth]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseYearMonth, _))

  implicit val zonedDateTime: JsonDecoder[ZonedDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseZonedDateTime, _))

  implicit val zoneId: JsonDecoder[ZoneId]         = mapStringOrFail(parseJavaTime(parsers.unsafeParseZoneId, _))
  implicit val zoneOffset: JsonDecoder[ZoneOffset] = mapStringOrFail(parseJavaTime(parsers.unsafeParseZoneOffset, _))

  // Commonized handling for decoding from string to java.time Class
  private[json] def parseJavaTime[A](f: String => A, s: String): Either[String, A] =
    try {
      Right(f(s))
    } catch {
      case zre: ZoneRulesException      => Left(s"$s is not a valid ISO-8601 format, ${zre.getMessage}")
      case dtpe: DateTimeParseException => Left(s"$s is not a valid ISO-8601 format, ${dtpe.getMessage}")
      case dte: DateTimeException       => Left(s"$s is not a valid ISO-8601 format, ${dte.getMessage}")
      case ex: Exception                => Left(ex.getMessage)
    }

  implicit val uuid: JsonDecoder[UUID] =
    mapStringOrFail { str =>
      try {
        Right(UUIDParser.unsafeParse(str))
      } catch {
        case iae: IllegalArgumentException => Left(s"Invalid UUID: ${iae.getMessage}")
      }
    }

  implicit val currency: JsonDecoder[java.util.Currency] =
    mapStringOrFail { str =>
      try {
        Right(java.util.Currency.getInstance(str))
      } catch {
        case iae: IllegalArgumentException => Left(s"Invalid Currency: ${iae.getMessage}")
      }
    }
}

private[json] trait DecoderLowPriority4 extends DecoderLowPriorityVersionSpecific {
  implicit def fromCodec[A](implicit codec: JsonCodec[A]): JsonDecoder[A] = codec.decoder
}
