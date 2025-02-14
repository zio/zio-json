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

  final def both[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = bothWith(that)((a, b) => (a, b))

  final def bothRight[B](that: => JsonDecoder[B]): JsonDecoder[B] = bothWith(that)((_, b) => b)

  final def bothLeft[B](that: => JsonDecoder[B]): JsonDecoder[A] = bothWith(that)((a, _) => a)

  final def bothWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] =
    new JsonDecoder[C] {
      override def unsafeDecode(trace: List[JsonError], in: RetractReader): C = {
        val rr = RecordingReader(in)
        val a  = self.unsafeDecode(trace, rr)
        rr.rewind()
        val b = that.unsafeDecode(trace, rr)
        f(a, b)
      }
    }

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
        val rr = RecordingReader(in)
        try self.unsafeDecode(trace, rr)
        catch {
          case _: JsonDecoder.UnsafeJson | _: UnexpectedEnd =>
            rr.rewind()
            that.unsafeDecode(trace, rr)
        }
      }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A1 =
        try self.unsafeFromJsonAST(trace, json)
        catch {
          case _: JsonDecoder.UnsafeJson | _: UnexpectedEnd => that.unsafeFromJsonAST(trace, json)
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
    self.map(new Left(_)).orElse(that.map(new Right(_)))

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function.
   */
  final def map[B](f: A => B): JsonDecoder[B] =
    new MappedJsonDecoder[B] {
      private[json] def underlying: JsonDecoder[A] = self

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B = f(self.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): B = f(
        self.unsafeFromJsonAST(trace, json)
      )

      override def unsafeDecodeMissing(trace: List[JsonError]): B = f(self.unsafeDecodeMissing(trace))
    }

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function, which may itself decide to fail
   * with some type of error.
   */
  final def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B] =
    new MappedJsonDecoder[B] {
      private[json] def underlying: JsonDecoder[A] = self

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in)) match {
          case Right(b)  => b
          case Left(err) => Lexer.error(err, trace)
        }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): B =
        f(self.unsafeFromJsonAST(trace, json)) match {
          case Right(b)  => b
          case Left(err) => Lexer.error(err, trace)
        }

      override def unsafeDecodeMissing(trace: List[JsonError]): B =
        f(self.unsafeDecodeMissing(trace)) match {
          case Right(b)  => b
          case Left(err) => Lexer.error(err, trace)
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
  final def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] = self.zip(that).map(f.tupled)

  def unsafeDecodeMissing(trace: List[JsonError]): A = Lexer.error("missing", trace)

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
      } else Lexer.error(s"missing case in `peekChar` for '${c}''", trace)
    }
  }

  def suspend[A](decoder0: => JsonDecoder[A]): JsonDecoder[A] =
    new MappedJsonDecoder[A] {
      private[json] def underlying: JsonDecoder[A] = decoder0

      lazy val decoder = decoder0

      override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = decoder.unsafeDecode(trace, in)

      override def unsafeDecodeMissing(trace: List[JsonError]): A = decoder.unsafeDecodeMissing(trace)

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = decoder.unsafeFromJsonAST(trace, json)
    }

  implicit val string: JsonDecoder[String] = new JsonDecoder[String] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): String = Lexer.string(trace, in).toString

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): String =
      json match {
        case s: Json.Str => s.value
        case _           => Lexer.error("expected string", trace)
      }
  }

  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder[Boolean] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Boolean = Lexer.boolean(trace, in)

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Boolean =
      json match {
        case b: Json.Bool => b.value
        case _            => Lexer.error("expected boolean", trace)
      }
  }

  implicit val char: JsonDecoder[Char] = new JsonDecoder[Char] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Char = Lexer.char(trace, in)

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Char =
      json match {
        case s: Json.Str if s.value.length == 1 => s.value.charAt(0)
        case _                                  => Lexer.error("expected single character string", trace)
      }
  }

  implicit val symbol: JsonDecoder[Symbol] = string.map(Symbol(_))

  implicit val byte: JsonDecoder[Byte] = new JsonDecoder[Byte] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Byte =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.byte(trace, in)
      } else {
        val a = Lexer.byte(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Byte =
      json match {
        case n: Json.Num =>
          try n.value.byteValueExact
          catch {
            case ex: ArithmeticException => Lexer.error(ex.getMessage, trace)
          }
        case s: Json.Str => Lexer.byte(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }

  implicit val short: JsonDecoder[Short] = new JsonDecoder[Short] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Short =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.short(trace, in)
      } else {
        val a = Lexer.short(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Short =
      json match {
        case n: Json.Num =>
          try n.value.shortValueExact
          catch {
            case ex: ArithmeticException => Lexer.error(ex.getMessage, trace)
          }
        case s: Json.Str => Lexer.short(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }

  implicit val int: JsonDecoder[Int] = new JsonDecoder[Int] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Int =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.int(trace, in)
      } else {
        val a = Lexer.int(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Int =
      json match {
        case n: Json.Num =>
          try n.value.intValueExact
          catch {
            case ex: ArithmeticException => Lexer.error(ex.getMessage, trace)
          }
        case s: Json.Str => Lexer.int(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  implicit val long: JsonDecoder[Long] = new JsonDecoder[Long] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Long =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.long(trace, in)
      } else {
        val a = Lexer.long(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Long =
      json match {
        case n: Json.Num =>
          try n.value.longValueExact
          catch {
            case ex: ArithmeticException => Lexer.error(ex.getMessage, trace)
          }
        case s: Json.Str => Lexer.long(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }

  implicit val bigInteger: JsonDecoder[java.math.BigInteger] = new JsonDecoder[java.math.BigInteger] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): java.math.BigInteger =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.bigInteger(trace, in)
      } else {
        val a = Lexer.bigInteger(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): java.math.BigInteger =
      json match {
        case n: Json.Num =>
          try n.value.toBigIntegerExact
          catch {
            case ex: ArithmeticException => Lexer.error(ex.getMessage, trace)
          }
        case s: Json.Str => Lexer.bigInteger(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  implicit val scalaBigInt: JsonDecoder[BigInt] = new JsonDecoder[BigInt] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): BigInt =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.bigInt(trace, in)
      } else {
        val a = Lexer.bigInt(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): BigInt =
      json match {
        case n: Json.Num =>
          try BigInt(n.value.toBigIntegerExact)
          catch {
            case ex: ArithmeticException => Lexer.error(ex.getMessage, trace)
          }
        case s: Json.Str => Lexer.bigInt(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  implicit val float: JsonDecoder[Float] = new JsonDecoder[Float] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Float =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.float(trace, in)
      } else {
        val a = Lexer.float(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Float =
      json match {
        case n: Json.Num => n.value.floatValue
        case s: Json.Str => Lexer.float(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  implicit val double: JsonDecoder[Double] = new JsonDecoder[Double] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Double =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.double(trace, in)
      } else {
        val a = Lexer.double(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Double =
      json match {
        case n: Json.Num => n.value.doubleValue
        case s: Json.Str => Lexer.double(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  implicit val bigDecimal: JsonDecoder[java.math.BigDecimal] = new JsonDecoder[java.math.BigDecimal] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): java.math.BigDecimal =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.bigDecimal(trace, in)
      } else {
        val a = Lexer.bigDecimal(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): java.math.BigDecimal =
      json match {
        case n: Json.Num => n.value
        case s: Json.Str => Lexer.bigDecimal(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  implicit val scalaBigDecimal: JsonDecoder[BigDecimal] = new JsonDecoder[BigDecimal] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): BigDecimal =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.bigDecimal(trace, in)
      } else {
        val a = Lexer.bigDecimal(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error("'\"'", c, trace)
        a
      }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): BigDecimal =
      json match {
        case n: Json.Num => new BigDecimal(n.value, BigDecimal.defaultMathContext)
        case s: Json.Str => Lexer.bigDecimal(trace, new FastStringReader(s.value))
        case _           => Lexer.error("expected number", trace)
      }
  }
  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.

  implicit def option[A](implicit A: JsonDecoder[A]): JsonDecoder[Option[A]] =
    new OptionJsonDecoder[Option[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Option[A] = None

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[A] =
        if (in.nextNonWhitespace() == 'n') {
          if (in.readChar() != 'u' || in.readChar() != 'l' || in.readChar() != 'l') {
            Lexer.error("expected 'null'", trace)
          }
          None
        } else {
          in.retract()
          new Some(A.unsafeDecode(trace, in))
        }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Option[A] =
        if (json eq Json.Null) None
        else new Some(A.unsafeFromJsonAST(trace, json))
    }

  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  implicit def either[A, B](implicit A: JsonDecoder[A], B: JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    new JsonDecoder[Either[A, B]] {
      private[this] val names  = Array("a", "Left", "left", "b", "Right", "right")
      private[this] val matrix = new StringMatrix(names)
      private[this] val spans  = names.map(new JsonError.ObjectAccess(_))

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Either[A, B] = {
        val c = in.nextNonWhitespace()
        if (c != '{') Lexer.error("'{'", c, trace)
        var left: Any  = null
        var right: Any = null
        if (Lexer.firstField(trace, in))
          while ({
            val field = Lexer.field(trace, in, matrix)
            if (field == -1) Lexer.skipValue(trace, in)
            else {
              val trace_ = spans(field) :: trace
              if (field < 3) {
                if (left != null) Lexer.error("duplicate", trace_)
                left = A.unsafeDecode(trace_, in)
              } else {
                if (right != null) Lexer.error("duplicate", trace_)
                right = B.unsafeDecode(trace_, in)
              }
            }
            Lexer.nextField(trace, in)
          }) ()
        if (left == null && right == null) Lexer.error("missing fields", trace)
        if (left != null && right != null) Lexer.error("ambiguous either, zip present", trace)
        if (left != null) new Left(left.asInstanceOf[A])
        else new Right(right.asInstanceOf[B])
      }
    }

  @inline private[json] def builder[A, T[_]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[A, T[A]]
  )(implicit A: JsonDecoder[A]): T[A] = {
    val c = in.nextNonWhitespace()
    if (c != '[') Lexer.error("'['", c, trace)
    var i: Int = 0
    if (Lexer.firstArrayElement(in)) while ({
      builder += A.unsafeDecode(new JsonError.ArrayAccess(i) :: trace, in)
      i += 1
      Lexer.nextArrayElement(trace, in)
    }) ()
    builder.result()
  }

  @inline private[json] def keyValueBuilder[K, V, T[X, Y] <: Iterable[(X, Y)]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[(K, V), T[K, V]]
  )(implicit K: JsonFieldDecoder[K], V: JsonDecoder[V]): T[K, V] = {
    var c = in.nextNonWhitespace()
    if (c != '{') Lexer.error("'{'", c, trace)
    if (Lexer.firstField(trace, in))
      while ({
        val field  = Lexer.string(trace, in).toString
        val trace_ = new JsonError.ObjectAccess(field) :: trace
        c = in.nextNonWhitespace()
        if (c != ':') Lexer.error("':'", c, trace)
        val value = V.unsafeDecode(trace_, in)
        builder += ((K.unsafeDecodeField(trace_, field), value))
        Lexer.nextField(trace, in)
      }) ()
    builder.result()
  }

  // FIXME: remove in the next major version
  private[json] def mapStringOrFail[A](f: String => Either[String, A]): JsonDecoder[A] =
    new JsonDecoder[A] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
        f(string.unsafeDecode(trace, in)) match {
          case Right(value) => value
          case Left(err)    => Lexer.error(err, trace)
        }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
        f(string.unsafeFromJsonAST(trace, json)) match {
          case Right(value) => value
          case Left(err)    => Lexer.error(err, trace)
        }
    }
}

private[json] trait CollectionJsonDecoder[A] extends JsonDecoder[A]
private[json] trait OptionJsonDecoder[A]     extends JsonDecoder[A]
private[json] trait MappedJsonDecoder[A] extends JsonDecoder[A] {
  private[json] def underlying: JsonDecoder[_]
}

private[json] trait DecoderLowPriority1 extends DecoderLowPriority2 {
  this: JsonDecoder.type =>

  implicit def array[A](implicit A: JsonDecoder[A], classTag: reflect.ClassTag[A]): JsonDecoder[Array[A]] =
    new CollectionJsonDecoder[Array[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Array[A] = Array.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Array[A] = {
        val c = in.nextNonWhitespace()
        if (c != '[') Lexer.error("'['", c, trace)
        if (Lexer.firstArrayElement(in)) {
          var l = 8
          var x = new Array[A](l)
          var i = 0
          while ({
            if (i == l) {
              l <<= 1
              val x1 = new Array[A](l)
              System.arraycopy(x, 0, x1, 0, i)
              x = x1
            }
            x(i) = A.unsafeDecode(new JsonError.ArrayAccess(i) :: trace, in)
            i += 1
            Lexer.nextArrayElement(trace, in)
          }) ()
          if (i != l) {
            val x1 = new Array[A](i)
            _root_.java.lang.System.arraycopy(x, 0, x1, 0, i)
            x = x1
          }
          x
        } else Array.empty
      }
    }

  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] =
    new CollectionJsonDecoder[Seq[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Seq[A] = Seq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Seq[A] =
        builder(trace, in, immutable.Seq.newBuilder[A])
    }

  implicit def chunk[A: JsonDecoder]: JsonDecoder[Chunk[A]] =
    new CollectionJsonDecoder[Chunk[A]] {
      private[this] val decoder = JsonDecoder[A]

      override def unsafeDecodeMissing(trace: List[JsonError]): Chunk[A] = Chunk.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[A] =
        builder(trace, in, zio.ChunkBuilder.make[A]())

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Chunk[A] =
        json match {
          case a: Json.Arr =>
            a.elements.map {
              var i = 0
              json =>
                val span = new JsonError.ArrayAccess(i)
                i += 1
                decoder.unsafeFromJsonAST(span :: trace, json)
            }
          case _ => Lexer.error("Not an array", trace)
        }
    }

  implicit def nonEmptyChunk[A: JsonDecoder]: JsonDecoder[NonEmptyChunk[A]] =
    chunk[A].mapOrFail(NonEmptyChunk.fromChunk(_).toRight("Chunk was empty"))

  implicit def indexedSeq[A: JsonDecoder]: JsonDecoder[IndexedSeq[A]] =
    new CollectionJsonDecoder[IndexedSeq[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): IndexedSeq[A] = IndexedSeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): IndexedSeq[A] =
        builder(trace, in, IndexedSeq.newBuilder[A])
    }

  implicit def linearSeq[A: JsonDecoder]: JsonDecoder[immutable.LinearSeq[A]] =
    new CollectionJsonDecoder[immutable.LinearSeq[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.LinearSeq[A] = immutable.LinearSeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): LinearSeq[A] =
        builder(trace, in, immutable.LinearSeq.newBuilder[A])
    }

  implicit def listSet[A: JsonDecoder]: JsonDecoder[immutable.ListSet[A]] =
    new CollectionJsonDecoder[immutable.ListSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ListSet[A] = immutable.ListSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): ListSet[A] =
        builder(trace, in, immutable.ListSet.newBuilder[A])
    }

  implicit def treeSet[A: JsonDecoder: Ordering]: JsonDecoder[immutable.TreeSet[A]] =
    new CollectionJsonDecoder[immutable.TreeSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.TreeSet[A] = immutable.TreeSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): TreeSet[A] =
        builder(trace, in, immutable.TreeSet.newBuilder[A])
    }

  implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] =
    new CollectionJsonDecoder[List[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): List[A] = List.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): List[A] =
        builder(trace, in, new mutable.ListBuffer[A])
    }

  implicit def vector[A: JsonDecoder]: JsonDecoder[Vector[A]] =
    new CollectionJsonDecoder[Vector[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Vector[A] = Vector.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Vector[A] =
        builder(trace, in, immutable.Vector.newBuilder[A])
    }

  implicit def set[A: JsonDecoder]: JsonDecoder[Set[A]] =
    new CollectionJsonDecoder[Set[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Set[A] = Set.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Set[A] =
        builder(trace, in, Set.newBuilder[A])
    }

  implicit def hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] =
    new CollectionJsonDecoder[immutable.HashSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.HashSet[A] = immutable.HashSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.HashSet[A] =
        builder(trace, in, immutable.HashSet.newBuilder[A])
    }

  implicit def map[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[Map[K, V]] =
    new CollectionJsonDecoder[Map[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Map[K, V] = Map.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Map[K, V] =
        keyValueBuilder(trace, in, Map.newBuilder[K, V])
    }

  implicit def hashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    new CollectionJsonDecoder[immutable.HashMap[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.HashMap[K, V] = immutable.HashMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.HashMap[K, V] =
        keyValueBuilder(trace, in, immutable.HashMap.newBuilder[K, V])
    }

  implicit def mutableMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[mutable.Map[K, V]] =
    new CollectionJsonDecoder[mutable.Map[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): mutable.Map[K, V] = mutable.Map.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): mutable.Map[K, V] =
        keyValueBuilder(trace, in, mutable.Map.newBuilder[K, V])
    }

  implicit def sortedSet[A: Ordering: JsonDecoder]: JsonDecoder[immutable.SortedSet[A]] =
    new CollectionJsonDecoder[immutable.SortedSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.SortedSet[A] = immutable.SortedSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.SortedSet[A] =
        builder(trace, in, immutable.SortedSet.newBuilder[A])
    }

  implicit def sortedMap[K: JsonFieldDecoder: Ordering, V: JsonDecoder]: JsonDecoder[collection.SortedMap[K, V]] =
    new CollectionJsonDecoder[collection.SortedMap[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): collection.SortedMap[K, V] = collection.SortedMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): collection.SortedMap[K, V] =
        keyValueBuilder(trace, in, collection.SortedMap.newBuilder[K, V])
    }

  implicit def listMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.ListMap[K, V]] =
    new CollectionJsonDecoder[immutable.ListMap[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ListMap[K, V] = immutable.ListMap.empty

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
    new CollectionJsonDecoder[Iterable[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Iterable[A] = Iterable.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Iterable[A] =
        builder(trace, in, immutable.Iterable.newBuilder[A])
    }

  // not implicit because this overlaps with decoders for lists of tuples
  def keyValueChunk[K, A](implicit K: JsonFieldDecoder[K], A: JsonDecoder[A]): JsonDecoder[Chunk[(K, A)]] =
    new CollectionJsonDecoder[Chunk[(K, A)]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Chunk[(K, A)] = Chunk.empty

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

  import java.time._

  implicit val dayOfWeek: JsonDecoder[DayOfWeek]           = javaTimeDecoder(s => DayOfWeek.valueOf(s.toUpperCase))
  implicit val duration: JsonDecoder[Duration]             = javaTimeDecoder(parsers.unsafeParseDuration)
  implicit val instant: JsonDecoder[Instant]               = javaTimeDecoder(parsers.unsafeParseInstant)
  implicit val localDate: JsonDecoder[LocalDate]           = javaTimeDecoder(parsers.unsafeParseLocalDate)
  implicit val localDateTime: JsonDecoder[LocalDateTime]   = javaTimeDecoder(parsers.unsafeParseLocalDateTime)
  implicit val localTime: JsonDecoder[LocalTime]           = javaTimeDecoder(parsers.unsafeParseLocalTime)
  implicit val month: JsonDecoder[Month]                   = javaTimeDecoder(s => Month.valueOf(s.toUpperCase))
  implicit val monthDay: JsonDecoder[MonthDay]             = javaTimeDecoder(parsers.unsafeParseMonthDay)
  implicit val offsetDateTime: JsonDecoder[OffsetDateTime] = javaTimeDecoder(parsers.unsafeParseOffsetDateTime)
  implicit val offsetTime: JsonDecoder[OffsetTime]         = javaTimeDecoder(parsers.unsafeParseOffsetTime)
  implicit val period: JsonDecoder[Period]                 = javaTimeDecoder(parsers.unsafeParsePeriod)
  implicit val year: JsonDecoder[Year]                     = javaTimeDecoder(parsers.unsafeParseYear)
  implicit val yearMonth: JsonDecoder[YearMonth]           = javaTimeDecoder(parsers.unsafeParseYearMonth)
  implicit val zonedDateTime: JsonDecoder[ZonedDateTime]   = javaTimeDecoder(parsers.unsafeParseZonedDateTime)
  implicit val zoneId: JsonDecoder[ZoneId]                 = javaTimeDecoder(parsers.unsafeParseZoneId)
  implicit val zoneOffset: JsonDecoder[ZoneOffset]         = javaTimeDecoder(parsers.unsafeParseZoneOffset)

  private[this] def javaTimeDecoder[A](f: String => A): JsonDecoder[A] = new JsonDecoder[A] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
      parseJavaTime(trace, Lexer.string(trace, in).toString)

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
      json match {
        case s: Json.Str => parseJavaTime(trace, s.value)
        case _           => Lexer.error("expected string", trace)
      }

    // Commonized handling for decoding from string to java.time Class
    @inline private[this] def parseJavaTime(trace: List[JsonError], s: String): A =
      try f(s)
      catch {
        case ex: DateTimeException =>
          Lexer.error(s"${strip(s)} is not a valid ISO-8601 format, ${ex.getMessage}", trace)
        case _: IllegalArgumentException =>
          Lexer.error(s"${strip(s)} is not a valid ISO-8601 format", trace)
      }
  }

  // FIXME: remove in the next major version
  private[json] def parseJavaTime[A](f: String => A, s: String): Either[String, A] =
    try new Right(f(s))
    catch {
      case ex: DateTimeException =>
        new Left(s"${strip(s)} is not a valid ISO-8601 format, ${ex.getMessage}")
      case _: IllegalArgumentException =>
        new Left(s"${strip(s)} is not a valid ISO-8601 format")
    }

  implicit val uuid: JsonDecoder[UUID] = new JsonDecoder[UUID] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): UUID =
      parseUUID(trace, Lexer.string(trace, in).toString)

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): UUID =
      json match {
        case s: Json.Str => parseUUID(trace, s.value)
        case _           => Lexer.error("expected string", trace)
      }

    @inline private[this] def parseUUID(trace: List[JsonError], s: String): UUID =
      try UUIDParser.unsafeParse(s)
      catch {
        case _: IllegalArgumentException => Lexer.error(s"Invalid UUID: ${strip(s)}", trace)
      }
  }

  implicit val currency: JsonDecoder[java.util.Currency] = new JsonDecoder[java.util.Currency] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): java.util.Currency =
      parseCurrency(trace, Lexer.string(trace, in).toString)

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): java.util.Currency =
      json match {
        case s: Json.Str => parseCurrency(trace, s.value)
        case _           => Lexer.error("expected string", trace)
      }

    @inline private[this] def parseCurrency(trace: List[JsonError], s: String): java.util.Currency =
      try java.util.Currency.getInstance(s)
      catch {
        case _: IllegalArgumentException => Lexer.error(s"Invalid Currency: ${strip(s)}", trace)
      }
  }

  @noinline private[json] def strip(s: String, len: Int = 50): String =
    if (s.length <= len) s
    else s.substring(0, len) + "..."
}

private[json] trait DecoderLowPriority4 extends DecoderLowPriorityVersionSpecific {
  @inline implicit def fromCodec[A](implicit codec: JsonCodec[A]): JsonDecoder[A] = codec.decoder
}
