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
import zio.json.internal.Lexer.NumberMaxBits
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
 *
 * Most code calls `fromJson` from `import zio.json._` and lets the compiler find the decoder. The companion object has
 * instances for the standard types, `DeriveJsonDecoder.gen` derives one for a case class or sealed trait, and `map`,
 * `mapOrFail`, `orElse` and `zip` build one from another. A failure is a `Left` with the path to the value that failed
 * and a message, for example `.pages(missing)`.
 *
 * {{{
 * final case class Book(title: String, pages: Int)
 *
 * object Book {
 *   implicit val decoder: JsonDecoder[Book] = DeriveJsonDecoder.gen[Book]
 * }
 *
 * val ok     = """{"title":"Dune","pages":412}""".fromJson[Book] // Right(Book(Dune,412))
 * val failed = """{"title":"Dune"}""".fromJson[Book]             // Left(.pages(missing))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/decoding/ Decoding]]
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
   * Returns a decoder that decodes the same JSON value with this decoder and with `that`, and returns both results as a
   * tuple. Use it to read two views of one object.
   *
   * {{{
   * final case class Title(title: String)
   * final case class Pages(pages: Int)
   *
   * val titleDecoder: JsonDecoder[Title] = DeriveJsonDecoder.gen[Title]
   * val pagesDecoder: JsonDecoder[Pages] = DeriveJsonDecoder.gen[Pages]
   *
   * val both = titleDecoder.both(pagesDecoder).decodeJson("""{"title":"Dune","pages":412}""")
   * // Right((Title(Dune),Pages(412)))
   * }}}
   */
  final def both[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = bothWith(that)((a, b) => (a, b))

  /** Like [[JsonDecoder#both]], but keeps only the result of `that`. Both decoders must succeed. */
  final def bothRight[B](that: => JsonDecoder[B]): JsonDecoder[B] = bothWith(that)((_, b) => b)

  /** Like [[JsonDecoder#both]], but keeps only the result of this decoder. Both decoders must succeed. */
  final def bothLeft[B](that: => JsonDecoder[B]): JsonDecoder[A] = bothWith(that)((a, _) => a)

  /**
   * Like [[JsonDecoder#both]], but combines the two results with `f`. The decoder records what this decoder reads and
   * replays it for `that`, so it holds the whole value in memory.
   */
  final def bothWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] =
    new JsonDecoder.AbstractJsonDecoder[C] {
      override def unsafeDecode(trace: List[JsonError], in: RetractReader): C = {
        val rr = RecordingReader(in)
        val a  = self.unsafeDecode(trace, rr)
        rr.rewind()
        val b = that.unsafeDecode(trace, rr)
        f(a, b)
      }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): C = {
        val a = self.unsafeFromJsonAST(trace, json)
        val b = that.unsafeFromJsonAST(trace, json)
        f(a, b)
      }
    }

  /**
   * Attempts to decode a value of type `A` from the specified `CharSequence`, but may fail with a human-readable error
   * message if the provided text does not encode a value of this type.
   *
   * The message is the path to the value that failed followed by the reason, for example `.pages(missing)`, or
   * `Unexpected end of input` when the text ends in the middle of a value. An exception thrown by a function passed to
   * `map` is not caught.
   *
   * Note: This method may not entirely consume the specified character sequence: `"1 garbage"` decodes as `Right(1)`
   * with `JsonDecoder[Int]`.
   */
  final def decodeJson(str: CharSequence): Either[String, A] =
    try new Right(unsafeDecode(Nil, new FastStringReader(str)))
    catch {
      case e: JsonDecoder.UnsafeJson => new Left(JsonError.render(e.trace))
      case _: UnexpectedEnd          => new Left("Unexpected end of input")
      case _: StackOverflowError     => new Left("Unexpected structure")
    }

  /**
   * Attempts to decode a value of type `A` from the specified UTF-8 encoded bytes, but may fail with a human-readable
   * error message if the provided bytes do not encode a value of this type.
   *
   * The bytes are decoded as they are parsed rather than copied into a `String` first, so this is the allocation
   * friendly way to handle a payload that already arrived as bytes, e.g. an HTTP response body.
   *
   * Malformed UTF-8 decodes to the replacement character U+FFFD rather than failing.
   *
   * Note: This method may not entirely consume the specified chunk.
   */
  final def decodeJson(bytes: Chunk[Byte]): Either[String, A] =
    try new Right(unsafeDecode(Nil, new Utf8ChunkReader(bytes)))
    catch {
      case e: JsonDecoder.UnsafeJson => new Left(JsonError.render(e.trace))
      case _: UnexpectedEnd          => new Left("Unexpected end of input")
      case _: StackOverflowError     => new Left("Unexpected structure")
    }

  /**
   * Attempts to decode a value of type `A` from the specified UTF-8 encoded bytes, but may fail with a human-readable
   * error message if the provided bytes do not encode a value of this type.
   *
   * The array is read in place, not copied.
   *
   * Note: This method may not entirely consume the specified array.
   */
  final def decodeJson(bytes: Array[Byte]): Either[String, A] = decodeJson(Chunk.fromArray(bytes))

  /**
   * Returns this decoder but widened to the given super-type. It is an unchecked cast of the same instance.
   */
  final def widen[B >: A]: JsonDecoder[B] = self.asInstanceOf[JsonDecoder[B]]

  /**
   * Returns a new decoder that combines this decoder and the specified decoder using fallback semantics: such that if
   * this decoder fails, the input is rewound and the specified decoder is tried instead. When both fail, the error is
   * the one from `that`.
   *
   * To rewind, the decoder records everything this decoder reads, so it can use more memory than a hand-coded
   * alternative. When the first character tells the alternatives apart, `JsonDecoder.peekChar` needs no recording:
   *
   * {{{
   * val decoder: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
   *   case 't' | 'f' => JsonDecoder[Boolean].widen[AnyVal]
   *   case _         => JsonDecoder[Int].widen[AnyVal]
   * }
   * }}}
   *
   * A version number that arrives either as a number or as a string:
   *
   * {{{
   * val version: JsonDecoder[String] = JsonDecoder[Int].map(_.toString).orElse(JsonDecoder[String])
   *
   * val fromNumber = version.decodeJson("3")       // Right(3)
   * val fromString = version.decodeJson("\"3.1\"") // Right(3.1)
   * }}}
   */
  final def orElse[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1] =
    new JsonDecoder.AbstractJsonDecoder[A1] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): A1 = {
        val rr = RecordingReader(in)
        try self.unsafeDecode(trace, rr)
        catch {
          case _: JsonDecoder.UnsafeJson | _: UnexpectedEnd =>
            rr.rewind()
            that.unsafeDecode(trace, rr)
        }
      }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A1 =
        try self.unsafeFromJsonAST(trace, json)
        catch {
          case _: JsonDecoder.UnsafeJson | _: UnexpectedEnd => that.unsafeFromJsonAST(trace, json)
        }

      override def unsafeDecodeMissing(trace: List[JsonError]): A1 =
        try self.unsafeDecodeMissing(trace)
        catch {
          case _: JsonDecoder.UnsafeJson | _: UnexpectedEnd => that.unsafeDecodeMissing(trace)
        }
    }

  /**
   * Returns a new decoder that combines this decoder and the specified decoder using fallback semantics: such that if
   * this decoder fails, the specified decoder will be tried instead. A result of this decoder is wrapped in `Left` and
   * a result of `that` in `Right`. It rewinds the input the way [[JsonDecoder#orElse]] does.
   *
   * {{{
   * val portOrName: JsonDecoder[Either[Int, String]] = JsonDecoder[Int].orElseEither(JsonDecoder[String])
   *
   * val port = portOrName.decodeJson("8080")     // Right(Left(8080))
   * val name = portOrName.decodeJson("\"http\"") // Right(Right(http))
   * }}}
   */
  final def orElseEither[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    self.map(new Left(_)).orElse(that.map(new Right(_)))

  /**
   * Returns a new decoder whose decoded values will be mapped by the specified function. An exception thrown by `f` is
   * not caught by `decodeJson`; use [[JsonDecoder#mapOrFail]] to reject a value.
   *
   * {{{
   * final case class Celsius(value: Double)
   *
   * val celsius: JsonDecoder[Celsius] = JsonDecoder[Double].map(Celsius(_))
   * }}}
   */
  final def map[B](f: A => B): JsonDecoder[B] =
    new MappedJsonDecoder[B] {
      private[json] def underlying: JsonDecoder[A] = self

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B = f(self.unsafeDecode(trace, in))

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): B = f(self.unsafeFromJsonAST(trace, json))

      override def unsafeDecodeMissing(trace: List[JsonError]): B = f(self.unsafeDecodeMissing(trace))
    }

  /**
   * Returns a new decoder whose decoded values will be mapped by the specified function, which may itself decide to
   * fail by returning a `Left` with a message. The message is reported at the position of the value, like any other
   * decoding error.
   *
   * {{{
   * val positive: JsonDecoder[Int] =
   *   JsonDecoder[Int].mapOrFail(n => if (n > 0) Right(n) else Left("must be positive"))
   *
   * val rejected = positive.decodeJson("-1") // Left((must be positive))
   * }}}
   */
  final def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B] =
    new MappedJsonDecoder[B] {
      private[json] def underlying: JsonDecoder[A] = self

      def unsafeDecode(trace: List[JsonError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in)) match {
          case Right(b)  => b
          case Left(err) => Lexer.error(err, trace)
        }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): B =
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
   * Returns a new decoder that reads a JSON array of exactly two elements, the first with this decoder and the second
   * with `that`, and returns them as a tuple. A shorter array fails with `expected ',' got ']'` and a longer one with
   * `expected ']' got ','`.
   *
   * {{{
   * val stock: JsonDecoder[(String, Int)] = JsonDecoder[String].zip(JsonDecoder[Int])
   *
   * val apples = stock.decodeJson("[\"apples\",3]") // Right((apples,3))
   * }}}
   */
  final def zip[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)] = JsonDecoder.tuple2(this, that)

  /**
   * Zips two decoders, but discards the output on the right hand side. The JSON array must still have both elements.
   */
  final def zipLeft[B](that: => JsonDecoder[B]): JsonDecoder[A] = self.zipWith(that)((a, _) => a)

  /**
   * Zips two decoders, but discards the output on the left hand side. The JSON array must still have both elements.
   */
  final def zipRight[B](that: => JsonDecoder[B]): JsonDecoder[B] = self.zipWith(that)((_, b) => b)

  /**
   * Zips two decoders into one, transforming the outputs of the two decoders by the specified function.
   */
  final def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C] = self.zip(that).map(f.tupled)

  /**
   * Returns the value that a derived decoder uses when the field is absent from the JSON object. The default fails with
   * the message `missing`, rendered as `.field(missing)`. The `Option` decoder returns `None`, and the collection
   * decoders return an empty collection, which derived decoders use only when `ExplicitEmptyCollections` has
   * `decoding = false`.
   */
  def unsafeDecodeMissing(trace: List[JsonError]): A = Lexer.error("missing", trace)

  /**
   * Low-level, unsafe method to decode a value or throw an exception. This method should not be called in application
   * code, although it can be implemented for user-defined data structures.
   */
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A

  /**
   * Low-level method behind [[JsonDecoder#fromJsonAST]]: decodes `json` or throws `JsonDecoder.UnsafeJson`. The default
   * implementation encodes `json` to JSON text and runs [[JsonDecoder#unsafeDecode]] on it. Override it to read the
   * tree directly.
   */
  def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
    unsafeDecode(trace, new FastStringReader(Json.encoder.encodeJson(json, None)))

  /**
   * Decode a value from an already parsed Json AST.
   *
   * It calls [[JsonDecoder#unsafeFromJsonAST]], whose default implementation encodes the Json to JSON text and parses
   * that with [[JsonDecoder#unsafeDecode]]. Override `unsafeFromJsonAST` to provide a more performant implementation.
   * Some messages are worded differently from `decodeJson`, for example `expected a Int` instead of `expected an Int`.
   *
   * {{{
   * import zio.json.ast.Json
   *
   * val seven  = JsonDecoder[Int].fromJsonAST(Json.Num(7))       // Right(7)
   * val failed = JsonDecoder[Int].fromJsonAST(Json.Str("seven")) // Left((expected a Int))
   * }}}
   */
  final def fromJsonAST(json: Json): Either[String, A] =
    try new Right(unsafeFromJsonAST(Nil, json))
    catch {
      case e: JsonDecoder.UnsafeJson => new Left(JsonError.render(e.trace))
      case _: UnexpectedEnd          => new Left("Unexpected end of input")
      case _: StackOverflowError     => new Left("Unexpected structure")
    }
}

/**
 * Instances of [[JsonDecoder]] for the standard types: primitives, `String`, numbers, `Option`, `Either`, tuples,
 * collections, maps, `java.time` types, `UUID` and `Currency`, and constructors such as `peekChar` and `suspend`.
 */
object JsonDecoder extends GeneratedTupleDecoders with DecoderLowPriority1 with JsonDecoderVersionSpecific {

  /** Abstract class of the `JsonDecoder` trait to reduce class file size in subclasses. */
  abstract class AbstractJsonDecoder[A] extends JsonDecoder[A]

  /** An alias for the `zio.json.JsonError` type. */
  type JsonError = zio.json.JsonError

  /** An alias for the `zio.json.JsonError` companion object. */
  val JsonError = zio.json.JsonError

  /** Returns the `JsonDecoder[A]` in implicit scope, as in `JsonDecoder[Int]`. */
  def apply[A](implicit a: JsonDecoder[A]): JsonDecoder[A] = a

  /**
   * The exception that [[JsonDecoder#unsafeDecode]] throws on failure, carrying the error path. It has no stack trace.
   * `decodeJson`, `fromJson` and [[JsonDecoder#fromJsonAST]] catch it and render the trace with `JsonError.render`.
   *
   * Design note: we could require the position in the stream here to improve debugging messages. But the cost would be
   * that the RetractReader would need to keep track and any wrappers would need to preserve the position. It may still
   * be desirable to do this but at the moment it is not necessary.
   */
  final case class UnsafeJson(trace: List[JsonError])
      extends Exception("If you see this, a developer made a mistake using JsonDecoder")
      with NoStackTrace

  /**
   * Returns a decoder that reads the next non-whitespace character, puts it back, and decodes with the decoder that
   * `partialFunction` returns for it. When no case matches, decoding fails with a message that starts with
   * `missing case in` and names the character; the message currently ends with a doubled quote, for example `'x''`.
   *
   * {{{
   * val flagOrCount: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
   *   case 't' | 'f' => JsonDecoder[Boolean].widen[AnyVal]
   *   case _         => JsonDecoder[Int].widen[AnyVal]
   * }
   *
   * val flag  = flagOrCount.decodeJson("true") // Right(true)
   * val count = flagOrCount.decodeJson("42")   // Right(42)
   * }}}
   */
  def peekChar[A](partialFunction: PartialFunction[Char, JsonDecoder[A]]): JsonDecoder[A] =
    new JsonDecoder.AbstractJsonDecoder[A] {
      override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
        val c = in.nextNonWhitespace()
        if (partialFunction.isDefinedAt(c)) {
          in.retract()
          partialFunction(c).unsafeDecode(trace, in)
        } else Lexer.error(s"missing case in `peekChar` for '${c}''", trace)
      }
    }

  /**
   * Returns a decoder that builds `decoder0` on first use and then reuses it, so the decoder of a recursive type can
   * refer to itself.
   *
   * {{{
   * final case class Category(name: String, children: List[Category])
   *
   * object Category {
   *   implicit val decoder: JsonDecoder[Category] = JsonDecoder.suspend(DeriveJsonDecoder.gen[Category])
   * }
   * }}}
   */
  def suspend[A](decoder0: => JsonDecoder[A]): JsonDecoder[A] =
    new MappedJsonDecoder[A] {
      private[json] def underlying: JsonDecoder[A] = decoder0

      lazy val decoder = decoder0

      override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = decoder.unsafeDecode(trace, in)

      override def unsafeDecodeMissing(trace: List[JsonError]): A = decoder.unsafeDecodeMissing(trace)

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = decoder.unsafeFromJsonAST(trace, json)
    }

  /** Decodes a JSON string, with its escapes resolved. Any other JSON value fails with `expected string`. */
  implicit val string: JsonDecoder[String] = new JsonDecoder.AbstractJsonDecoder[String] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): String = Lexer.string(trace, in).toString

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): String =
      json match {
        case s: Json.Str => s.value
        case _           => Lexer.error("expected string", trace)
      }
  }

  /**
   * Decodes `true` or `false`. Anything else, including the strings `"true"` and `"false"`, fails with
   * `expected a Boolean`.
   */
  implicit val boolean: JsonDecoder[Boolean] = new JsonDecoder.AbstractJsonDecoder[Boolean] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Boolean = Lexer.boolean(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Boolean =
      json match {
        case b: Json.Bool => b.value
        case _            => Lexer.error("expected boolean", trace)
      }
  }

  /** Reads and discards one JSON value of any type, and returns `()`. */
  implicit val unit: JsonDecoder[Unit] = new JsonDecoder.AbstractJsonDecoder[Unit] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Unit = {
      Lexer.skipValue(trace, in)
      ()
    }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Unit = ()
  }

  /**
   * Decodes a JSON string of exactly one character, such as `"x"` or `"\n"`. A longer or empty string fails with
   * `expected single character string`.
   */
  implicit val char: JsonDecoder[Char] = new JsonDecoder.AbstractJsonDecoder[Char] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Char = Lexer.char(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Char =
      json match {
        case s: Json.Str if s.value.length == 1 => s.value.charAt(0)
        case _                                  => Lexer.error("expected single character string", trace)
      }
  }

  /** Decodes a JSON string into a `Symbol`. */
  implicit val symbol: JsonDecoder[Symbol] = string.map(Symbol(_))

  /**
   * Decodes a JSON number, or a number inside a JSON string such as `"12"`, into a `Byte`. A value out of range, such
   * as `128`, or a value that is not a number, fails with `expected a Byte`.
   */
  implicit val byte: JsonDecoder[Byte] = new JsonDecoder.AbstractJsonDecoder[Byte] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Byte =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.byte(trace, in)
      } else {
        val a = Lexer.byte(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Byte = {
      json match {
        case n: Json.Num =>
          try return n.value.byteValueExact
          catch {
            case _: ArithmeticException =>
          }
        case s: Json.Str =>
          try return UnsafeNumbers.byte_(new FastStringReader(s.value), true)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error("expected a Byte", trace)
    }
  }

  /**
   * Decodes a JSON number, or a number inside a JSON string such as `"12"`, into a `Short`. A value out of range, such
   * as `40000`, or a value that is not a number, fails with `expected a Short`.
   */
  implicit val short: JsonDecoder[Short] = new JsonDecoder.AbstractJsonDecoder[Short] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Short =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.short(trace, in)
      } else {
        val a = Lexer.short(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Short = {
      json match {
        case n: Json.Num =>
          try return n.value.shortValueExact
          catch {
            case _: ArithmeticException =>
          }
        case s: Json.Str =>
          try return UnsafeNumbers.short_(new FastStringReader(s.value), true)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error("expected a Short", trace)
    }
  }

  /**
   * Decodes a JSON number, or a number inside a JSON string such as `"12"`, into an `Int`. A value out of range, such
   * as `2147483648`, or a value that is not a number, fails with `expected an Int` (`expected a Int` from
   * [[JsonDecoder#fromJsonAST]]).
   */
  implicit val int: JsonDecoder[Int] = new JsonDecoder.AbstractJsonDecoder[Int] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Int =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.int(trace, in)
      } else {
        val a = Lexer.int(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Int = {
      json match {
        case n: Json.Num =>
          try return n.value.intValueExact
          catch {
            case _: ArithmeticException =>
          }
        case s: Json.Str =>
          try return UnsafeNumbers.int_(new FastStringReader(s.value), true)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error("expected a Int", trace)
    }
  }

  /**
   * Decodes a JSON number, or a number inside a JSON string such as `"12"`, into a `Long`. A value out of range, or a
   * value that is not a number, fails with `expected a Long`.
   */
  implicit val long: JsonDecoder[Long] = new JsonDecoder.AbstractJsonDecoder[Long] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Long =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.long(trace, in)
      } else {
        val a = Lexer.long(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Long = {
      json match {
        case n: Json.Num =>
          try return n.value.longValueExact
          catch {
            case _: ArithmeticException =>
          }
        case s: Json.Str =>
          try return UnsafeNumbers.long_(new FastStringReader(s.value), true)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error("expected a Long", trace)
    }
  }

  /**
   * Decodes a JSON integer, or one inside a JSON string, into a `java.math.BigInteger`. An integer that needs more than
   * 256 bits fails with `expected a 256-bit BigInteger`.
   */
  implicit val bigInteger: JsonDecoder[java.math.BigInteger] =
    new JsonDecoder.AbstractJsonDecoder[java.math.BigInteger] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): java.math.BigInteger =
        if (in.nextNonWhitespace() != '"') {
          in.retract()
          Lexer.bigInteger(trace, in)
        } else {
          val a = Lexer.bigInteger(trace, in)
          val c = in.readChar()
          if (c != '"') Lexer.error('\"', c, trace)
          a
        }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): java.math.BigInteger = {
        json match {
          case n: Json.Num =>
            try return n.value.toBigIntegerExact
            catch {
              case _: ArithmeticException =>
            }
          case s: Json.Str =>
            try return UnsafeNumbers.bigInteger_(new FastStringReader(s.value), true, Lexer.NumberMaxBits)
            catch {
              case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
            }
          case _ =>
        }
        Lexer.error(s"expected a $NumberMaxBits-bit BigInteger", trace)
      }
    }

  /**
   * Decodes a JSON integer, or one inside a JSON string, into a `BigInt`. An integer that needs more than 256 bits
   * fails with `expected a 256-bit BigInt`.
   */
  implicit val scalaBigInt: JsonDecoder[BigInt] = new JsonDecoder.AbstractJsonDecoder[BigInt] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): BigInt =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.bigInt(trace, in)
      } else {
        val a = Lexer.bigInt(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): BigInt = {
      json match {
        case n: Json.Num =>
          try return BigInt(n.value.toBigIntegerExact)
          catch {
            case _: ArithmeticException =>
          }
        case s: Json.Str =>
          try return UnsafeNumbers.bigInt_(new FastStringReader(s.value), true, Lexer.NumberMaxBits)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error(s"expected a $NumberMaxBits-bit BigInt", trace)
    }
  }

  /**
   * Decodes a JSON number, or a number inside a JSON string, into a `Float`. A value that is not a number fails with
   * `expected a Float`.
   */
  implicit val float: JsonDecoder[Float] = new JsonDecoder.AbstractJsonDecoder[Float] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Float =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.float(trace, in)
      } else {
        val a = Lexer.float(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Float = {
      json match {
        case n: Json.Num =>
          return n.value.floatValue
        case s: Json.Str =>
          try return UnsafeNumbers.float_(new FastStringReader(s.value), true, Lexer.NumberMaxBits)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error("expected a Float", trace)
    }
  }

  /**
   * Decodes a JSON number, or a number inside a JSON string, into a `Double`. The strings `"NaN"` and `"Infinity"`
   * decode to those values, and a number beyond the `Double` range, such as `1e400`, decodes to `Infinity`. A value
   * that is not a number fails with `expected a Double`.
   */
  implicit val double: JsonDecoder[Double] = new JsonDecoder.AbstractJsonDecoder[Double] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Double =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.double(trace, in)
      } else {
        val a = Lexer.double(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Double = {
      json match {
        case n: Json.Num =>
          return n.value.doubleValue
        case s: Json.Str =>
          try return UnsafeNumbers.double_(new FastStringReader(s.value), true, Lexer.NumberMaxBits)
          catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error("expected a Double", trace)
    }
  }

  /**
   * Decodes a JSON number, or one inside a JSON string, into a `java.math.BigDecimal`. A number whose digits need more
   * than 256 bits fails with `expected a BigDecimal with 256-bit mantissa`.
   */
  implicit val bigDecimal: JsonDecoder[java.math.BigDecimal] =
    new JsonDecoder.AbstractJsonDecoder[java.math.BigDecimal] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): java.math.BigDecimal =
        if (in.nextNonWhitespace() != '"') {
          in.retract()
          Lexer.bigDecimal(trace, in)
        } else {
          val a = Lexer.bigDecimal(trace, in)
          val c = in.readChar()
          if (c != '"') Lexer.error('\"', c, trace)
          a
        }

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): java.math.BigDecimal = {
        json match {
          case n: Json.Num =>
            return n.value
          case s: Json.Str =>
            try return UnsafeNumbers.bigDecimal_(new FastStringReader(s.value), true, Lexer.NumberMaxBits)
            catch {
              case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
            }
          case _ =>
        }
        Lexer.error(s"expected a BigDecimal with $NumberMaxBits-bit mantissa", trace)
      }
    }

  /**
   * Decodes a JSON number, or one inside a JSON string, into a `BigDecimal`. A number whose digits need more than 256
   * bits fails with `expected a BigDecimal with 256-bit mantissa`.
   */
  implicit val scalaBigDecimal: JsonDecoder[BigDecimal] = new JsonDecoder.AbstractJsonDecoder[BigDecimal] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): BigDecimal =
      if (in.nextNonWhitespace() != '"') {
        in.retract()
        Lexer.bigDecimal(trace, in)
      } else {
        val a = Lexer.bigDecimal(trace, in)
        val c = in.readChar()
        if (c != '"') Lexer.error('\"', c, trace)
        a
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): BigDecimal = {
      json match {
        case n: Json.Num =>
          return new BigDecimal(n.value, BigDecimal.defaultMathContext)
        case s: Json.Str =>
          try {
            val bd = UnsafeNumbers.bigDecimal_(new FastStringReader(s.value), true, Lexer.NumberMaxBits)
            return new BigDecimal(bd, BigDecimal.defaultMathContext)
          } catch {
            case _: UnexpectedEnd | UnsafeNumbers.UnsafeNumber =>
          }
        case _ =>
      }
      Lexer.error(s"expected a BigDecimal with $NumberMaxBits-bit mantissa", trace)
    }
  }
  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.

  /**
   * Decodes `null` as `None` and any other value with the `A` decoder, wrapped in `Some`. An absent field of a derived
   * decoder is `None` as well. For `Option[Option[A]]`, `null` is the outer `None`.
   */
  implicit def option[A](implicit A: JsonDecoder[A]): JsonDecoder[Option[A]] =
    new OptionJsonDecoder[Option[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Option[A] = None

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[A] =
        if (in.nextNonWhitespace() != 'n') {
          in.retract()
          new Some(A.unsafeDecode(trace, in))
        } else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') None
        else Lexer.error("expected 'null'", trace)

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Option[A] =
        if (json ne Json.Null) new Some(A.unsafeFromJsonAST(trace, json))
        else None
    }

  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  /**
   * Decodes a JSON object with one key: `a`, `Left` or `left` for a `Left`, and `b`, `Right` or `right` for a `Right`,
   * as in `{"Left":404}`. Other keys are skipped. An object with none of these keys fails with `missing fields`, one
   * with both a left and a right key with `ambiguous either, zip present`, and two left keys with `duplicate` at the
   * second key.
   */
  implicit def either[A, B](implicit A: JsonDecoder[A], B: JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    new JsonDecoder.AbstractJsonDecoder[Either[A, B]] {
      private[this] val names  = Array("a", "Left", "left", "b", "Right", "right")
      private[this] val matrix = new StringMatrix(names)
      private[this] val spans  = names.map(new JsonError.ObjectAccess(_))

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Either[A, B] = {
        val c = in.nextNonWhitespace()
        if (c != '{') Lexer.error('{', c, trace)
        var left: Any  = null
        var right: Any = null
        if (Lexer.firstField(in))
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
    if (c == '[') {
      var i = 0
      if (Lexer.firstArrayElement(in)) while ({
        builder += A.unsafeDecode(new JsonError.ArrayAccess(i) :: trace, in)
        i += 1
        Lexer.nextArrayElement(trace, in)
      }) ()
      return builder.result()
    }
    Lexer.error('[', c, trace)
  }

  @inline private[json] def keyValueBuilder[K, V, T[X, Y] <: Iterable[(X, Y)]](
    trace: List[JsonError],
    in: RetractReader,
    builder: mutable.Builder[(K, V), T[K, V]]
  )(implicit K: JsonFieldDecoder[K], V: JsonDecoder[V]): T[K, V] = {
    var c = in.nextNonWhitespace()
    if (c == '{') {
      if (Lexer.firstField(in))
        while ({
          val field  = Lexer.string(trace, in).toString
          val trace_ = new JsonError.ObjectAccess(field) :: trace
          c = in.nextNonWhitespace()
          if (c != ':') Lexer.error(':', c, trace)
          val value = V.unsafeDecode(trace_, in)
          builder += ((K.unsafeDecodeField(trace_, field), value))
          Lexer.nextField(trace, in)
        }) ()
      return builder.result()
    }
    Lexer.error('{', c, trace)
  }
}

private[json] trait CollectionJsonDecoder[A] extends JsonDecoder[A]
private[json] trait OptionJsonDecoder[A]     extends JsonDecoder[A]
private[json] trait MappedJsonDecoder[A]     extends JsonDecoder[A] {
  private[json] def underlying: JsonDecoder[_]
}

private[json] trait DecoderLowPriority1 extends DecoderLowPriority2 {
  this: JsonDecoder.type =>

  /**
   * Decodes a JSON array into an `Array`, failing at the index of the first element that does not decode, for example
   * `[1](expected an Int)`.
   */
  implicit def array[A](implicit A: JsonDecoder[A], classTag: reflect.ClassTag[A]): JsonDecoder[Array[A]] =
    new CollectionJsonDecoder[Array[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Array[A] = Array.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Array[A] = {
        val c = in.nextNonWhitespace()
        if (c == '[') {
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
            if (i == l) return x
            val x1 = new Array[A](i)
            System.arraycopy(x, 0, x1, 0, i)
            return x1
          } else return Array.empty
        }
        Lexer.error('[', c, trace)
      }
    }

  /** Decodes a JSON array into a `Seq`, failing at the index of the first element that does not decode. */
  implicit def seq[A: JsonDecoder]: JsonDecoder[Seq[A]] =
    new CollectionJsonDecoder[Seq[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Seq[A] = Seq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Seq[A] =
        builder(trace, in, immutable.Seq.newBuilder[A])
    }

  /**
   * Decodes a JSON array into a `Chunk`, failing at the index of the first element that does not decode. From a `Json`
   * value that is not an array, [[JsonDecoder#fromJsonAST]] fails with `Not an array`.
   */
  implicit def chunk[A: JsonDecoder]: JsonDecoder[Chunk[A]] =
    new CollectionJsonDecoder[Chunk[A]] {
      private[this] val decoder = JsonDecoder[A]

      override def unsafeDecodeMissing(trace: List[JsonError]): Chunk[A] = Chunk.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Chunk[A] =
        builder(trace, in, zio.ChunkBuilder.make[A]())

      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Chunk[A] =
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

  /** Decodes a non-empty JSON array into a `NonEmptyChunk`. An empty array fails with `Chunk was empty`. */
  implicit def nonEmptyChunk[A: JsonDecoder]: JsonDecoder[NonEmptyChunk[A]] =
    chunk[A].mapOrFail(NonEmptyChunk.fromChunk(_).toRight("Chunk was empty"))

  /** Decodes a JSON array into an `IndexedSeq`, failing at the index of the first element that does not decode. */
  implicit def indexedSeq[A: JsonDecoder]: JsonDecoder[IndexedSeq[A]] =
    new CollectionJsonDecoder[IndexedSeq[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): IndexedSeq[A] = IndexedSeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): IndexedSeq[A] =
        builder(trace, in, IndexedSeq.newBuilder[A])
    }

  /**
   * Decodes a JSON array into an `immutable.LinearSeq`, failing at the index of the first element that does not decode.
   */
  implicit def linearSeq[A: JsonDecoder]: JsonDecoder[immutable.LinearSeq[A]] =
    new CollectionJsonDecoder[immutable.LinearSeq[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.LinearSeq[A] = immutable.LinearSeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): LinearSeq[A] =
        builder(trace, in, immutable.LinearSeq.newBuilder[A])
    }

  /**
   * Decodes a JSON array into an `immutable.ListSet`, which keeps the first occurrence of each element and drops
   * duplicates.
   */
  implicit def listSet[A: JsonDecoder]: JsonDecoder[immutable.ListSet[A]] =
    new CollectionJsonDecoder[immutable.ListSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ListSet[A] = immutable.ListSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): ListSet[A] =
        builder(trace, in, immutable.ListSet.newBuilder[A])
    }

  /** Decodes a JSON array into an `immutable.TreeSet`, sorted by the `Ordering` in scope. Duplicates are dropped. */
  implicit def treeSet[A: JsonDecoder: Ordering]: JsonDecoder[immutable.TreeSet[A]] =
    new CollectionJsonDecoder[immutable.TreeSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.TreeSet[A] = immutable.TreeSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): TreeSet[A] =
        builder(trace, in, immutable.TreeSet.newBuilder[A])
    }

  /** Decodes a JSON array into a `List`, failing at the index of the first element that does not decode. */
  implicit def list[A: JsonDecoder]: JsonDecoder[List[A]] =
    new CollectionJsonDecoder[List[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): List[A] = List.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): List[A] =
        builder(trace, in, new mutable.ListBuffer[A])
    }

  /** Decodes a JSON array into a `Vector`, failing at the index of the first element that does not decode. */
  implicit def vector[A: JsonDecoder]: JsonDecoder[Vector[A]] =
    new CollectionJsonDecoder[Vector[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Vector[A] = Vector.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Vector[A] =
        builder(trace, in, immutable.Vector.newBuilder[A])
    }

  /** Decodes a JSON array into a `Set`. Duplicates are dropped without an error. */
  implicit def set[A: JsonDecoder]: JsonDecoder[Set[A]] =
    new CollectionJsonDecoder[Set[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Set[A] = Set.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Set[A] =
        builder(trace, in, Set.newBuilder[A])
    }

  /** Decodes a JSON array into an `immutable.HashSet`. Duplicates are dropped without an error. */
  implicit def hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] =
    new CollectionJsonDecoder[immutable.HashSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.HashSet[A] = immutable.HashSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.HashSet[A] =
        builder(trace, in, immutable.HashSet.newBuilder[A])
    }

  /**
   * Decodes a JSON object into a `Map`, with a [[JsonFieldDecoder]] for the keys. When a key appears twice, the last
   * value wins. A key or a value that does not decode fails at the key, for example `.b(expected an Int)`.
   */
  implicit def map[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[Map[K, V]] =
    new CollectionJsonDecoder[Map[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Map[K, V] = Map.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Map[K, V] =
        keyValueBuilder(trace, in, Map.newBuilder[K, V])
    }

  /**
   * Decodes a JSON object into an `immutable.HashMap`, with a [[JsonFieldDecoder]] for the keys. When a key appears
   * twice, the last value wins.
   */
  implicit def hashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    new CollectionJsonDecoder[immutable.HashMap[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.HashMap[K, V] = immutable.HashMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.HashMap[K, V] =
        keyValueBuilder(trace, in, immutable.HashMap.newBuilder[K, V])
    }

  /**
   * Decodes a JSON object into a new `mutable.Map`, with a [[JsonFieldDecoder]] for the keys. When a key appears twice,
   * the last value wins.
   */
  implicit def mutableMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[mutable.Map[K, V]] =
    new CollectionJsonDecoder[mutable.Map[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): mutable.Map[K, V] = mutable.Map.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): mutable.Map[K, V] =
        keyValueBuilder(trace, in, mutable.Map.newBuilder[K, V])
    }

  /** Decodes a JSON array into an `immutable.SortedSet`, sorted by the `Ordering` in scope. Duplicates are dropped. */
  implicit def sortedSet[A: Ordering: JsonDecoder]: JsonDecoder[immutable.SortedSet[A]] =
    new CollectionJsonDecoder[immutable.SortedSet[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.SortedSet[A] = immutable.SortedSet.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.SortedSet[A] =
        builder(trace, in, immutable.SortedSet.newBuilder[A])
    }

  /**
   * Decodes a JSON object into a `collection.SortedMap`, sorted by the `Ordering` of the keys, with a
   * [[JsonFieldDecoder]] for the keys. When a key appears twice, the last value wins.
   */
  implicit def sortedMap[K: JsonFieldDecoder: Ordering, V: JsonDecoder]: JsonDecoder[collection.SortedMap[K, V]] =
    new CollectionJsonDecoder[collection.SortedMap[K, V]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): collection.SortedMap[K, V] = collection.SortedMap.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): collection.SortedMap[K, V] =
        keyValueBuilder(trace, in, collection.SortedMap.newBuilder[K, V])
    }

  /**
   * Decodes a JSON object into an `immutable.ListMap`, in the order of the keys in the input, with a
   * [[JsonFieldDecoder]] for the keys. When a key appears twice, the last value wins and the key keeps its first
   * position; on Scala 2.12 it moves to the position of its last occurrence.
   */
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

  /** Decodes a JSON array into an `Iterable`, failing at the index of the first element that does not decode. */
  implicit def iterable[A: JsonDecoder]: JsonDecoder[Iterable[A]] =
    new CollectionJsonDecoder[Iterable[A]] {
      override def unsafeDecodeMissing(trace: List[JsonError]): Iterable[A] = Iterable.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): Iterable[A] =
        builder(trace, in, immutable.Iterable.newBuilder[A])
    }

  // not implicit because this overlaps with decoders for lists of tuples
  /**
   * Decodes a JSON object into a `Chunk` of key and value pairs, in the order of the input and keeping every duplicate
   * key. It is not implicit, because the `Chunk` decoder for pairs reads a JSON array of two-element arrays.
   *
   * {{{
   * val counts = JsonDecoder.keyValueChunk[String, Int].decodeJson("""{"b":1,"a":2,"b":3}""")
   * // Right(Chunk((b,1),(a,2),(b,3)))
   * }}}
   */
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

  /**
   * Decodes a day name such as `"MONDAY"`, in any letter case, into a `java.time.DayOfWeek`. An invalid value fails
   * with `expected a DayOfWeek`.
   */
  implicit val dayOfWeek: JsonDecoder[DayOfWeek] = new JsonDecoder.AbstractJsonDecoder[DayOfWeek] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): DayOfWeek = Lexer.dayOfWeek(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): DayOfWeek = {
      json match {
        case s: Json.Str =>
          try return DayOfWeek.valueOf(s.value.toUpperCase)
          catch {
            case _: IllegalArgumentException =>
          }
        case _ =>
      }
      Lexer.error("expected a DayOfWeek", trace)
    }
  }

  /**
   * Decodes an ISO-8601 duration such as `"PT1H30M"` into a `java.time.Duration`. Fails with `expected a Duration` for
   * an invalid value.
   */
  implicit val duration: JsonDecoder[Duration] = new JsonDecoder.AbstractJsonDecoder[Duration] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Duration = Lexer.duration(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Duration = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseDuration(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a Duration", trace)
    }
  }

  /**
   * Decodes an ISO-8601 instant such as `"2024-03-01T10:15:30Z"` into a `java.time.Instant`. An offset such as `+01:00`
   * is accepted and converted to UTC. An invalid value fails with `expected an Instant`.
   */
  implicit val instant: JsonDecoder[Instant] = new JsonDecoder.AbstractJsonDecoder[Instant] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Instant = Lexer.instant(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Instant = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseInstant(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected an Instant", trace)
    }
  }

  /**
   * Decodes an ISO-8601 date such as `"2024-02-29"` into a `java.time.LocalDate`. Fails with `expected a LocalDate` for
   * an invalid value, such as `"2023-02-29"`.
   */
  implicit val localDate: JsonDecoder[LocalDate] = new JsonDecoder.AbstractJsonDecoder[LocalDate] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): LocalDate = Lexer.localDate(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): LocalDate = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseLocalDate(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a LocalDate", trace)
    }
  }

  /**
   * Decodes an ISO-8601 date and time such as `"2024-02-29T10:15"` into a `java.time.LocalDateTime`. An invalid value
   * fails with `expected a LocalDateTime`.
   */
  implicit val localDateTime: JsonDecoder[LocalDateTime] = new JsonDecoder.AbstractJsonDecoder[LocalDateTime] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): LocalDateTime = Lexer.localDateTime(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): LocalDateTime = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseLocalDateTime(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a LocalDateTime", trace)
    }
  }

  /**
   * Decodes an ISO-8601 time such as `"10:15:30.123"` into a `java.time.LocalTime`. An invalid value fails with
   * `expected a LocalTime`.
   */
  implicit val localTime: JsonDecoder[LocalTime] = new JsonDecoder.AbstractJsonDecoder[LocalTime] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): LocalTime = Lexer.localTime(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): LocalTime = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseLocalTime(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a LocalTime", trace)
    }
  }

  /**
   * Decodes a month name such as `"MARCH"`, in any letter case, into a `java.time.Month`. An invalid value fails with
   * `expected a Month`.
   */
  implicit val month: JsonDecoder[Month] = new JsonDecoder.AbstractJsonDecoder[Month] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Month = Lexer.month(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Month = {
      json match {
        case s: Json.Str =>
          try return Month.valueOf(s.value.toUpperCase)
          catch {
            case _: IllegalArgumentException =>
          }
        case _ =>
      }
      Lexer.error("expected a Month", trace)
    }
  }

  /**
   * Decodes an ISO-8601 month and day such as `"--12-25"` into a `java.time.MonthDay`. An invalid value fails with
   * `expected a MonthDay`.
   */
  implicit val monthDay: JsonDecoder[MonthDay] = new JsonDecoder.AbstractJsonDecoder[MonthDay] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): MonthDay = Lexer.monthDay(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): MonthDay = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseMonthDay(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a MonthDay", trace)
    }
  }

  /**
   * Decodes an ISO-8601 date and time with an offset such as `"2024-03-01T10:15:30+02:00"` into a
   * `java.time.OffsetDateTime`. An invalid value fails with `expected an OffsetDateTime`.
   */
  implicit val offsetDateTime: JsonDecoder[OffsetDateTime] = new JsonDecoder.AbstractJsonDecoder[OffsetDateTime] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): OffsetDateTime = Lexer.offsetDateTime(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): OffsetDateTime = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseOffsetDateTime(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected an OffsetDateTime", trace)
    }
  }

  /**
   * Decodes an ISO-8601 time with an offset such as `"10:15:30+02:00"` into a `java.time.OffsetTime`. An invalid value
   * fails with `expected an OffsetTime`.
   */
  implicit val offsetTime: JsonDecoder[OffsetTime] = new JsonDecoder.AbstractJsonDecoder[OffsetTime] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): OffsetTime = Lexer.offsetTime(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): OffsetTime = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseOffsetTime(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected an OffsetTime", trace)
    }
  }

  /** Decodes an ISO-8601 period such as `"P1Y2M3D"` into a `java.time.Period`, or fails with `expected a Period`. */
  implicit val period: JsonDecoder[Period] = new JsonDecoder.AbstractJsonDecoder[Period] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Period = Lexer.period(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Period = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParsePeriod(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a Period", trace)
    }
  }

  /**
   * Decodes a year in a JSON string, such as `"2024"`, into a `java.time.Year`. A JSON number, or an invalid value,
   * fails with `expected a Year`.
   */
  implicit val year: JsonDecoder[Year] = new JsonDecoder.AbstractJsonDecoder[Year] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Year = Lexer.year(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Year = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseYear(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a Year", trace)
    }
  }

  /**
   * Decodes an ISO-8601 year and month such as `"2024-03"` into a `java.time.YearMonth`. An invalid value fails with
   * `expected a YearMonth`.
   */
  implicit val yearMonth: JsonDecoder[YearMonth] = new JsonDecoder.AbstractJsonDecoder[YearMonth] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): YearMonth = Lexer.yearMonth(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): YearMonth = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseYearMonth(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a YearMonth", trace)
    }
  }

  /**
   * Decodes an ISO-8601 date and time with an offset and an optional region into a `java.time.ZonedDateTime`, for
   * example `"2024-03-01T10:15:30+01:00[Europe/Paris]"`. Fails with `expected a ZonedDateTime` for an invalid value.
   */
  implicit val zonedDateTime: JsonDecoder[ZonedDateTime] = new JsonDecoder.AbstractJsonDecoder[ZonedDateTime] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): ZonedDateTime = Lexer.zonedDateTime(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): ZonedDateTime = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseZonedDateTime(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a ZonedDateTime", trace)
    }
  }

  /**
   * Decodes a region such as `"Europe/Paris"` or an offset such as `"+02:00"` into a `java.time.ZoneId`. An invalid
   * value fails with `expected a ZoneId`.
   */
  implicit val zoneId: JsonDecoder[ZoneId] = new JsonDecoder.AbstractJsonDecoder[ZoneId] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): ZoneId =
      try parsers.unsafeParseZoneId(Lexer.string(trace, in).toString)
      catch {
        case _: DateTimeException => Lexer.error("expected a ZoneId", trace)
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): ZoneId = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseZoneId(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a ZoneId", trace)
    }
  }

  /**
   * Decodes an offset such as `"+05:30"` or `"Z"` into a `java.time.ZoneOffset`. Fails with `expected a ZoneOffset` for
   * an invalid value, such as `"+25:00"`.
   */
  implicit val zoneOffset: JsonDecoder[ZoneOffset] = new JsonDecoder.AbstractJsonDecoder[ZoneOffset] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): ZoneOffset = Lexer.zoneOffset(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): ZoneOffset = {
      json match {
        case s: Json.Str =>
          try return parsers.unsafeParseZoneOffset(s.value)
          catch {
            case _: DateTimeException =>
          }
        case _ =>
      }
      Lexer.error("expected a ZoneOffset", trace)
    }
  }

  /**
   * Decodes a JSON string such as `"123e4567-e89b-12d3-a456-426614174000"` into a `java.util.UUID`. Five groups of
   * fewer hex digits, such as `"1-2-3-4-5"`, are accepted too. An invalid string fails with `expected a UUID`.
   */
  implicit val uuid: JsonDecoder[UUID] = new JsonDecoder.AbstractJsonDecoder[UUID] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): UUID = Lexer.uuid(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): UUID = {
      json match {
        case s: Json.Str =>
          try return UUIDParser.unsafeParse(s.value)
          catch {
            case _: IllegalArgumentException =>
          }
        case _ =>
      }
      Lexer.error("expected a UUID", trace)
    }
  }

  /**
   * Decodes an ISO 4217 currency code such as `"EUR"` into a `java.util.Currency`. The code is case-sensitive, so
   * `"eur"` fails with `expected a Currency`.
   */
  implicit val currency: JsonDecoder[java.util.Currency] = new JsonDecoder.AbstractJsonDecoder[java.util.Currency] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): java.util.Currency =
      try java.util.Currency.getInstance(Lexer.string(trace, in).toString)
      catch {
        case _: IllegalArgumentException => Lexer.error("expected a Currency", trace)
      }

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): java.util.Currency = {
      json match {
        case s: Json.Str =>
          try return java.util.Currency.getInstance(s.value)
          catch {
            case _: IllegalArgumentException =>
          }
        case _ =>
      }
      Lexer.error("expected a Currency", trace)
    }
  }
}

private[json] trait DecoderLowPriority4 extends DecoderLowPriorityVersionSpecific {

  /**
   * Returns the decoder of a `JsonCodec[A]` in implicit scope, when no `JsonDecoder[A]` is found with higher priority.
   */
  @inline implicit def fromCodec[A](implicit codec: JsonCodec[A]): JsonDecoder[A] = codec.decoder
}
