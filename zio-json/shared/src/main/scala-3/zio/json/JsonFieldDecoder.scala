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

import zio.json.internal.{ FastStringReader, Lexer }
import zio.json.uuid.UUIDParser

/**
 * Decodes a JSON object key into a value of type `A`. The `Map`, `HashMap`, `mutable.Map`, `SortedMap` and `ListMap`
 * decoders, and `JsonDecoder.keyValueChunk`, need one for their key type, next to a [[JsonDecoder]] for the values.
 *
 * The companion object has instances for `String`, `Int`, `Long` and `java.util.UUID` keys. On Scala 3 it also has one
 * for any subtype of `String` that has a [[JsonDecoder]], such as a union of string literals. Build one for a type of
 * your own with `map` or `mapOrFail` from an existing instance.
 *
 * {{{
 * final case class Isbn(value: String)
 *
 * implicit val isbnKey: JsonFieldDecoder[Isbn] = JsonFieldDecoder[String].map(Isbn(_))
 *
 * val stock = """{"978-0441013593":4}""".fromJson[Map[Isbn, Int]]
 * // Right(Map(Isbn(978-0441013593) -> 4))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/decoding/ Decoding]]
 */
trait JsonFieldDecoder[+A] {
  self =>

  /** Returns a key decoder that applies `f` to each key this decoder produces. */
  final def map[B](f: A => B): JsonFieldDecoder[B] =
    new JsonFieldDecoder[B] {

      def unsafeDecodeField(trace: List[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in))
    }

  /**
   * Returns a key decoder that applies `f` to each key this decoder produces, and fails with the message in a `Left`.
   * The error is reported at the key, for example `.12(shelf out of range)` for the key `12`.
   *
   * {{{
   * val shelfKey: JsonFieldDecoder[Int] =
   *   JsonFieldDecoder[Int].mapOrFail(n => if (n >= 1 && n <= 9) Right(n) else Left("shelf out of range"))
   * }}}
   */
  final def mapOrFail[B](f: A => Either[String, B]): JsonFieldDecoder[B] =
    new JsonFieldDecoder[B] {

      def unsafeDecodeField(trace: List[JsonError], in: String): B =
        f(self.unsafeDecodeField(trace, in)) match {
          case Left(err) => Lexer.error(err, trace)
          case Right(b)  => b
        }
    }

  /**
   * Low-level method that decodes the unescaped key `in`, or throws `JsonDecoder.UnsafeJson` with the failure added to
   * `trace`. The map decoders pass a `trace` that already includes the key, so the error renders at the key's path.
   * Implement it when you write a key decoder by hand; application code uses `fromJson` or `decodeJson`.
   */
  def unsafeDecodeField(trace: List[JsonError], in: String): A
}

/**
 * Instances of [[JsonFieldDecoder]] for `String`, `Int`, `Long` and `java.util.UUID` keys. On Scala 3 it also has one
 * for any subtype of `String` that has a [[JsonDecoder]].
 */
object JsonFieldDecoder extends LowPriorityJsonFieldDecoder {

  /** Returns the `JsonFieldDecoder[A]` in implicit scope, as in `JsonFieldDecoder[Int]`. */
  def apply[A](implicit a: JsonFieldDecoder[A]): JsonFieldDecoder[A] = a

  /** Returns the key as it is. */
  implicit val string: JsonFieldDecoder[String] = new JsonFieldDecoder[String] {
    def unsafeDecodeField(trace: List[JsonError], in: String): String = in
  }

  /**
   * Parses the key with `String#toInt`, which also accepts a leading `+`. A key that is not an `Int` fails with
   * `Invalid Int: <key>`, for example `.two(Invalid Int: two)`; a key longer than 50 characters is cut to 50 followed
   * by `...` in the message.
   */
  implicit val int: JsonFieldDecoder[Int] = new JsonFieldDecoder[Int] {
    def unsafeDecodeField(trace: List[JsonError], in: String): Int =
      try in.toInt
      catch {
        case _: NumberFormatException => Lexer.error(s"Invalid Int: ${strip(in)}", trace)
      }
  }

  /**
   * Parses the key with `String#toLong`, which also accepts a leading `+`. A key that is not a `Long` fails with
   * `Invalid Long: <key>`, for example `.abc(Invalid Long: abc)`; a key longer than 50 characters is cut to 50 followed
   * by `...` in the message.
   */
  implicit val long: JsonFieldDecoder[Long] = new JsonFieldDecoder[Long] {
    def unsafeDecodeField(trace: List[JsonError], in: String): Long =
      try in.toLong
      catch {
        case _: NumberFormatException => Lexer.error(s"Invalid Long: ${strip(in)}", trace)
      }
  }

  /**
   * Parses the key with the same parser as the `UUID` value decoder, which also accepts five groups of fewer hex digits
   * such as `1-2-3-4-5`. An invalid key fails with `expected a UUID`.
   */
  implicit val uuid: JsonFieldDecoder[java.util.UUID] = new JsonFieldDecoder[java.util.UUID] {
    def unsafeDecodeField(trace: List[JsonError], in: String): java.util.UUID =
      try UUIDParser.unsafeParse(in)
      catch {
        case _: IllegalArgumentException => Lexer.error("expected a UUID", trace)
      }
  }

  private[json] def strip(s: String, len: Int = 50): String =
    if (s.length <= len) s
    else s.substring(0, len) + "..."
}

private[json] trait LowPriorityJsonFieldDecoder {

  /**
   * Scala 3 only: a key decoder for any subtype `T` of `String` that has a `JsonDecoder[T]`, such as a union of string
   * literals. It puts the key between quotes and decodes the result with that `JsonDecoder`, so the key `test` for the
   * type `"dev" | "prod"` fails with `.test(expected one of: dev, prod)`.
   */
  implicit def stringLike[T <: String](implicit decoder: JsonDecoder[T]): JsonFieldDecoder[T] =
    new JsonFieldDecoder[T] {
      def unsafeDecodeField(trace: List[JsonError], in: String): T =
        decoder.unsafeDecode(trace, new FastStringReader(s""""$in""""))
    }
}
