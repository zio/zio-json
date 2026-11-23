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
package zio

import zio.json.ast.Json

package object json extends JsonPackagePlatformSpecific {

  /**
   * Adds `toJson`, `toJsonPretty` and `toJsonAST` to every value whose type has a [[JsonEncoder]] in implicit scope.
   * `import zio.json._` brings it into scope.
   *
   * {{{
   * import zio.json._
   *
   * final case class Sensor(id: String, reading: Double)
   *
   * implicit val encoder: JsonEncoder[Sensor] = DeriveJsonEncoder.gen[Sensor]
   *
   * val compact = Sensor("t-1", 21.5).toJson
   * // {"id":"t-1","reading":21.5}
   * val pretty = Sensor("t-1", 21.5).toJsonPretty
   * // {
   * //   "id" : "t-1",
   * //   "reading" : 21.5
   * // }
   * }}}
   *
   * @see
   *   [[https://zio.dev/zio-json/getting-started/ Getting Started with ZIO Json]]
   */
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {

    /** Encodes the value as compact JSON text, with no whitespace between tokens. */
    @inline def toJson(implicit encoder: JsonEncoder[A]): String = encoder.encodeJson(a, None).toString

    // Jon Pretty's better looking brother, but a bit slower
    /**
     * Encodes the value as indented JSON text, two spaces per level, with `" : "` between a key and its value. Object
     * fields and the elements of arrays and collections go on separate lines; the elements of a tuple are separated by
     * `", "` on one line, although an array or object inside a tuple is still spread over lines.
     */
    @inline def toJsonPretty(implicit encoder: JsonEncoder[A]): String = encoder.encodeJson(a, Some(0)).toString

    /**
     * Encodes the value as a [[zio.json.ast.Json]] tree, or returns `Left` with a message when the encoder cannot build
     * one. A known bug: for a `Double` or `Float` that is `NaN` or infinite, it throws
     * `java.lang.NumberFormatException` instead of returning a `Left`.
     */
    @inline def toJsonAST(implicit encoder: JsonEncoder[A]): Either[String, Json] = encoder.toJsonAST(a)
  }

  /**
   * Adds `fromJson` to a `CharSequence` such as a `String`. `import zio.json._` brings it into scope.
   *
   * {{{
   * import zio.json._
   *
   * final case class Book(title: String, pages: Int)
   *
   * implicit val decoder: JsonDecoder[Book] = DeriveJsonDecoder.gen[Book]
   *
   * val ok      = """{"title":"Dune","pages":412}""".fromJson[Book] // Right(Book(Dune,412))
   * val missing = """{"title":"Dune"}""".fromJson[Book]             // Left(.pages(missing))
   * }}}
   *
   * @see
   *   [[https://zio.dev/zio-json/decoding/ Decoding]]
   */
  implicit final class DecoderOps(private val json: CharSequence) extends AnyVal {

    /**
     * Attempts to decode the raw JSON string as an `A`, with the same result as `decodeJson` on the `JsonDecoder[A]` in
     * implicit scope.
     *
     * On failure a human readable message is returned using a jq friendly format. For example the error
     * `.rows[0].elements[0].distance.value(missing)` tells us the location of a missing field named "value". We can use
     * part of the error message in the `jq` command line tool for further inspection, e.g.
     * `jq '.rows[0].elements[0].distance' input.json`.
     */
    @inline def fromJson[A](implicit decoder: JsonDecoder[A]): Either[String, A] = decoder.decodeJson(json)
  }

  /** Adds `fromJson` to a `Chunk[Byte]` of UTF-8 encoded JSON. `import zio.json._` brings it into scope. */
  implicit final class DecoderBytesOps(private val json: Chunk[Byte]) extends AnyVal {

    /**
     * Attempts to decode the UTF-8 encoded bytes as an `A`, without copying them into a `String` first.
     *
     * Errors are reported exactly as for the `CharSequence` variant.
     */
    @inline def fromJson[A](implicit decoder: JsonDecoder[A]): Either[String, A] = decoder.decodeJson(json)
  }

  /** Adds `fromJson` to an `Array[Byte]` of UTF-8 encoded JSON. `import zio.json._` brings it into scope. */
  implicit final class DecoderByteArrayOps(private val json: Array[Byte]) extends AnyVal {

    /**
     * Attempts to decode the UTF-8 encoded bytes as an `A`, without copying them into a `String` first.
     *
     * Errors are reported exactly as for the `CharSequence` variant.
     */
    @inline def fromJson[A](implicit decoder: JsonDecoder[A]): Either[String, A] = decoder.decodeJson(json)
  }
}
