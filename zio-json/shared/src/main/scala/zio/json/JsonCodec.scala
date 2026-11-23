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

import zio.{ Chunk, NonEmptyChunk }

import scala.collection.immutable

/**
 * A [[JsonEncoder]] and a [[JsonDecoder]] for the same type `A`, kept together. Define a codec when a type is both
 * written and read as JSON: a `JsonCodec[A]` in implicit scope also supplies the `JsonEncoder[A]` and the
 * `JsonDecoder[A]` that `toJson` and `fromJson` need. Get one with `DeriveJsonCodec.gen`, build one with
 * `JsonCodec(encoder, decoder)`, or make one from an existing codec with [[JsonCodec#transform]] or
 * [[JsonCodec#transformOrFail]].
 *
 * A codec should round-trip: decoding the JSON that its encoder writes for a value gives that value back.
 *
 * {{{
 * final case class Order(id: Long, total: BigDecimal)
 *
 * object Order {
 *   implicit val codec: JsonCodec[Order] = DeriveJsonCodec.gen[Order]
 * }
 *
 * val json: String = Order(7L, BigDecimal("19.99")).toJson
 * // {"id":7,"total":19.99}
 *
 * val decoded = """{"id":7,"total":19.99}""".fromJson[Order]
 * // Right(Order(7,19.99))
 * }}}
 *
 * @param encoder
 *   the encoder half, used by `encodeJson`
 * @param decoder
 *   the decoder half, used by `decodeJson`
 * @see
 *   [[https://zio.dev/zio-json/getting-started/ Getting Started with ZIO Json]]
 * @see
 *   [[https://zio.dev/zio-json/manual-instances/ Manual Instances]]
 */
final case class JsonCodec[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A]) { self =>

  /**
   * An alias for [[JsonCodec#orElse]].
   */
  final def <>(that: => JsonCodec[A]): JsonCodec[A] = self.orElse(that)

  /**
   * An alias for [[JsonCodec#orElseEither]].
   */
  final def <+>[B](that: => JsonCodec[B]): JsonCodec[Either[A, B]] = self.orElseEither(that)

  /**
   * An alias for [[JsonCodec#zip]].
   */
  final def <*>[B](that: => JsonCodec[B]): JsonCodec[(A, B)] =
    JsonCodec(self.encoder.zip(that.encoder), self.decoder.zip(that.decoder))

  /**
   * An alias for [[JsonCodec#zipRight]].
   */
  final def *>[B](that: => JsonCodec[B])(implicit ev: Unit <:< A): JsonCodec[B] = self.zipRight(that)

  /**
   * An alias for [[JsonCodec#zipLeft]].
   */
  final def <*(that: => JsonCodec[Unit]): JsonCodec[A] = self.zipLeft(that)

  /**
   * Decodes `str` with `decoder`. Returns `Right` with the value, or `Left` with the path of the failing value followed
   * by the reason, such as `.total(missing)`, or `Unexpected end of input` when the text ends in the middle of a value.
   */
  final def decodeJson(str: CharSequence): Either[String, A] = decoder.decodeJson(str)

  /**
   * Encodes `a` with `encoder`. Unlike `JsonEncoder#encodeJson`, `indent` has no default: pass `None` for compact
   * output, or `Some(0)` for pretty output with two spaces per nesting level.
   */
  final def encodeJson(a: A, indent: Option[Int]): CharSequence = encoder.encodeJson(a, indent)

  /**
   * Returns a codec that decodes with this codec and, when that fails, with `that`. Encoding uses the encoder of `that`
   * for every value, not the encoder of this codec.
   */
  final def orElse(that: => JsonCodec[A]): JsonCodec[A] =
    self.orElseEither[A](that).transform(_.merge, Right(_))

  /**
   * Returns a codec for `Either[A, B]` that decodes with this codec into a `Left` and, when that fails, with `that`
   * into a `Right`. Its encoder writes a `Left` with this codec and a `Right` with `that`, with no `Left` or `Right`
   * field around the value.
   *
   * {{{
   * val idOrName: JsonCodec[Either[Int, String]] = JsonCodec[Int].orElseEither(JsonCodec[String])
   *
   * val byId   = idOrName.decodeJson("42")      // Right(Left(42))
   * val byName = idOrName.decodeJson("\"ada\"") // Right(Right(ada))
   * val json   = idOrName.encodeJson(Right("ada"), None).toString // "ada"
   * }}}
   */
  final def orElseEither[B](that: => JsonCodec[B]): JsonCodec[Either[A, B]] =
    JsonCodec.orElseEither(self, that)

  /**
   * Returns a codec for `B` that decodes with this codec and converts the result with `f`, and encodes by converting
   * with `g` and then using this codec.
   *
   * {{{
   * final case class Sku(code: String)
   *
   * val skuCodec: JsonCodec[Sku] = JsonCodec[String].transform[Sku](Sku(_), _.code)
   *
   * val json    = skuCodec.encodeJson(Sku("A-1"), None).toString // "A-1"
   * val decoded = skuCodec.decodeJson("\"A-1\"")                  // Right(Sku(A-1))
   * }}}
   */
  final def transform[B](f: A => B, g: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(g), decoder.map(f))

  /**
   * Returns a codec for `B` like [[JsonCodec#transform]], but `f` can reject a decoded value by returning
   * `Left(message)`; decoding then fails with that message at the path of the value. `g` cannot fail.
   *
   * {{{
   * final case class Email(value: String)
   *
   * val emailCodec: JsonCodec[Email] = JsonCodec[String].transformOrFail[Email](
   *   s => if (s.contains("@")) Right(Email(s)) else Left("not an email address"),
   *   _.value
   * )
   *
   * val accepted = emailCodec.decodeJson("\"ada@example.com\"") // Right(Email(ada@example.com))
   * val rejected = emailCodec.decodeJson("\"ada\"")             // Left((not an email address))
   * }}}
   */
  final def transformOrFail[B](f: A => Either[String, B], g: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(g), decoder.mapOrFail(f))

  /**
   * Returns a codec for the pair `(A, B)` as a two-element JSON array: the first element with this codec, the second
   * with `that`. `("apples", 3)` is written as `["apples",3]`.
   */
  final def zip[B](that: => JsonCodec[B]): JsonCodec[(A, B)] =
    JsonCodec(self.encoder.zip(that.encoder), self.decoder.zip(that.decoder))

  /**
   * Returns a codec for `B` that keeps only the second element of [[JsonCodec#zip]]. It is available when this codec's
   * type is `Unit`: it writes the encoding of `()` in the first slot of the array, and when decoding it reads the first
   * element with this codec's decoder and drops the result. With a `Unit` codec built from `JsonEncoder.unit` and
   * `JsonDecoder.unit`, `3` is written as `[{},3]` and any first element is accepted.
   */
  final def zipRight[B](that: => JsonCodec[B])(implicit ev: Unit <:< A): JsonCodec[B] =
    self.zip(that).transform((t: (A, B)) => t._2, b => (ev(()), b))

  /**
   * Returns a codec for `A` that keeps only the first element of [[JsonCodec#zip]] with a `Unit` codec: it writes the
   * encoding of `()` in the second slot of the array, and when decoding it reads the second element with the decoder of
   * `that` and drops the result. With a `Unit` codec built from `JsonEncoder.unit` and `JsonDecoder.unit`, `3` is
   * written as `[3,{}]` and any second element is accepted.
   */
  final def zipLeft(that: => JsonCodec[Unit]): JsonCodec[A] =
    self.zip(that).transform((t: (A, Unit)) => t._1, a => (a, ()))

}

/**
 * Built-in [[JsonCodec]] instances and constructors. The implicit instances cover the primitive types, `Option`,
 * `Either`, tuples of 1 to 22 elements, the common collections and maps, the `java.time` types, `UUID` and `Currency`,
 * and are found without an import. A few types that have an encoder and a decoder have no codec, among them `Unit`,
 * `IndexedSeq` and `mutable.Map`; build one with `JsonCodec(JsonEncoder[T], JsonDecoder[T])`.
 */
object JsonCodec extends GeneratedTupleCodecs with CodecLowPriority0 with JsonCodecVersionSpecific {

  /**
   * Returns the implicit `JsonCodec[A]` in scope. `JsonCodec(encoder, decoder)`, with two arguments, is the case-class
   * constructor instead.
   */
  def apply[A](implicit jsonCodec: JsonCodec[A]): JsonCodec[A] = jsonCodec

  /**
   * Returns `JsonCodec(encoder, decoder)`. It is declared `implicit`, but its parameters are not, so the compiler does
   * not use it to build a codec from an encoder and a decoder in scope.
   */
  implicit def fromEncoderDecoder[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] =
    JsonCodec(encoder, decoder)

  private def orElseEither[A, B](A: JsonCodec[A], B: JsonCodec[B]): JsonCodec[Either[A, B]] =
    JsonCodec(
      JsonEncoder.orElseEither[A, B](A.encoder, B.encoder),
      A.decoder
        .map(x => Left(x): Either[A, B])
        .orElse(B.decoder.map(x => Right(x): Either[A, B]))
    )

  /**
   * Codec for `String`, as a JSON string.
   */
  implicit val string: JsonCodec[String] = JsonCodec(JsonEncoder.string, JsonDecoder.string)

  /**
   * Codec for `Boolean`, as `true` or `false`.
   */
  implicit val boolean: JsonCodec[Boolean] = JsonCodec(JsonEncoder.boolean, JsonDecoder.boolean)

  /**
   * Codec for `Char`, as a JSON string of one character. Decoding a longer or an empty string fails with
   * `(expected single character string)`.
   */
  implicit val char: JsonCodec[Char] = JsonCodec(JsonEncoder.char, JsonDecoder.char)

  /**
   * Codec for `Long`, as a JSON number. Decoding a number out of range fails with `(expected a Long)`.
   */
  implicit val long: JsonCodec[Long] = JsonCodec(JsonEncoder.long, JsonDecoder.long)

  /**
   * Codec for `Symbol`, as a JSON string that holds its name.
   */
  implicit val symbol: JsonCodec[Symbol] = JsonCodec(JsonEncoder.symbol, JsonDecoder.symbol)

  /**
   * Codec for `Byte`, as a JSON number. Decoding a number out of range, such as `128`, fails with `(expected a Byte)`.
   */
  implicit val byte: JsonCodec[Byte] = JsonCodec(JsonEncoder.byte, JsonDecoder.byte)

  /**
   * Codec for `Short`, as a JSON number. Decoding a number out of range fails with `(expected a Short)`.
   */
  implicit val short: JsonCodec[Short] = JsonCodec(JsonEncoder.short, JsonDecoder.short)

  /**
   * Codec for `Int`, as a JSON number. The decoder also accepts a number in a string, such as `"42"`.
   */
  implicit val int: JsonCodec[Int] = JsonCodec(JsonEncoder.int, JsonDecoder.int)

  /**
   * Codec for `java.math.BigInteger`, as a JSON number.
   */
  implicit val bigInteger: JsonCodec[java.math.BigInteger] = JsonCodec(JsonEncoder.bigInteger, JsonDecoder.bigInteger)

  /**
   * Codec for `BigInt`, as a JSON number.
   */
  implicit val scalaBigInt: JsonCodec[BigInt] = JsonCodec(JsonEncoder.scalaBigInt, JsonDecoder.scalaBigInt)

  /**
   * Codec for `Double`, as a JSON number, with `NaN` and the infinities as the strings `"NaN"`, `"Infinity"` and
   * `"-Infinity"`. Decoding a number too large for a `Double`, such as `1e400`, gives `Infinity`.
   */
  implicit val double: JsonCodec[Double] = JsonCodec(JsonEncoder.double, JsonDecoder.double)

  /**
   * Codec for `Float`, as a JSON number, with `NaN` and the infinities as the strings `"NaN"`, `"Infinity"` and
   * `"-Infinity"`.
   */
  implicit val float: JsonCodec[Float] = JsonCodec(JsonEncoder.float, JsonDecoder.float)

  /**
   * Codec for `java.math.BigDecimal`, as a JSON number that keeps its scale: `1.50` stays `1.50`.
   */
  implicit val bigDecimal: JsonCodec[java.math.BigDecimal] = JsonCodec(JsonEncoder.bigDecimal, JsonDecoder.bigDecimal)

  /**
   * Codec for `BigDecimal`, as a JSON number that keeps its scale: `BigDecimal("1.50")` stays `1.50`.
   */
  implicit val scalaBigDecimal: JsonCodec[BigDecimal] =
    JsonCodec(JsonEncoder.scalaBigDecimal, JsonDecoder.scalaBigDecimal)

  /**
   * Codec for `Option[A]`: `None` is written as `null`, and `null` decodes as `None`.
   */
  implicit def option[A: JsonEncoder: JsonDecoder]: JsonCodec[Option[A]] =
    JsonCodec(JsonEncoder.option(JsonEncoder[A]), JsonDecoder.option(JsonDecoder[A]))

  /**
   * Codec for `Either[A, B]`, as `{"Left":a}` or `{"Right":b}`. The decoder also accepts the keys `left` and `a` for a
   * `Left`, and `right` and `b` for a `Right`.
   */
  implicit def either[A: JsonEncoder: JsonDecoder, B: JsonEncoder: JsonDecoder]: JsonCodec[Either[A, B]] =
    JsonCodec(JsonEncoder.either(JsonEncoder[A], JsonEncoder[B]), JsonDecoder.either(JsonDecoder[A], JsonDecoder[B]))
}

private[json] trait CodecLowPriority0 extends CodecLowPriority1 { this: JsonCodec.type =>

  /**
   * Codec for `Array[A]`, as a JSON array. It needs a `ClassTag` for the element type.
   */
  implicit def array[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[Array[A]] =
    JsonCodec(JsonEncoder.array[A], JsonDecoder.array[A])

  /**
   * Codec for `zio.Chunk[A]`, as a JSON array.
   */
  implicit def chunk[A: JsonEncoder: JsonDecoder]: JsonCodec[Chunk[A]] =
    JsonCodec(JsonEncoder.chunk[A], JsonDecoder.chunk[A])

  /**
   * Codec for `zio.NonEmptyChunk[A]`, as a JSON array. Decoding an empty array fails with `(Chunk was empty)`.
   */
  implicit def nonEmptyChunk[A: JsonEncoder: JsonDecoder]: JsonCodec[NonEmptyChunk[A]] =
    JsonCodec(JsonEncoder.nonEmptyChunk[A], JsonDecoder.nonEmptyChunk[A])

  /**
   * Codec for `immutable.HashSet[A]`, as a JSON array. Repeated elements in the input collapse into one.
   */
  implicit def hashSet[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.HashSet[A]] =
    JsonCodec(JsonEncoder.hashSet[A], JsonDecoder.hashSet[A])

  /**
   * Codec for `immutable.HashMap[K, V]`, as a JSON object, in the same way as the `Map` codec.
   */
  implicit def hashMap[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]
    : JsonCodec[immutable.HashMap[K, V]] =
    JsonCodec(JsonEncoder.hashMap[K, V], JsonDecoder.hashMap[K, V])
}

private[json] trait CodecLowPriority1 extends CodecLowPriority2 { this: JsonCodec.type =>

  /**
   * Codec for `Seq[A]`, as a JSON array. The decoder builds a `List`.
   */
  implicit def seq[A: JsonEncoder: JsonDecoder]: JsonCodec[Seq[A]] = JsonCodec(JsonEncoder.seq[A], JsonDecoder.seq[A])

  /**
   * Codec for `List[A]`, as a JSON array.
   */
  implicit def list[A: JsonEncoder: JsonDecoder]: JsonCodec[List[A]] =
    JsonCodec(JsonEncoder.list[A], JsonDecoder.list[A])

  /**
   * Codec for `Vector[A]`, as a JSON array.
   */
  implicit def vector[A: JsonEncoder: JsonDecoder]: JsonCodec[Vector[A]] =
    JsonCodec(JsonEncoder.vector[A], JsonDecoder.vector[A])

  /**
   * Codec for `Set[A]`, as a JSON array. Repeated elements in the input collapse into one.
   */
  implicit def set[A: JsonEncoder: JsonDecoder]: JsonCodec[Set[A]] = JsonCodec(JsonEncoder.set[A], JsonDecoder.set[A])

  /**
   * Codec for `Map[K, V]`, as a JSON object, with the `JsonFieldEncoder` and `JsonFieldDecoder` for the keys. Entries
   * whose value is missing, such as `None`, are left out when encoding. When decoding a repeated key, the last value
   * wins.
   */
  implicit def map[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]: JsonCodec[Map[K, V]] =
    JsonCodec(JsonEncoder.map[K, V], JsonDecoder.map[K, V])

  /**
   * Codec for `collection.SortedMap[K, V]`, as a JSON object, in the same way as the `Map` codec. It needs an
   * `Ordering[K]`, and writes the keys in sorted order.
   */
  implicit def sortedMap[
    K: JsonFieldEncoder: JsonFieldDecoder: Ordering,
    V: JsonEncoder: JsonDecoder
  ]: JsonCodec[collection.SortedMap[K, V]] =
    JsonCodec(JsonEncoder.sortedMap[K, V], JsonDecoder.sortedMap[K, V])

  /**
   * Codec for `immutable.SortedSet[A]`, as a JSON array in sorted order. It needs an `Ordering[A]`.
   */
  implicit def sortedSet[A: Ordering: JsonEncoder: JsonDecoder]: JsonCodec[immutable.SortedSet[A]] =
    JsonCodec(JsonEncoder.sortedSet[A], JsonDecoder.sortedSet[A])

  /**
   * Codec for `immutable.ListMap[K, V]`, as a JSON object, in the same way as the `Map` codec. It keeps the order of
   * the keys in both directions.
   */
  implicit def listMap[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]
    : JsonCodec[immutable.ListMap[K, V]] =
    JsonCodec(JsonEncoder.listMap[K, V], JsonDecoder.listMap[K, V])
}

private[json] trait CodecLowPriority2 extends CodecLowPriority3 { this: JsonCodec.type =>

  /**
   * Codec for `Iterable[A]`, as a JSON array. The decoder builds a `List`.
   */
  implicit def iterable[A: JsonEncoder: JsonDecoder]: JsonCodec[Iterable[A]] =
    JsonCodec(JsonEncoder.iterable[A, Iterable], JsonDecoder.iterable)
}

private[json] trait CodecLowPriority3 { this: JsonCodec.type =>
  import java.time._

  /**
   * Codec for `java.time.DayOfWeek`, as its upper-case name, such as `"MONDAY"`. The decoder ignores case.
   */
  implicit val dayOfWeek: JsonCodec[DayOfWeek] = JsonCodec(JsonEncoder.dayOfWeek, JsonDecoder.dayOfWeek)

  /**
   * Codec for `java.time.Duration`, as an ISO-8601 string such as `"PT1H30M"`.
   */
  implicit val duration: JsonCodec[Duration] = JsonCodec(JsonEncoder.duration, JsonDecoder.duration)

  /**
   * Codec for `java.time.Instant`, as an ISO-8601 string in UTC such as `"2024-01-02T03:04:05Z"`. The decoder also
   * accepts an offset, such as `+02:00`, and converts the time to UTC.
   */
  implicit val instant: JsonCodec[Instant] = JsonCodec(JsonEncoder.instant, JsonDecoder.instant)

  /**
   * Codec for `java.time.LocalDate`, as an ISO-8601 string such as `"2024-01-02"`.
   */
  implicit val localDate: JsonCodec[LocalDate] = JsonCodec(JsonEncoder.localDate, JsonDecoder.localDate)

  /**
   * Codec for `java.time.LocalDateTime`, as an ISO-8601 string such as `"2024-01-02T03:04:00"`.
   */
  implicit val localDateTime: JsonCodec[LocalDateTime] = JsonCodec(JsonEncoder.localDateTime, JsonDecoder.localDateTime)

  /**
   * Codec for `java.time.LocalTime`, as an ISO-8601 string such as `"03:04:00"`.
   */
  implicit val localTime: JsonCodec[LocalTime] = JsonCodec(JsonEncoder.localTime, JsonDecoder.localTime)

  /**
   * Codec for `java.time.Month`, as its upper-case name, such as `"JANUARY"`. The decoder ignores case.
   */
  implicit val month: JsonCodec[Month] = JsonCodec(JsonEncoder.month, JsonDecoder.month)

  /**
   * Codec for `java.time.MonthDay`, as an ISO-8601 string such as `"--12-25"`.
   */
  implicit val monthDay: JsonCodec[MonthDay] = JsonCodec(JsonEncoder.monthDay, JsonDecoder.monthDay)

  /**
   * Codec for `java.time.OffsetDateTime`, as an ISO-8601 string such as `"2024-01-02T03:04:05+02:00"`.
   */
  implicit val offsetDateTime: JsonCodec[OffsetDateTime] =
    JsonCodec(JsonEncoder.offsetDateTime, JsonDecoder.offsetDateTime)

  /**
   * Codec for `java.time.OffsetTime`, as an ISO-8601 string such as `"03:04:05+02:00"`.
   */
  implicit val offsetTime: JsonCodec[OffsetTime] = JsonCodec(JsonEncoder.offsetTime, JsonDecoder.offsetTime)

  /**
   * Codec for `java.time.Period`, as an ISO-8601 string such as `"P1Y2M3D"`. The decoder also accepts weeks: `"P1W"`
   * decodes as 7 days.
   */
  implicit val period: JsonCodec[Period] = JsonCodec(JsonEncoder.period, JsonDecoder.period)

  /**
   * Codec for `java.time.Year`, as a JSON string such as `"2024"`. Decoding the number `2024` fails with
   * `(expected a Year)`.
   */
  implicit val year: JsonCodec[Year] = JsonCodec(JsonEncoder.year, JsonDecoder.year)

  /**
   * Codec for `java.time.YearMonth`, as an ISO-8601 string such as `"2024-01"`.
   */
  implicit val yearMonth: JsonCodec[YearMonth] = JsonCodec(JsonEncoder.yearMonth, JsonDecoder.yearMonth)

  /**
   * Codec for `java.time.ZonedDateTime`, as an ISO-8601 string with the offset and the zone id, such as
   * `"2024-01-02T03:04:05+01:00[Europe/Paris]"`.
   */
  implicit val zonedDateTime: JsonCodec[ZonedDateTime] = JsonCodec(JsonEncoder.zonedDateTime, JsonDecoder.zonedDateTime)

  /**
   * Codec for `java.time.ZoneId`, as its id, such as `"Europe/Paris"`.
   */
  implicit val zoneId: JsonCodec[ZoneId] = JsonCodec(JsonEncoder.zoneId, JsonDecoder.zoneId)

  /**
   * Codec for `java.time.ZoneOffset`, as a JSON string such as `"+02:00"`, or `"Z"` for UTC.
   */
  implicit val zoneOffset: JsonCodec[ZoneOffset] = JsonCodec(JsonEncoder.zoneOffset, JsonDecoder.zoneOffset)

  /**
   * Codec for `java.util.UUID`, as a JSON string in lower-case canonical form. The decoder also accepts non-canonical
   * forms with shorter groups, as `UUID.fromString` does: `"1-2-3-4-5"` decodes as
   * `00000001-0002-0003-0004-000000000005`.
   */
  implicit val uuid: JsonCodec[java.util.UUID] = JsonCodec(JsonEncoder.uuid, JsonDecoder.uuid)

  /**
   * Codec for `java.util.Currency`, as its ISO 4217 code such as `"EUR"`. The decoder is case-sensitive: `"eur"` fails
   * with `(expected a Currency)`.
   */
  implicit val currency: JsonCodec[java.util.Currency] = JsonCodec(JsonEncoder.currency, JsonDecoder.currency)
}
