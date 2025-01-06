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
 * A `JsonCodec[A]` instance has the ability to encode values of type `A` into JSON, together with the ability to decode
 * such JSON into values of type `A`.
 *
 * Instances of this trait should satisfy round-tripping laws: that is, for every value, instances must be able to
 * successfully encode the value into JSON, and then successfully decode the same value from such JSON.
 *
 * For more information, see [[JsonDecoder]] and [[JsonEncoder]].
 *
 * {{ val intCodec: JsonCodec[Int] = JsonCodec[Int]
 *
 * intCodec.encodeJson(intCodec.encodeJson(42)) == Right(42) }}
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

  final def *>[B](that: => JsonCodec[B])(implicit ev: Unit <:< A): JsonCodec[B] = self.zipRight(that)

  final def <*(that: => JsonCodec[Unit]): JsonCodec[A] = self.zipLeft(that)

  final def decodeJson(str: CharSequence): Either[String, A] = decoder.decodeJson(str)

  final def encodeJson(a: A, indent: Option[Int]): CharSequence = encoder.encodeJson(a, indent)

  final def orElse(that: => JsonCodec[A]): JsonCodec[A] =
    self.orElseEither[A](that).transform(_.merge, Right(_))

  final def orElseEither[B](that: => JsonCodec[B]): JsonCodec[Either[A, B]] =
    JsonCodec.orElseEither(self, that)

  final def transform[B](f: A => B, g: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(g), decoder.map(f))

  final def transformOrFail[B](f: A => Either[String, B], g: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(g), decoder.mapOrFail(f))

  final def zip[B](that: => JsonCodec[B]): JsonCodec[(A, B)] =
    JsonCodec(self.encoder.zip(that.encoder), self.decoder.zip(that.decoder))

  final def zipRight[B](that: => JsonCodec[B])(implicit ev: Unit <:< A): JsonCodec[B] =
    self.zip(that).transform((t: (A, B)) => t._2, b => (ev(()), b))

  final def zipLeft(that: => JsonCodec[Unit]): JsonCodec[A] =
    self.zip(that).transform((t: (A, Unit)) => t._1, a => (a, ()))

}

object JsonCodec extends GeneratedTupleCodecs with CodecLowPriority0 with JsonCodecVersionSpecific {
  def apply[A](implicit jsonCodec: JsonCodec[A]): JsonCodec[A] = jsonCodec

  implicit def fromEncoderDecoder[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] =
    JsonCodec(encoder, decoder)

  private def orElseEither[A, B](A: JsonCodec[A], B: JsonCodec[B]): JsonCodec[Either[A, B]] =
    JsonCodec(
      JsonEncoder.orElseEither[A, B](A.encoder, B.encoder),
      A.decoder
        .map(x => Left(x): Either[A, B])
        .orElse(B.decoder.map(x => Right(x): Either[A, B]))
    )

  implicit val string: JsonCodec[String]                   = JsonCodec(JsonEncoder.string, JsonDecoder.string)
  implicit val boolean: JsonCodec[Boolean]                 = JsonCodec(JsonEncoder.boolean, JsonDecoder.boolean)
  implicit val char: JsonCodec[Char]                       = JsonCodec(JsonEncoder.char, JsonDecoder.char)
  implicit val long: JsonCodec[Long]                       = JsonCodec(JsonEncoder.long, JsonDecoder.long)
  implicit val symbol: JsonCodec[Symbol]                   = JsonCodec(JsonEncoder.symbol, JsonDecoder.symbol)
  implicit val byte: JsonCodec[Byte]                       = JsonCodec(JsonEncoder.byte, JsonDecoder.byte)
  implicit val short: JsonCodec[Short]                     = JsonCodec(JsonEncoder.short, JsonDecoder.short)
  implicit val int: JsonCodec[Int]                         = JsonCodec(JsonEncoder.int, JsonDecoder.int)
  implicit val bigInteger: JsonCodec[java.math.BigInteger] = JsonCodec(JsonEncoder.bigInteger, JsonDecoder.bigInteger)
  implicit val scalaBigInt: JsonCodec[BigInt]              = JsonCodec(JsonEncoder.scalaBigInt, JsonDecoder.scalaBigInt)
  implicit val double: JsonCodec[Double]                   = JsonCodec(JsonEncoder.double, JsonDecoder.double)
  implicit val float: JsonCodec[Float]                     = JsonCodec(JsonEncoder.float, JsonDecoder.float)
  implicit val bigDecimal: JsonCodec[java.math.BigDecimal] = JsonCodec(JsonEncoder.bigDecimal, JsonDecoder.bigDecimal)

  implicit val scalaBigDecimal: JsonCodec[BigDecimal] =
    JsonCodec(JsonEncoder.scalaBigDecimal, JsonDecoder.scalaBigDecimal)

  implicit def option[A: JsonEncoder: JsonDecoder]: JsonCodec[Option[A]] =
    JsonCodec(JsonEncoder.option(JsonEncoder[A]), JsonDecoder.option(JsonDecoder[A]))

  implicit def either[A: JsonEncoder: JsonDecoder, B: JsonEncoder: JsonDecoder]: JsonCodec[Either[A, B]] =
    JsonCodec(JsonEncoder.either(JsonEncoder[A], JsonEncoder[B]), JsonDecoder.either(JsonDecoder[A], JsonDecoder[B]))
}

private[json] trait CodecLowPriority0 extends CodecLowPriority1 { this: JsonCodec.type =>

  implicit def chunk[A: JsonEncoder: JsonDecoder]: JsonCodec[Chunk[A]] =
    JsonCodec(JsonEncoder.chunk[A], JsonDecoder.chunk[A])

  implicit def nonEmptyChunk[A: JsonEncoder: JsonDecoder]: JsonCodec[NonEmptyChunk[A]] =
    JsonCodec(JsonEncoder.nonEmptyChunk[A], JsonDecoder.nonEmptyChunk[A])

  implicit def hashSet[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.HashSet[A]] =
    JsonCodec(JsonEncoder.hashSet[A], JsonDecoder.hashSet[A])

  implicit def hashMap[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]
    : JsonCodec[immutable.HashMap[K, V]] =
    JsonCodec(JsonEncoder.hashMap[K, V], JsonDecoder.hashMap[K, V])
}

private[json] trait CodecLowPriority1 extends CodecLowPriority2 { this: JsonCodec.type =>
  implicit def seq[A: JsonEncoder: JsonDecoder]: JsonCodec[Seq[A]] = JsonCodec(JsonEncoder.seq[A], JsonDecoder.seq[A])
  implicit def list[A: JsonEncoder: JsonDecoder]: JsonCodec[List[A]] =
    JsonCodec(JsonEncoder.list[A], JsonDecoder.list[A])
  implicit def vector[A: JsonEncoder: JsonDecoder]: JsonCodec[Vector[A]] =
    JsonCodec(JsonEncoder.vector[A], JsonDecoder.vector[A])
  implicit def set[A: JsonEncoder: JsonDecoder]: JsonCodec[Set[A]] = JsonCodec(JsonEncoder.set[A], JsonDecoder.set[A])

  implicit def map[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]: JsonCodec[Map[K, V]] =
    JsonCodec(JsonEncoder.map[K, V], JsonDecoder.map[K, V])

  implicit def sortedMap[
    K: JsonFieldEncoder: JsonFieldDecoder: Ordering,
    V: JsonEncoder: JsonDecoder
  ]: JsonCodec[collection.SortedMap[K, V]] =
    JsonCodec(JsonEncoder.sortedMap[K, V], JsonDecoder.sortedMap[K, V])

  implicit def sortedSet[A: Ordering: JsonEncoder: JsonDecoder]: JsonCodec[immutable.SortedSet[A]] =
    JsonCodec(JsonEncoder.sortedSet[A], JsonDecoder.sortedSet[A])

  implicit def listMap[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]
    : JsonCodec[immutable.ListMap[K, V]] =
    JsonCodec(JsonEncoder.listMap[K, V], JsonDecoder.listMap[K, V])
}

private[json] trait CodecLowPriority2 extends CodecLowPriority3 { this: JsonCodec.type =>

  implicit def iterable[A: JsonEncoder: JsonDecoder]: JsonCodec[Iterable[A]] =
    JsonCodec(JsonEncoder.iterable[A, Iterable], JsonDecoder.iterable)
}

private[json] trait CodecLowPriority3 { this: JsonCodec.type =>
  import java.time._

  implicit val dayOfWeek: JsonCodec[DayOfWeek]         = JsonCodec(JsonEncoder.dayOfWeek, JsonDecoder.dayOfWeek)
  implicit val duration: JsonCodec[Duration]           = JsonCodec(JsonEncoder.duration, JsonDecoder.duration)
  implicit val instant: JsonCodec[Instant]             = JsonCodec(JsonEncoder.instant, JsonDecoder.instant)
  implicit val localDate: JsonCodec[LocalDate]         = JsonCodec(JsonEncoder.localDate, JsonDecoder.localDate)
  implicit val localDateTime: JsonCodec[LocalDateTime] = JsonCodec(JsonEncoder.localDateTime, JsonDecoder.localDateTime)
  implicit val localTime: JsonCodec[LocalTime]         = JsonCodec(JsonEncoder.localTime, JsonDecoder.localTime)
  implicit val month: JsonCodec[Month]                 = JsonCodec(JsonEncoder.month, JsonDecoder.month)
  implicit val monthDay: JsonCodec[MonthDay]           = JsonCodec(JsonEncoder.monthDay, JsonDecoder.monthDay)

  implicit val offsetDateTime: JsonCodec[OffsetDateTime] =
    JsonCodec(JsonEncoder.offsetDateTime, JsonDecoder.offsetDateTime)

  implicit val offsetTime: JsonCodec[OffsetTime]       = JsonCodec(JsonEncoder.offsetTime, JsonDecoder.offsetTime)
  implicit val period: JsonCodec[Period]               = JsonCodec(JsonEncoder.period, JsonDecoder.period)
  implicit val year: JsonCodec[Year]                   = JsonCodec(JsonEncoder.year, JsonDecoder.year)
  implicit val yearMonth: JsonCodec[YearMonth]         = JsonCodec(JsonEncoder.yearMonth, JsonDecoder.yearMonth)
  implicit val zonedDateTime: JsonCodec[ZonedDateTime] = JsonCodec(JsonEncoder.zonedDateTime, JsonDecoder.zonedDateTime)
  implicit val zoneId: JsonCodec[ZoneId]               = JsonCodec(JsonEncoder.zoneId, JsonDecoder.zoneId)
  implicit val zoneOffset: JsonCodec[ZoneOffset]       = JsonCodec(JsonEncoder.zoneOffset, JsonDecoder.zoneOffset)

  implicit val uuid: JsonCodec[java.util.UUID] = JsonCodec(JsonEncoder.uuid, JsonDecoder.uuid)

  implicit val currency: JsonCodec[java.util.Currency] = JsonCodec(JsonEncoder.currency, JsonDecoder.currency)
}
