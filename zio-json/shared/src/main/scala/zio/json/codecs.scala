package zio.json

import scala.collection.immutable

import zio.Chunk
import zio.json.JsonDecoder.JsonError
import zio.json.internal._

/**
 * A `JsonCodec[A]` instance has the ability to encode values of type `A` into JSON, together with
 * the ability to decode such JSON into values of type `A`.
 *
 * The trait is the intersection composition of `JsonDecoder[A]` and `JsonEncoder[A]`, and
 * instances should satisfy round-tripping laws: that is, for every value, instances must be able
 * to successfully encode the value into JSON, and then successfully decode the same value from
 * such JSON.
 *
 * For more information, see [[JsonDecoder]] and [[JsonEncoder]].
 *
 * {{
 * val intCodec: JsonCodec[Int] = JsonCodec[Int]
 *
 * intCodec.encodeJson(intCodec.encodeJson(42)) == Right(42)
 * }}
 */
trait JsonCodec[A] extends JsonDecoder[A] with JsonEncoder[A] {
  def encoder: JsonEncoder[A]
  def decoder: JsonDecoder[A]

  override def xmap[B](f: A => B, g: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(g), decoder.map(f))
}

object JsonCodec extends GeneratedTupleCodecs with CodecLowPriority0 {
  def apply[A](implicit jsonCodec: JsonCodec[A]): JsonCodec[A] = jsonCodec

  implicit val string: JsonCodec[String]                   = JsonCodec(JsonEncoder.string, JsonDecoder.string)
  implicit val boolean: JsonCodec[Boolean]                 = JsonCodec(JsonEncoder.boolean, JsonDecoder.boolean)
  implicit val char: JsonCodec[Char]                       = JsonCodec(JsonEncoder.char, JsonDecoder.char)
  implicit val long: JsonCodec[Long]                       = JsonCodec(JsonEncoder.long, JsonDecoder.long)
  implicit val symbol: JsonCodec[Symbol]                   = JsonCodec(JsonEncoder.symbol, JsonDecoder.symbol)
  implicit val byte: JsonCodec[Byte]                       = JsonCodec(JsonEncoder.byte, JsonDecoder.byte)
  implicit val short: JsonCodec[Short]                     = JsonCodec(JsonEncoder.short, JsonDecoder.short)
  implicit val int: JsonCodec[Int]                         = JsonCodec(JsonEncoder.int, JsonDecoder.int)
  implicit val bigInteger: JsonCodec[java.math.BigInteger] = JsonCodec(JsonEncoder.bigInteger, JsonDecoder.bigInteger)
  implicit val double: JsonCodec[Double]                   = JsonCodec(JsonEncoder.double, JsonDecoder.double)
  implicit val float: JsonCodec[Float]                     = JsonCodec(JsonEncoder.float, JsonDecoder.float)
  implicit val bigDecimal: JsonCodec[java.math.BigDecimal] = JsonCodec(JsonEncoder.bigDecimal, JsonDecoder.bigDecimal)

  implicit val scalaBigDecimal: JsonCodec[BigDecimal] =
    JsonCodec(JsonEncoder.scalaBigDecimal, JsonDecoder.scalaBigDecimal)

  implicit def option[A](implicit c: JsonCodec[A]): JsonCodec[Option[A]] =
    JsonCodec(JsonEncoder.option(c.encoder), JsonDecoder.option(c.decoder))

  implicit def either[A, B](implicit ac: JsonCodec[A], bc: JsonCodec[B]): JsonCodec[Either[A, B]] =
    JsonCodec(JsonEncoder.either(ac.encoder, bc.encoder), JsonDecoder.either(ac.decoder, bc.decoder))

  /**
   * Derives a `JsonCodec[A]` from an encoder and a decoder.
   */
  implicit def apply[A](implicit encoder0: JsonEncoder[A], decoder0: JsonDecoder[A]): JsonCodec[A] =
    (encoder0, decoder0) match {
      case (e: JsonCodec[_], d: JsonCodec[_]) =>
        // protects against cycles in implicit resolution, unfortunately the
        // instantiation of decoder0 could have been wasteful.
        e
      case other =>
        new JsonCodec[A] {
          override def encoder: JsonEncoder[A] = encoder0

          override def decoder: JsonDecoder[A] = decoder0

          override def unsafeDecodeMissing(trace: List[JsonError]): A =
            decoder0.unsafeDecodeMissing(trace)

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
            decoder0.unsafeDecode(trace, in)

          override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
            encoder0.unsafeEncode(a, indent, out)
        }
    }
}

private[json] trait CodecLowPriority0 extends CodecLowPriority1 { this: JsonCodec.type =>

  implicit def chunk[A: JsonCodec]: JsonCodec[Chunk[A]] =
    JsonCodec(JsonEncoder.chunk[A], JsonDecoder.chunk[A])

  implicit def hashSet[A: JsonCodec]: JsonCodec[immutable.HashSet[A]] =
    JsonCodec(JsonEncoder.hashSet[A], JsonDecoder.hashSet[A])

  implicit def hashMap[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonCodec]: JsonEncoder[immutable.HashMap[K, V]] =
    JsonCodec(JsonEncoder.hashMap[K, V], JsonDecoder.hashMap[K, V])
}

private[json] trait CodecLowPriority1 extends CodecLowPriority2 { this: JsonCodec.type =>
  implicit def seq[A: JsonCodec]: JsonCodec[Seq[A]]       = JsonCodec(JsonEncoder.seq[A], JsonDecoder.seq[A])
  implicit def list[A: JsonCodec]: JsonCodec[List[A]]     = JsonCodec(JsonEncoder.list[A], JsonDecoder.list[A])
  implicit def vector[A: JsonCodec]: JsonCodec[Vector[A]] = JsonCodec(JsonEncoder.vector[A], JsonDecoder.vector[A])
  implicit def set[A: JsonCodec]: JsonCodec[Set[A]]       = JsonCodec(JsonEncoder.set[A], JsonDecoder.set[A])

  implicit def map[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonCodec]: JsonCodec[Map[K, V]] =
    JsonCodec(JsonEncoder.map[K, V], JsonDecoder.map[K, V])

  implicit def sortedMap[
    K: JsonFieldEncoder: JsonFieldDecoder: Ordering,
    V: JsonCodec
  ]: JsonCodec[collection.SortedMap[K, V]] =
    JsonCodec(JsonEncoder.sortedMap[K, V], JsonDecoder.sortedMap[K, V])

  implicit def sortedSet[A: Ordering: JsonCodec]: JsonCodec[immutable.SortedSet[A]] =
    JsonCodec(JsonEncoder.sortedSet[A], JsonDecoder.sortedSet[A])
}

private[json] trait CodecLowPriority2 extends CodecLowPriority3 { this: JsonCodec.type =>

  implicit def iterable[A: JsonCodec]: JsonCodec[Iterable[A]] =
    JsonCodec(JsonEncoder.iterable, JsonDecoder.iterable)
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
}
