package zio.json

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

object JsonCodec {
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

//          override def unsafeDecodeMissing(trace: List[JsonError]): A =
//            decoder0.unsafeDecodeMissing(trace)

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
            decoder0.unsafeDecode(trace, in)

          override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
            encoder0.unsafeEncode(a, indent, out)
        }
    }
}
