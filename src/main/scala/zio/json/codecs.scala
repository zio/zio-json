package zio.json

import zio.Chunk
import zio.json.internal._

import JsonDecoder.JsonError

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
trait JsonCodec[A] extends JsonDecoder[A] with JsonEncoder[A]
object JsonCodec {
  def apply[A](implicit jsonCodec: JsonCodec[A]): JsonCodec[A] = jsonCodec

  /**
   * Derives a `JsonCodec[A]` from an encoder and a decoder.
   */
  implicit def apply[A](implicit encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] =
    new JsonCodec[A] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A =
        decoder.unsafeDecode(trace, in)

      def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit =
        encoder.unsafeEncode(a, indent, out)
    }
}
