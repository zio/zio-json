package zio.json

import zio.Chunk
import zio.json.internal._

import JsonDecoder.JsonError

trait JsonCodec[A] extends JsonDecoder[A] with JsonEncoder[A]
object JsonCodec {
  def apply[A](implicit jsonCodec: JsonCodec[A]): JsonCodec[A] = jsonCodec

  implicit def apply[A](implicit encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] =
    new JsonCodec[A] {
      private[zio] def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): A =
        decoder.unsafeDecode(trace, in)

      private[zio] def unsafeEncode(a: A, indent: Option[Int], out: java.io.Writer): Unit =
        encoder.unsafeEncode(a, indent, out)
    }
}
