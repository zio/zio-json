package zio.json

private[json] trait JsonCodecVersionSpecific {

  implicit def fromEncoderDecoder[A](implicit encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] =
    JsonCodec(encoder, decoder)
}
