package zio.json

private[json] trait JsonCodecVersionSpecific {
  inline def derived[A: deriving.Mirror.Of]: JsonCodec[A] = DeriveJsonCodec.gen[A]

  given fromEncoderDecoder[A](using encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] =
    JsonCodec(encoder, decoder)
}
