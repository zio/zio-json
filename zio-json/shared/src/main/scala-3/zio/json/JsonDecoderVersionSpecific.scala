package zio.json

private[json] trait JsonDecoderVersionSpecific {
  inline def derived[A: deriving.Mirror.Of]: JsonDecoder[A] = DeriveJsonDecoder.gen[A]
}
