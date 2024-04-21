package zio.json

trait JsonDecoderVersionSpecific {
  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonDecoder[A] = DeriveJsonDecoder.gen[A]
}
