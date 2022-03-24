package zio.json

trait JsonDecoderVersionSpecific {
  inline def derived[a: deriving.Mirror.Of]: JsonDecoder[a] = DeriveJsonDecoder.gen[a]
}
