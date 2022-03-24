package zio.json

trait JsonCodecVersionSpecific {
  inline def derived[a: deriving.Mirror.Of]: JsonCodec[a] = DeriveJsonCodec.gen[a]
}
