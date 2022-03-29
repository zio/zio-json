package zio.json

trait JsonCodecVersionSpecific {
  inline def derived[A: deriving.Mirror.Of]: JsonCodec[A] = DeriveJsonCodec.gen[A]
}
