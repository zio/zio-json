package zio.json

private[json] trait JsonCodecVersionSpecific {
  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonCodec[A] = DeriveJsonCodec.gen[A]

}
