package zio.json

trait JsonEncoderVersionSpecific {
  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonEncoder[A] = DeriveJsonEncoder.gen[A]
}
