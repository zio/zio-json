package zio.json

trait JsonEncoderVersionSpecific {
  inline def derived[a: deriving.Mirror.Of]: JsonEncoder[a] = DeriveJsonEncoder.gen[a]
}
