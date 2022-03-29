package zio.json

trait JsonEncoderVersionSpecific {
  inline def derived[A: deriving.Mirror.Of]: JsonEncoder[A] = DeriveJsonEncoder.gen[A]
}
