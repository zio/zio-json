package zio.json

import scala.collection.immutable

private[json] trait JsonCodecVersionSpecific {
  implicit def arraySeq[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[immutable.ArraySeq[A]] =
    JsonCodec(JsonEncoder.arraySeq[A], JsonDecoder.arraySeq[A])

  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonCodec[A] = DeriveJsonCodec.gen[A]

  implicit def iArray[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[IArray[A]] =
    JsonCodec(JsonEncoder.iArray[A], JsonDecoder.iArray[A])

}
