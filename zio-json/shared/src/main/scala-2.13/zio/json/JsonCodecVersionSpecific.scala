package zio.json

import scala.collection.immutable

trait JsonCodecVersionSpecific {
  implicit def arraySeq[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[immutable.ArraySeq[A]] =
    JsonCodec(JsonEncoder.arraySeq[A], JsonDecoder.arraySeq[A])
}
