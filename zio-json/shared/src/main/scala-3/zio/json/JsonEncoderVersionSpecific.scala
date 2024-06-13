package zio.json

import scala.compiletime.ops.any.IsConst

private[json] trait JsonEncoderVersionSpecific {
  inline def derived[A: deriving.Mirror.Of]: JsonEncoder[A] = DeriveJsonEncoder.gen[A]
}

private[json] trait EncoderLowPriorityVersionSpecific {

  inline given unionOfStringEnumeration[T](using IsUnionOf[String, T]): JsonEncoder[T] =
    JsonEncoder.string.asInstanceOf[JsonEncoder[T]]

  inline given constStringToEnum[T <: String](using IsConst[T] =:= true): JsonEncoder[T] =
    JsonEncoder.string.narrow[T]
}
