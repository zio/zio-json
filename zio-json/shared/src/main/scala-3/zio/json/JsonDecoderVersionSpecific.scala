package zio.json

import scala.compiletime.*
import scala.compiletime.ops.any.IsConst

private[json] trait JsonDecoderVersionSpecific {
  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonDecoder[A] =
    DeriveJsonDecoder.gen[A]
}

trait DecoderLowPriorityVersionSpecific {

  inline given unionOfStringEnumeration[T](using IsUnionOf[String, T]): JsonDecoder[T] =
    val values = UnionDerivation.constValueUnionTuple[String, T]
    JsonDecoder.string.mapOrFail {
      case raw if values.toList.contains(raw) => Right(raw.asInstanceOf[T])
      case _                                  => Left("expected one of: " + values.toList.mkString(", "))
    }
}
