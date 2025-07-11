package zio.json

import zio.json.JsonDecoder.JsonError
import zio.json.internal.RetractReader

import scala.collection.immutable
import scala.compiletime.*
import scala.compiletime.ops.any.IsConst

private[json] trait JsonDecoderVersionSpecific {
  implicit def arraySeq[A: JsonDecoder: reflect.ClassTag]: JsonDecoder[immutable.ArraySeq[A]] =
    new CollectionJsonDecoder[immutable.ArraySeq[A]] {
      private[this] val arrayDecoder = JsonDecoder.array[A]

      override def unsafeDecodeMissing(trace: List[JsonError]): immutable.ArraySeq[A] = immutable.ArraySeq.empty

      def unsafeDecode(trace: List[JsonError], in: RetractReader): immutable.ArraySeq[A] =
        immutable.ArraySeq.unsafeWrapArray(arrayDecoder.unsafeDecode(trace, in))
    }

  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonDecoder[A] =
    DeriveJsonDecoder.gen[A]

  implicit def iArray[A](implicit A: JsonDecoder[A], classTag: reflect.ClassTag[A]): JsonDecoder[IArray[A]] =
    JsonDecoder.array[A].map(IArray.unsafeFromArray)

}

trait DecoderLowPriorityVersionSpecific {
  inline given unionOfStringEnumeration[T](using IsUnionOf[String, T]): JsonDecoder[T] =
    val values = UnionDerivation.constValueUnionTuple[String, T]
    JsonDecoder.string.mapOrFail {
      case raw if values.toList.contains(raw) => Right(raw.asInstanceOf[T])
      case _                                  => Left("expected one of: " + values.toList.mkString(", "))
    }
}
