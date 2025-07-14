package zio.json

import zio.json.ast.Json
import zio.json.internal.Write

import scala.collection.immutable
import scala.compiletime.ops.any.IsConst

private[json] trait JsonEncoderVersionSpecific {
  implicit def arraySeq[A: JsonEncoder: scala.reflect.ClassTag]: JsonEncoder[immutable.ArraySeq[A]] =
    new JsonEncoder[immutable.ArraySeq[A]] {
      private[this] val arrayEnc = JsonEncoder.array[A]

      override def isEmpty(as: immutable.ArraySeq[A]): Boolean = as.isEmpty

      def unsafeEncode(as: immutable.ArraySeq[A], indent: Option[Int], out: Write): Unit =
        arrayEnc.unsafeEncode(as.unsafeArray.asInstanceOf[Array[A]], indent, out)

      override final def toJsonAST(as: immutable.ArraySeq[A]): Either[String, Json] =
        arrayEnc.toJsonAST(as.unsafeArray.asInstanceOf[Array[A]])
    }

  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonEncoder[A] =
    DeriveJsonEncoder.gen[A]

  implicit def iArray[A](implicit A: JsonEncoder[A], classTag: scala.reflect.ClassTag[A]): JsonEncoder[IArray[A]] =
    JsonEncoder.array[A].contramap[IArray[A]](arr => IArray.genericWrapArray(arr).toArray)

}

private[json] trait EncoderLowPriorityVersionSpecific {
  inline given unionOfStringEnumeration[T](using IsUnionOf[String, T]): JsonEncoder[T] =
    JsonEncoder.string.asInstanceOf[JsonEncoder[T]]
}
