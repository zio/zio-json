package zio.json

import zio.json.ast.Json
import zio.json.internal.Write

import scala.collection.immutable

private[json] trait JsonEncoderVersionSpecific {
  implicit def arraySeq[A: JsonEncoder: scala.reflect.ClassTag]: JsonEncoder[immutable.ArraySeq[A]] =
    new JsonEncoder.AbstractJsonEncoder[immutable.ArraySeq[A]] {
      private[this] val arrayEnc = JsonEncoder.array[A]

      override def isEmpty(as: immutable.ArraySeq[A]): Boolean = as.isEmpty

      def unsafeEncode(as: immutable.ArraySeq[A], indent: Option[Int], out: Write): Unit =
        arrayEnc.unsafeEncode(as.unsafeArray.asInstanceOf[Array[A]], indent, out)

      override final def toJsonAST(as: immutable.ArraySeq[A]): Either[String, Json] =
        arrayEnc.toJsonAST(as.unsafeArray.asInstanceOf[Array[A]])
    }
}

private[json] trait EncoderLowPriorityVersionSpecific
