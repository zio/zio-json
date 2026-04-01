package zio.json

import scala.deriving.Mirror
import scala.compiletime.*

object DeriveJsonEncoder {
  inline def apply[A](using m: Mirror.Of[A]): JsonEncoder[A] = {
    val elemEncoders = summonAll[m.MirroredElemTypes]
    val elemLabels   = summonLabels[m.MirroredElemLabels]

    inline m match {
      case s: Mirror.SumOf[A]     => deriveSum(s, elemEncoders, elemLabels)
      case p: Mirror.ProductOf[A] => deriveProduct(p, elemEncoders, elemLabels)
    }
  }

  private inline def summonAll[T <: Tuple]: List[JsonEncoder[?]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[JsonEncoder[t]] :: summonAll[ts]
    }

  private inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: summonLabels[ts]
    }

  private def deriveProduct[A](
    p: Mirror.ProductOf[A],
    encoders: List[JsonEncoder[?]],
    labels: List[String]
  ): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: internal.Write): Unit = {
      out.write('{')
      val pStruct = a.asInstanceOf[Product]
      var i = 0
      while (i < labels.length) {
        if (i > 0) out.write(',')
        JsonEncoder.string.unsafeEncode(labels(i), indent, out)
        out.write(':')
        encoders(i).asInstanceOf[JsonEncoder[Any]].unsafeEncode(pStruct.productElement(i), indent, out)
        i += 1
      }
      out.write('}')
    }
  }

  private def deriveSum[A](
    s: Mirror.SumOf[A],
    encoders: List[JsonEncoder[?]],
    labels: List[String]
  ): JsonEncoder[A] = new JsonEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: internal.Write): Unit = {
      val index = s.ordinal(a)
      out.write('{')
      JsonEncoder.string.unsafeEncode(labels(index), indent, out)
      out.write(':')
      encoders(index).asInstanceOf[JsonEncoder[Any]].unsafeEncode(a, indent, out)
      out.write('}')
    }
  }
}
