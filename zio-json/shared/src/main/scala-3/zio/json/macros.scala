package zio.json

import scala.deriving.Mirror
import scala.compiletime._
import zio.json.internal._

trait MirrorJsonDecoder {
  inline implicit def derived[A](implicit m: Mirror.Of[A]): JsonDecoder[A] = {
    val labels = constValueTuple[m.MirroredElemLabels].toList.map(_.toString).toArray
    val size = constValue[Tuple.Size[m.MirroredElemTypes]]

    new JsonDecoder[A] {
      def decodeJson(trace: List[JsonError], in: RetractReader): Either[JsonError, A] = {
        inline m match {
          case p: Mirror.ProductOf[A] =>
            val buffer = new Array[Any](size)
            // FINAL FIX: Inline recursion to keep index as a Literal Type
            val err = decodeProductInline[m.MirroredElemTypes](trace, in, buffer, labels, 0)
            if (err == null) Right(p.fromProduct(Tuple.fromArray(buffer).asInstanceOf[p.MirroredElemTypes]))
            else Left(err)

          case s: Mirror.SumOf[A] =>
            val tag = in.readString()
            decodeSumFast[A, m.MirroredElemTypes, m.MirroredElemLabels](trace, in, tag)
        }
      }
    }
  }

  // Purely inline recursion: No runtime loop, completely unrolled by the compiler
  private inline def decodeProductInline[T <: Tuple](
    trace: List[JsonError], in: RetractReader, buffer: Array[Any], labels: Array[String], inline index: Int
  ): JsonError =
    inline erasedValue[T] match {
      case _: EmptyTuple => null
      case _: (head *: tail) =>
        // Now 'index' is a compile-time constant!
        val decoder = summonInline[JsonDecoder[head]]
        decoder.decodeJson(JsonError.ObjectContext(labels(index)) :: trace, in) match {
          case Right(v) =>
            buffer(index) = v
            decodeProductInline[tail](trace, in, buffer, labels, index + 1)
          case Left(e) => e
        }
    }

  private inline def decodeSumFast[A, ET <: Tuple, EL <: Tuple](
    trace: List[JsonError], in: RetractReader, tag: String
  ): Either[JsonError, A] =
    inline (erasedValue[ET], erasedValue[EL]) match {
      case _: (et *: ets, el *: els) =>
        if (tag == constValue[el].asInstanceOf[String])
          summonInline[JsonDecoder[et]].asInstanceOf[JsonDecoder[A]].decodeJson(trace, in)
        else decodeSumFast[A, ets, els](trace, in, tag)
      case _ => Left(JsonError.Message(s"Unknown tag: $tag"))
    }
}
