package zio.json

import scala.deriving.Mirror
import scala.compiletime._
import zio.json.internal._

trait MirrorJsonDecoder {
  inline implicit def derived[A](implicit m: Mirror.Of[A]): JsonDecoder[A] = {
    new JsonDecoder[A] {
      def decodeJson(trace: List[JsonError], in: RetractReader): Either[JsonError, A] = {
        inline m match {
          case p: Mirror.ProductOf[A] =>
            val size = constValue[Tuple.Size[m.MirroredElemTypes]]
            val buffer = new Array[Any](size)
            // FINAL FIX: Recursively peeling both Types and Labels tuples
            decodeProductRecursive[m.MirroredElemTypes, m.MirroredElemLabels](trace, in, buffer, 0) match {
              case null => Right(p.fromProduct(Tuple.fromArray(buffer).asInstanceOf[p.MirroredElemTypes]))
              case err  => Left(err)
            }

          case s: Mirror.SumOf[A] =>
            // Sum types usually need a discriminator (type field) in ZIO JSON
            val tag = in.readString() 
            decodeSumFast[A, m.MirroredElemTypes, m.MirroredElemLabels](trace, in, tag)
        }
      }
    }
  }

  // Peeling logic: T for Types, L for Labels
  private inline def decodeProductRecursive[T <: Tuple, L <: Tuple](
    trace: List[JsonError], in: RetractReader, buffer: Array[Any], index: Int
  ): JsonError =
    inline (erasedValue[T], erasedValue[L]) match {
      case _: (EmptyTuple, EmptyTuple) => null
      case _: (tHead *: tTail, lHead *: lTail) =>
        val label = constValue[lHead].toString
        val decoder = summonInline[JsonDecoder[tHead]]
        decoder.decodeJson(JsonError.ObjectContext(label) :: trace, in) match {
          case Right(v) =>
            buffer(index) = v
            decodeProductRecursive[tTail, lTail](trace, in, buffer, index + 1)
          case Left(e) => e
        }
      case _ => JsonError.Message("Internal error: Tuple mismatch")
    }
}
