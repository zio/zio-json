package zio.json

import scala.deriving.Mirror
import scala.compiletime._
import zio.json.internal._

trait MirrorJsonDecoder {

  inline implicit def derived[A](implicit m: Mirror.Of[A]): JsonDecoder[A] = {
    // metadata retrieval is zero-cost at compile time
    val fieldLabels = constValueTuple[m.MirroredElemLabels].toList.map(_.toString).toArray
    val fieldCount = constValue[Tuple.Size[m.MirroredElemTypes]]

    new JsonDecoder[A] {
      def decodeJson(trace: List[JsonError], in: RetractReader): Either[JsonError, A] = {
        inline m match {
          case p: Mirror.ProductOf[A] =>
            val buffer = new Array[AnyRef](fieldCount)
            
            // Hot-path: Recursive inlining for raw speed
            decodeUnrolled[m.MirroredElemTypes](trace, in, buffer, fieldLabels, 0) match {
              case Right(_) => 
                try {
                  // Direct product creation to bypass Tuple allocation overhead
                  Right(p.fromProduct(new Product {
                    def canEqual(that: Any): Boolean = true
                    def productArity: Int = fieldCount
                    def productElement(n: Int): Any = buffer(n)
                  }.asInstanceOf[p.MirroredElemTypes]))
                } catch {
                  case e: Exception => Left(JsonError.Message(e.getMessage) :: trace)
                }
              case Left(err) => Left(err)
            }

          case _: Mirror.SumOf[A] =>
            val tag = in.readString()
            // Using the outer implicit 'm' for types and labels
            decodeSumUnrolled[A, m.MirroredElemTypes, m.MirroredElemLabels](trace, in, tag)
        }
      }
    }
  }

  private inline def decodeUnrolled[T <: Tuple](
    trace: List[JsonError], 
    in: RetractReader, 
    buffer: Array[AnyRef], 
    labels: Array[String], 
    index: Int
  ): Either[JsonError, Unit] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Right(())
      case _: (head *: tail) => 
        val fieldName = labels(index)
        val context = JsonError.ObjectContext(fieldName) :: trace
        
        summonInline[JsonDecoder[head]].decodeJson(context, in) match {
          case Right(value) =>
            buffer(index) = value.asInstanceOf[AnyRef]
            decodeUnrolled[tail](trace, in, buffer, labels, index + 1)
          case Left(err) => Left(err)
        }
    }
  }

  private inline def decodeSumUnrolled[A, ET <: Tuple, EL <: Tuple](
    trace: List[JsonError], 
    in: RetractReader, 
    tag: String
  ): Either[JsonError, A] = {
    inline (erasedValue[ET], erasedValue[EL]) match {
      case (_: (et *: ets), _: (el *: els)) =>
        if (tag == constValue[el].asInstanceOf[String])
          summonInline[JsonDecoder[et]].asInstanceOf[JsonDecoder[A]].decodeJson(trace, in)
        else 
          decodeSumUnrolled[A, ets, els](trace, in, tag)
      case _ => 
        Left(JsonError.Message(s"Unknown discriminator tag: $tag") :: trace)
    }
  }
}
