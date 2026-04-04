package zio.json

import scala.deriving.Mirror
import scala.compiletime._
import zio.json.ast.Json
import zio.json.internal._
import zio.Chunk

trait MirrorJsonDecoder {
  inline val discriminator: String = "type"
  implicit val config: JsonDecoder.Config = JsonDecoder.Config(ignoreUnknownFields = true)

  inline implicit def derived[A](implicit m: Mirror.Of[A]): JsonDecoder[A] = new JsonDecoder[A] {
    def decodeJson(trace: List[JsonError], in: RetractReader): Either[JsonError, A] = 
      JsonDecoder[Json].decodeJson(trace, in).flatMap(fromJsonAST)

    override def fromJsonAST(json: Json): Either[JsonError, A] = json match {
      case Json.Str(tag) => 
        inline m match {
          case s: Mirror.SumOf[A] => decodeSumFromAST[A, m.MirroredElemTypes, m.MirroredElemLabels](Nil, Json.Obj(Chunk(discriminator -> Json.Str(tag))), tag)
          case _ => Left(JsonError.Message("Expected object for product type") :: Nil)
        }
      case obj: Json.Obj =>
        inline m match {
          case p: Mirror.ProductOf[A] =>
            val size = constValue[Tuple.Size[m.MirroredElemTypes]]
            val buffer = new Array[Any](size)
            decodeFieldsRecursive[m.MirroredElemTypes, m.MirroredElemLabels](Nil, obj.fields, buffer, 0) match {
              case None => Right(p.fromProduct(Tuple.fromArray(buffer).asInstanceOf[p.MirroredElemTypes]))
              case Some(err) => Left(err)
            }
          case s: Mirror.SumOf[A] =>
            obj.fields.find(_._1 == discriminator) match {
              case Some((_, Json.Str(tag))) => decodeSumFromAST[A, m.MirroredElemTypes, m.MirroredElemLabels](Nil, obj, tag)
              case _ => Left(JsonError.Message(s"Missing discriminator '$discriminator'") :: Nil)
            }
        }
      case _ => Left(JsonError.Message("Invalid JSON format") :: Nil)
    }
  }

  private inline def decodeSumFromAST[A, ET <: Tuple, EL <: Tuple](trace: List[JsonError], obj: Json.Obj, tag: String): Either[JsonError, A] =
    inline (erasedValue[ET], erasedValue[EL]) match {
      case _: (et *: ets, el *: els) =>
        if (tag == constValue[el].asInstanceOf[String]) summonInline[JsonDecoder[et]].asInstanceOf[JsonDecoder[A]].fromJsonAST(obj)
        else decodeSumFromAST[A, ets, els](trace, obj, tag)
      case _ => Left(JsonError.Message(s"Unknown tag: $tag") :: trace)
    }

  private inline def decodeFieldsRecursive[T <: Tuple, L <: Tuple](trace: List[JsonError], fields: Chunk[(String, Json)], buffer: Array[Any], index: Int): Option[JsonError] =
    inline (erasedValue[T], erasedValue[L]) match {
      case _: (EmptyTuple, EmptyTuple) => None
      case _: (tHead *: tTail, lHead *: lTail) =>
        val label = constValue[lHead].toString
        fields.find(_._1 == label) match {
          case Some((_, json)) => summonInline[JsonDecoder[tHead]].fromJsonAST(json) match {
            case Right(v) => buffer(index) = v; decodeFieldsRecursive[tTail, lTail](trace, fields, buffer, index + 1)
            case Left(e) => Some(JsonError.ObjectContext(label) :: e)
          }
          case None => Some(JsonError.Message(s"Missing field: $label") :: trace)
        }
      case _ => None
    }
}
