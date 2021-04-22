package zio.json.ast

import zio.Chunk

// a.diff(b).apply(a) === Right(b)
// Unchanged.apply(a) === Right(a)
sealed trait JsonDiff { self =>
  def apply(json: Json): Either[String, Json]

  // Unchanged + diff === diff
  // diff + Unchanged === diff
  final def +(that: JsonDiff): JsonDiff = JsonDiff.Sequence(self, that)
}
object JsonDiff {
  import Json._

  case object Unchanged extends JsonDiff {
    def apply(json: Json): Either[String, Json] = Right(json)
  }
  final case class Replace(value: Json) extends JsonDiff {
    def apply(json: Json): Either[String, Json] = Right(value)
  }
  final case class ObjDiff(operations: Chunk[Either[(String, JsonDiff), (String, Json)]]) extends JsonDiff {
    def apply(json: Json): Either[String, Json] =
      ???
  }
  final case class ArrDiff(elements: Chunk[Either[(Int, JsonDiff), Json]]) extends JsonDiff {
    def apply(json: Json): Either[String, Json] =
      ???
  }
  final case class Sequence(first: JsonDiff, second: JsonDiff) extends JsonDiff {
    def apply(json: Json): Either[String, Json] = first(json).flatMap(second(_))
  }

  def apply(from: Json, to: Json): JsonDiff =
    (from, to) match {
      case (j1, j2) if j1 == j2 => Unchanged
      case (Obj(l), Obj(r))     => ???
      case (Arr(l), Arr(r))     => ???
      case (_, to)              => Replace(to)
    }
}
