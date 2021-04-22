package zio.json.ast

import scala.annotation.implicitNotFound

// TODO: Add another type parameter (From) to JsonCursor
// TODO: Rename to JsonPath[From, To]???
sealed trait JsonCursor[A] { self =>
  // TODO: Rename to >>>
  def ++[B <: Json](that: JsonCursor[B])(implicit drillDown: JsonCursor.DrillDown[A, B]): JsonCursor[B] = ???

  def isArray: JsonCursor[Json.Arr] = filterType(JsonType.Arr)

  def isBool: JsonCursor[Json.Bool] = filterType(JsonType.Bool)

  def filterType[B <: Json](jsonType: JsonType[B]): JsonCursor[B] = JsonCursor.FilterType(self, jsonType)

  def isNull: JsonCursor[Json.Null] = filterType(JsonType.Null)

  def isNumber: JsonCursor[Json.Num] = filterType(JsonType.Num)

  def isObject: JsonCursor[Json.Obj] = filterType(JsonType.Obj)

  def isString: JsonCursor[Json.Str] = filterType(JsonType.Str)
}

object JsonCursor {
  @implicitNotFound("Cannot drill down from ${A} into ${B}")
  sealed trait DrillDown[A, B]
  object DrillDown {
    implicit def LeftIsJson[B]: DrillDown[Json, B]    = new DrillDown[Json, B] {}
    implicit def LeftIsArr[B]: DrillDown[Json.Arr, B] = new DrillDown[Json.Arr, B] {}
    implicit def LeftIsObj[B]: DrillDown[Json.Obj, B] = new DrillDown[Json.Obj, B] {}
  }

  def element(index: Int): JsonCursor[Json] = DownElement(Identity.isArray, index)

  def field(name: String): JsonCursor[Json] = DownField(Identity.isObject, name)

  def filter[A <: Json](jsonType: JsonType[A]): JsonCursor[A] = identity.filterType(jsonType)

  val identity: JsonCursor[Json] = Identity

  val isArray: JsonCursor[Json.Arr] = filter(JsonType.Arr)

  val isBool: JsonCursor[Json.Bool] = filter(JsonType.Bool)

  val isNull: JsonCursor[Json.Null] = filter(JsonType.Null)

  val isNumber: JsonCursor[Json.Num] = filter(JsonType.Num)

  val isObject: JsonCursor[Json.Obj] = filter(JsonType.Obj)

  val isString: JsonCursor[Json.Str] = filter(JsonType.Str)

  case object Identity                                                                         extends JsonCursor[Json]
  final case class DownField(parent: JsonCursor[Json.Obj], name: String)                       extends JsonCursor[Json]
  final case class DownElement(parent: JsonCursor[Json.Arr], index: Int)                       extends JsonCursor[Json]
  final case class FilterType[A](parent: JsonCursor[_], jsonType: JsonType[A]) extends JsonCursor[A]
}
