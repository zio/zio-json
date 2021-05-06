package zio.json.ast

sealed trait JsonCursor[-From, +To <: Json] { self =>
  final def >>>[Next <: Json](that: JsonCursor[To, Next]): JsonCursor[From, Next] =
    (that.asInstanceOf[JsonCursor[_ <: Json, _ <: Json]] match {
      case JsonCursor.Identity =>
        that

      case JsonCursor.DownField(oldParent @ _, name) =>
        JsonCursor.DownField(self.asInstanceOf[JsonCursor[Json, Json.Obj]], name)

      case JsonCursor.DownElement(oldParent @ _, index) =>
        JsonCursor.DownElement(self.asInstanceOf[JsonCursor[Json, Json.Arr]], index)

      case JsonCursor.FilterType(oldParent @ _, tpe) =>
        JsonCursor.FilterType(self.asInstanceOf[JsonCursor[Json, Json]], tpe)
    }).asInstanceOf[JsonCursor[From, Next]]

  final def isArray: JsonCursor[Json, Json.Arr] = filterType(JsonType.Arr)

  final def isBool: JsonCursor[Json, Json.Bool] = filterType(JsonType.Bool)

  final def filterType[B <: Json](jsonType: JsonType[B]): JsonCursor[Json, B] =
    JsonCursor.FilterType(self, jsonType)

  final def isNull: JsonCursor[Json, Json.Null] = filterType(JsonType.Null)

  final def isNumber: JsonCursor[Json, Json.Num] = filterType(JsonType.Num)

  final def isObject: JsonCursor[Json, Json.Obj] = filterType(JsonType.Obj)

  final def isString: JsonCursor[Json, Json.Str] = filterType(JsonType.Str)

  final def field(field: String)(implicit ev: To <:< Json.Obj): JsonCursor.DownField =
    JsonCursor.DownField(self.widenTo[Json.Obj], field)

  final def element(index: Int)(implicit ev: To <:< Json.Arr): JsonCursor.DownElement =
    JsonCursor.DownElement(self.widenTo[Json.Arr], index)

  final private def widenTo[To1 <: Json](implicit ev: To <:< To1): JsonCursor[From, To1] =
    self.asInstanceOf[JsonCursor[From, To1]]
}

object JsonCursor {
  def element(index: Int): JsonCursor[Json.Arr, Json] = DownElement(Identity.isArray, index)

  def field(name: String): JsonCursor[Json.Obj, Json] = DownField(Identity.isObject, name)

  def filter[A <: Json](jsonType: JsonType[A]): JsonCursor[Json, A] =
    identity.filterType(jsonType)

  val identity: JsonCursor[Json, Json] = Identity

  val isArray: JsonCursor[Json, Json.Arr] = filter(JsonType.Arr)

  val isBool: JsonCursor[Json, Json.Bool] = filter(JsonType.Bool)

  val isNull: JsonCursor[Json, Json.Null] = filter(JsonType.Null)

  val isNumber: JsonCursor[Json, Json.Num] = filter(JsonType.Num)

  val isObject: JsonCursor[Json, Json.Obj] = filter(JsonType.Obj)

  val isString: JsonCursor[Json, Json.Str] = filter(JsonType.Str)

  case object Identity extends JsonCursor[Json, Json]

  final case class DownField(parent: JsonCursor[_, Json.Obj], name: String) extends JsonCursor[Json.Obj, Json]

  final case class DownElement(parent: JsonCursor[_, Json.Arr], index: Int) extends JsonCursor[Json.Arr, Json]

  final case class FilterType[A <: Json](parent: JsonCursor[_, _ <: Json], jsonType: JsonType[A])
      extends JsonCursor[Json, A]
}
