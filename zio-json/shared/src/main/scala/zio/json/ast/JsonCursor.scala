/*
 * Copyright 2019-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.json.ast

/**
 * A path from one JSON node to another, used to read and edit a document with `Json#get`, `Json#delete`,
 * `Json#transformAt` and `Json#relocate`.
 *
 * `From` is the kind of node the path starts at and `To` the kind of node it ends at. Build a cursor from the
 * constructors in the companion (`JsonCursor.field`, `JsonCursor.element`, `JsonCursor.identity`), then extend it with
 * `field`, `element`, the `is...` checks and `>>>`. A cursor is only a description: it does not hold or change any
 * JSON.
 *
 * {{{
 * import zio.json._
 *
 * val json = """{"users":[{"name":"Ada"},{"name":"Alan"}]}""".fromJson[Json].toOption.get
 *
 * val cursor = JsonCursor.field("users").isArray.element(0).isObject.field("name")
 *
 * json.get(cursor)
 * // Right("Ada")
 *
 * json.transformAt(cursor)(_ => Json.Str("Grace"))
 * // Right({"users":[{"name":"Grace"},{"name":"Alan"}]})
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/ ZIO JSON]]
 */
sealed trait JsonCursor[-From, +To <: Json] { self =>

  /**
   * Appends the cursor `that` to this one: the result starts where this cursor starts and follows `that` from the node
   * this cursor ends at.
   *
   * {{{
   * val users = JsonCursor.field("users").isArray
   * val first = JsonCursor.element(0)
   *
   * users >>> first
   * }}}
   */
  final def >>>[Next <: Json](that: JsonCursor[To, Next]): JsonCursor[From, Next] =
    (that.asInstanceOf[JsonCursor[_ <: Json, _ <: Json]] match {
      case JsonCursor.Identity =>
        self

      case JsonCursor.DownField(oldParent: JsonCursor[To, Json.Obj], name) =>
        JsonCursor.DownField(self >>> oldParent, name)

      case JsonCursor.DownElement(oldParent: JsonCursor[To, Json.Arr], index) =>
        JsonCursor.DownElement(self >>> oldParent, index)

      case JsonCursor.FilterType(oldParent: JsonCursor[To, _], tpe) =>
        JsonCursor.FilterType(self >>> oldParent, tpe)
    }).asInstanceOf[JsonCursor[From, Next]]

  /**
   * Alias for `>>>`.
   */
  final def andThen[Next <: Json](that: JsonCursor[To, Next]): JsonCursor[From, Next] = self >>> that

  /**
   * Extends this cursor with a check that the node is a `Json.Arr`. Reading fails with `expected array` otherwise.
   */
  final def isArray: JsonCursor[Json, Json.Arr] = filterType(JsonType.Arr)

  /**
   * Extends this cursor with a check that the node is a `Json.Bool`. Reading fails with `expected boolean` otherwise.
   */
  final def isBool: JsonCursor[Json, Json.Bool] = filterType(JsonType.Bool)

  /**
   * Extends this cursor with a check that the node has the given `jsonType`.
   *
   * Reading fails with `expected null`, `expected boolean`, `expected object`, `expected array`, `expected string` or
   * `expected number`, according to `jsonType`, when the node has a different type.
   */
  final def filterType[B <: Json](jsonType: JsonType[B]): JsonCursor[Json, B] =
    JsonCursor.FilterType(self, jsonType)

  /**
   * Extends this cursor with a check that the node is `Json.Null`. Reading fails with `expected null` otherwise.
   */
  final def isNull: JsonCursor[Json, Json.Null] = filterType(JsonType.Null)

  /**
   * Extends this cursor with a check that the node is a `Json.Num`. Reading fails with `expected number` otherwise.
   */
  final def isNumber: JsonCursor[Json, Json.Num] = filterType(JsonType.Num)

  /**
   * Extends this cursor with a check that the node is a `Json.Obj`. Reading fails with `expected object` otherwise.
   */
  final def isObject: JsonCursor[Json, Json.Obj] = filterType(JsonType.Obj)

  /**
   * Extends this cursor with a check that the node is a `Json.Str`. Reading fails with `expected string` otherwise.
   */
  final def isString: JsonCursor[Json, Json.Str] = filterType(JsonType.Str)

  /**
   * Extends this cursor to the value of the object field called `field`.
   *
   * It only applies to a cursor that ends at a `Json.Obj`. Reading fails with `No such field: '<name>'` when the object
   * has no such field.
   */
  final def field(field: String)(implicit ev: To <:< Json.Obj): JsonCursor.DownField =
    JsonCursor.DownField(self.widenTo[Json.Obj], field)

  /**
   * Extends this cursor to the array element at `index`, counting from 0.
   *
   * It only applies to a cursor that ends at a `Json.Arr`. Reading fails with `The array does not have index <index>`
   * when the array is too short.
   */
  final def element(index: Int)(implicit ev: To <:< Json.Arr): JsonCursor.DownElement =
    JsonCursor.DownElement(self.widenTo[Json.Arr], index)

  final private def widenTo[To1 <: Json](implicit ev: To <:< To1): JsonCursor[From, To1] =
    self.asInstanceOf[JsonCursor[From, To1]]
}

/**
 * Constructors for [[JsonCursor]].
 *
 * {{{
 * JsonCursor.field("address").isObject.field("city")
 * }}}
 */
object JsonCursor {

  /**
   * The cursor that points to the node it starts at, which is the whole document.
   */
  val identity: JsonCursor[Json, Json] = Identity

  /**
   * A cursor that points to the whole document and checks that it is a `Json.Arr`.
   */
  val isArray: JsonCursor[Json, Json.Arr] = filter(JsonType.Arr)

  /**
   * A cursor that points to the whole document and checks that it is a `Json.Bool`.
   */
  val isBool: JsonCursor[Json, Json.Bool] = filter(JsonType.Bool)

  /**
   * A cursor that points to the whole document and checks that it is `Json.Null`.
   */
  val isNull: JsonCursor[Json, Json.Null] = filter(JsonType.Null)

  /**
   * A cursor that points to the whole document and checks that it is a `Json.Num`.
   */
  val isNumber: JsonCursor[Json, Json.Num] = filter(JsonType.Num)

  /**
   * A cursor that points to the whole document and checks that it is a `Json.Obj`.
   */
  val isObject: JsonCursor[Json, Json.Obj] = filter(JsonType.Obj)

  /**
   * A cursor that points to the whole document and checks that it is a `Json.Str`.
   */
  val isString: JsonCursor[Json, Json.Str] = filter(JsonType.Str)

  /**
   * A cursor that points to the whole document and checks that it has the given `jsonType`.
   */
  def filter[A <: Json](jsonType: JsonType[A]): JsonCursor[Json, A] = FilterType(identity, jsonType)

  /**
   * A cursor that points to the element at `index` of a document that is a `Json.Arr`.
   *
   * {{{
   * val third = JsonCursor.element(2)
   * }}}
   */
  def element(index: Int): JsonCursor[Json.Arr, Json] = DownElement(isArray, index)

  /**
   * A cursor that points to the value of the field `name` of a document that is a `Json.Obj`.
   *
   * {{{
   * val name = JsonCursor.field("name")
   * }}}
   */
  def field(name: String): JsonCursor[Json.Obj, Json] = DownField(isObject, name)

  /**
   * The cursor that points to the node it starts at.
   */
  case object Identity extends JsonCursor[Json, Json]

  /**
   * The cursor that follows the field `name` of the object that `parent` points to.
   */
  final case class DownField(parent: JsonCursor[_, Json.Obj], name: String) extends JsonCursor[Json.Obj, Json]

  /**
   * The cursor that follows the element at `index` of the array that `parent` points to.
   */
  final case class DownElement(parent: JsonCursor[_, Json.Arr], index: Int) extends JsonCursor[Json.Arr, Json]

  /**
   * The cursor that checks that the node `parent` points to has the type `jsonType`.
   */
  final case class FilterType[A <: Json](parent: JsonCursor[_, _ <: Json], jsonType: JsonType[A])
      extends JsonCursor[Json, A]
}
