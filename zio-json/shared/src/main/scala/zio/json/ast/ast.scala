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

import zio.Chunk
import zio.json.JsonDecoder.JsonError
import zio.json._
import zio.json.ast.Json._
import zio.json.ast.JsonType.rightNull
import zio.json.internal._

import scala.annotation._

/**
 * A JSON value: one of `Json.Obj`, `Json.Arr`, `Json.Str`, `Json.Num`, `Json.Bool` or `Json.Null`.
 *
 * This AST of JSON is made available so that arbitrary JSON may be included as part of a business object, it is not
 * used as an intermediate representation, unlike most other JSON libraries. It is not advised to `.map` or `.mapOrFail`
 * from these decoders, since a higher performance decoder is often available.
 *
 * Beware of the potential for DOS attacks, since an attacker can provide much more data than is perhaps needed.
 *
 * Also beware of converting `Num` (a `BigDecimal`) into any other kind of number, since many of the stdlib functions
 * are non-total or are known DOS vectors (e.g. calling `.toBigInteger` on a "1e214748364" will consume an excessive
 * amount of heap memory).
 *
 * Object equality ignores the order of the fields. `Json` has a `JsonEncoder`, a `JsonDecoder` and a `JsonCodec`, so it
 * can be parsed with `fromJson` and printed with `toJson`.
 *
 * {{{
 * import zio.json._
 *
 * val parsed: Either[String, Json] = """{"name":"Ada","langs":["en","fr"]}""".fromJson[Json]
 * // Right({"name":"Ada","langs":["en","fr"]})
 *
 * val built: Json = Json.Obj("name" -> Json.Str("Ada"), "age" -> Json.Num(36))
 * val text: String = built.toJson
 * // {"name":"Ada","age":36}
 * }}}
 *
 * @see [[https://zio.dev/zio-json/ ZIO JSON]]
 */
sealed abstract class Json { self =>
  /**
   * Decodes this value into an `A` with the implicit decoder, without going through text.
   *
   * Returns `Left` with the decoding error message when this value cannot be decoded as an `A`.
   *
   * {{{
   * import zio.json._
   *
   * Json.Num(42).as[Int]
   * // Right(42)
   * }}}
   */
  final def as[A](implicit decoder: JsonDecoder[A]): Either[String, A] = decoder.fromJsonAST(self)

  /**
   * Returns `Some(())` when this value is `Json.Null`, and `None` otherwise.
   */
  def asNull: Option[Unit]         = None
  /**
   * Returns the value when this is a `Json.Bool`, and `None` otherwise.
   */
  def asBoolean: Option[Boolean]   = None
  /**
   * Returns this value when it is a `Json.Num`, and `None` otherwise.
   */
  def asNumber: Option[Json.Num]   = None
  /**
   * Returns the string when this is a `Json.Str`, and `None` otherwise.
   */
  def asString: Option[String]     = None
  /**
   * Returns the elements when this is a `Json.Arr`, and `None` otherwise.
   */
  def asArray: Option[Chunk[Json]] = None
  /**
   * Returns this value when it is a `Json.Obj`, and `None` otherwise.
   */
  def asObject: Option[Json.Obj]   = None

  /**
   * Applies `f` to the value when this is a `Json.Bool`, and returns this value unchanged otherwise.
   */
  def mapBoolean(@nowarn f: Boolean => Boolean): Json                          = self
  /**
   * Applies `f` to the number when this is a `Json.Num`, and returns this value unchanged otherwise.
   */
  def mapNumber(@nowarn f: java.math.BigDecimal => java.math.BigDecimal): Json = self
  /**
   * Applies `f` to the string when this is a `Json.Str`, and returns this value unchanged otherwise.
   */
  def mapString(@nowarn f: String => String): Json                             = self
  /**
   * Applies `f` to the elements when this is a `Json.Arr`, and returns this value unchanged otherwise.
   */
  def mapArray(@nowarn f: Chunk[Json] => Chunk[Json]): Json                    = self
  /**
   * Applies `f` to each element when this is a `Json.Arr`, and returns this value unchanged otherwise.
   */
  def mapArrayValues(@nowarn f: Json => Json): Json                            = self
  /**
   * Applies `f` to this object when this is a `Json.Obj`, and returns this value unchanged otherwise.
   */
  def mapObject(@nowarn f: Json.Obj => Json.Obj): Json                         = self
  /**
   * Applies `f` to each field name when this is a `Json.Obj`, and returns this value unchanged otherwise.
   *
   * The values are not changed.
   */
  def mapObjectKeys(@nowarn f: String => String): Json                         = self
  /**
   * Applies `f` to each field value when this is a `Json.Obj`, and returns this value unchanged otherwise.
   */
  def mapObjectValues(@nowarn f: Json => Json): Json                           = self
  /**
   * Applies `f` to each name and value pair when this is a `Json.Obj`, and returns this value unchanged otherwise.
   */
  def mapObjectEntries(@nowarn f: ((String, Json)) => (String, Json)): Json    = self

  /**
   * Folds this value into an `X` according to its shape.
   *
   * Calls `jsonArray` with the elements when this is a `Json.Arr`, `jsonObject` when this is a `Json.Obj`, and evaluates `or`
   * for every other kind of value.
   */
  final def arrayOrObject[X](
    or: => X,
    jsonArray: Chunk[Json] => X,
    jsonObject: Json.Obj => X
  ): X = self match {
    case a: Json.Arr => jsonArray(a.elements)
    case o: Json.Obj => jsonObject(o)
    case _           => or
  }

  /**
   * Deletes the node that `cursor` points to.
   *
   * The result is `Left` with an error message when the node does not exist. Deleting the whole document, with
   * `JsonCursor.identity`, gives `Right(Json.Null)`.
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"name":"Ada","age":36}""".fromJson[Json].toOption.get
   *
   * json.delete(JsonCursor.field("age"))
   * // Right({"name":"Ada"})
   *
   * json.delete(JsonCursor.field("email"))
   * // Left(No such field: 'email')
   * }}}
   *
   * @param cursor
   *   Cursor which specifies node to delete
   * @return
   *   Json without specified node if node specified by cursor exists, error otherwise
   */
  final def delete(cursor: JsonCursor[_, _]): Either[String, Json] = {
    val c = cursor.asInstanceOf[JsonCursor[_, Json]]

    transformOrDelete(c, delete = true)(_ => rightNull)
  }

  override final def equals(that: Any): Boolean = {
    def objEqual(left: Map[String, Json], right: Chunk[(String, Json)]): Boolean =
      right.forall { case (key, r) =>
        left.get(key) match {
          case Some(l) if l == r => true
          case _                 => false
        }
      }

    that match {
      case that: Json =>
        (self, that) match {
          case (Obj(l), Obj(r)) =>
            // order does not matter for JSON Objects
            if (l.length == r.length) objEqual(l.toMap, r)
            else false
          case (Arr(l), Arr(r))   => l == r
          case (Bool(l), Bool(r)) => l == r
          case (Str(l), Str(r))   => l == r
          case (Num(l), Num(r))   => l == r
          case (_: Null, _: Null) => true
          case _                  => false
        }

      case _ => false
    }
  }

  /**
   * Folds over this value and all of its descendants, visiting every parent before its children.
   *
   * Object fields and array elements are visited in order. `f` receives the accumulator and the visited value.
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"a":[1,2],"b":3}""".fromJson[Json].toOption.get
   *
   * json.foldDown(0)((count, _) => count + 1)
   * // 6
   * }}}
   */
  final def foldDown[A](initial: A)(f: (A, Json) => A): A = {
    val a = f(initial, self)

    self match {
      case Obj(fields)   => fields.map(_._2).foldLeft(a)((a, json) => json.foldDown(a)(f))
      case Arr(elements) => elements.foldLeft(a)((a, json) => json.foldDown(a)(f))
      case _             => a
    }
  }

  /**
   * Like `foldDown`, but only values for which `pf` is defined update the accumulator.
   *
   * For every other value the accumulator is kept as it is.
   */
  final def foldDownSome[A](initial: A)(pf: PartialFunction[(A, Json), A]): A = {
    val lifted = pf.lift
    val total  = (a: A, json: Json) => lifted((a, json)).getOrElse(a)

    foldDown(initial)(total)
  }

  /**
   * Folds over this value and all of its descendants, visiting every child before its parent.
   *
   * Object fields and array elements are visited in order, and this value is visited last. `f` receives the accumulator and
   * the visited value.
   */
  final def foldUp[A](initial: A)(f: (A, Json) => A): A = {
    // bottom (leaves) up
    val a = self match {
      case Obj(fields) =>
        fields.map(_._2).foldLeft(initial)((acc, next) => next.foldUp(acc)(f))

      case Arr(elements) =>
        elements.foldLeft(initial)((acc, next) => next.foldUp(acc)(f))

      case _ =>
        initial
    }
    f(a, self)
  }

  /**
   * Like `foldUp`, but only values for which `pf` is defined update the accumulator.
   *
   * For every other value the accumulator is kept as it is.
   */
  final def foldUpSome[A](initial: A)(pf: PartialFunction[(A, Json), A]): A = {
    val lifted = pf.lift
    val total  = (a: A, json: Json) => lifted((a, json)).getOrElse(a)
    foldUp(initial)(total)
  }

  /**
   * Returns the node that `cursor` points to.
   *
   * The result is `Left` with an error message when a field or an array index on the path does not exist, or when a node
   * on the path has a different type than the cursor expects (for example `expected object`).
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"users":[{"name":"Ada"},{"name":"Alan"}]}""".fromJson[Json].toOption.get
   *
   * val name = JsonCursor.field("users").isArray.element(1).isObject.field("name")
   *
   * json.get(name)
   * // Right("Alan")
   *
   * json.get(JsonCursor.field("users").isArray.element(5))
   * // Left(The array does not have index 5)
   *
   * json.get(JsonCursor.field("users").isString)
   * // Left(expected string)
   * }}}
   */
  final def get[A <: Json](cursor: JsonCursor[_, A]): Either[String, A] =
    cursor match {
      case JsonCursor.Identity => Right(self)

      case JsonCursor.DownField(parent, field) =>
        self.get(parent).flatMap { case Obj(fields) =>
          fields.collectFirst { case kv if kv._1 == field => Right(kv._2) } match {
            case Some(x) => x
            case _       => Left(s"No such field: '$field'")
          }
        }

      case JsonCursor.DownElement(parent, index) =>
        self.get(parent).flatMap { case a: Arr =>
          a.elements.lift(index).map(Right(_)).getOrElse(Left(s"The array does not have index ${index}"))
        }

      case JsonCursor.FilterType(parent, jsonType) =>
        if (parent eq JsonCursor.identity) jsonType.get(self)
        else self.get(parent).flatMap(jsonType.get)
    }

  override final def hashCode: Int =
    self match {
      case s: Str  => s.value.hashCode
      case n: Num  => n.value.hashCode
      case b: Bool => b.value.hashCode
      case o: Obj  =>
        var result = 0
        o.fields.foreach(tuple => result = result ^ tuple.hashCode)
        result
      case a: Arr =>
        var result = 0
        a.elements.foreach { json =>
          result = 31 * result + json.hashCode
        }
        result
      case _ => 1
    }

  /**
   * Intersects two JSON values of the same kind.
   *
   * When both values are `Json.Obj`, the result keeps the fields that appear on both sides with equal names and equal
   * values, in the order of this object. When both are `Json.Arr`, the result keeps the elements that appear in both
   * arrays. Nested values are compared whole, not intersected recursively.
   *
   * {{{
   * import zio.json._
   *
   * val left  = """{"a":1,"b":2}""".fromJson[Json].toOption.get
   * val right = """{"a":1,"b":3}""".fromJson[Json].toOption.get
   *
   * left.intersect(right)
   * // Right({"a":1})
   *
   * left.intersect(Json.Num(1))
   * // Left(Non compatible types)
   * }}}
   *
   * @param that
   *   The value to intersect with
   * @return
   *   The intersection if both values are objects or both are arrays, `Left("Non compatible types")` otherwise
   */
  final def intersect(that: Json): Either[String, Json] =
    (self, that) match {
      case (Obj(fields1), Obj(fields2)) =>
        Right(Obj(fields1.intersect(fields2)))
      case (Arr(fields1), Arr(fields2)) =>
        Right(Arr(fields1.intersect(fields2)))
      case _ => Left("Non compatible types")
    }

  /**
   * Merges `that` into this value.
   *
   *   - merging objects results in a new objects with all pairs of both sides, with the right hand side being used on
   *     key conflicts
   *   - merging arrays results in all of the individual elements being merged
   *   - scalar values will be replaced by the right hand side
   *
   * {{{
   * import zio.json._
   *
   * val left  = """{"a":{"x":1,"y":2},"b":[1,2]}""".fromJson[Json].toOption.get
   * val right = """{"a":{"y":3},"b":[9],"c":true}""".fromJson[Json].toOption.get
   *
   * left.merge(right)
   * // {"a":{"x":1,"y":3},"b":[9,2],"c":true}
   * }}}
   */
  final def merge(that: Json): Json =
    (self, that) match {
      case (a: Obj, b: Obj) => a merge b
      case (a: Arr, b: Arr) => a merge b
      case (_, r)           => r
    }

  /**
   * Moves the node that the `from` cursor points to, to the location that the `to` cursor points to.
   *
   * The node at `to` is replaced by the node from `from`, and the node at `from` is then deleted. The result is `Left` when
   * either cursor points to a node that does not exist; an object field that does not exist yet is not created.
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"draft":"hello","posts":["old","older"]}""".fromJson[Json].toOption.get
   *
   * json.relocate(JsonCursor.field("draft"), JsonCursor.field("posts").isArray.element(0))
   * // Right({"posts":["hello","older"]})
   * }}}
   *
   * @param from
   *   Cursor which specifies the node to relocate
   * @param to
   *   Cursor which specifies the location to relocate the node to
   * @return
   *   Json with the relocated node if the nodes specified by both cursors exist, error otherwise
   */
  final def relocate(from: JsonCursor[_, _], to: JsonCursor[_, _]): Either[String, Json] = {
    val f = from.asInstanceOf[JsonCursor[_, Json]]
    val t = to.asInstanceOf[JsonCursor[_, Json]]
    self.get(f).flatMap(json => self.transformAt(t)(_ => json).flatMap(_.delete(f)))
  }

  /**
   * Transforms the node that `cursor` points to with `f`.
   *
   * The result is `Left` with an error message when the node does not exist or has a different type than the cursor
   * expects.
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"name":"Ada","age":36}""".fromJson[Json].toOption.get
   *
   * json.transformAt(JsonCursor.field("name").isString)(s => Json.Str(s.value.toUpperCase))
   * // Right({"name":"ADA","age":36})
   * }}}
   *
   * @param cursor
   *   Cursor which specifies node to transform
   * @param f
   *   Function used to transform node
   * @tparam A
   *   refined node type
   * @return
   *   Json with transformed node if node specified by cursor exists, error otherwise
   */
  final def transformAt[A <: Json](cursor: JsonCursor[_, A])(f: A => Json): Either[String, Json] =
    transformOrDelete(cursor, delete = false)(x => Right(f(x)))

  /**
   * Rebuilds this value from the top down: `f` is applied to a value first, and then to each child of the result.
   *
   * Because `f` runs before the children are visited, a replacement that has children has those children transformed as well.
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"a":"x","b":["y"]}""".fromJson[Json].toOption.get
   *
   * json.transformDown { case Json.Str(s) => Json.Str(s + "!"); case other => other }
   * // {"a":"x!","b":["y!"]}
   * }}}
   */
  final def transformDown(f: Json => Json): Json = {
    def loop(json: Json): Json =
      f(json) match {
        case o: Obj => Obj(o.fields.map(kv => (kv._1, loop(kv._2))))
        case a: Arr => Arr(a.elements.map(loop(_)))
        case json   => json
      }

    loop(self)
  }

  /**
   * Like `transformDown`, but values for which `pf` is not defined are kept as they are.
   */
  final def transformDownSome(pf: PartialFunction[Json, Json]): Json = {
    val lifted = pf.lift
    val total  = (json: Json) => lifted(json).getOrElse(json)

    self.transformDown(total)
  }

  @tailrec
  private def transformOrDelete[A <: Json](cursor: JsonCursor[_, A], delete: Boolean)(
    f: A => Either[String, Json]
  ): Either[String, Json] =
    cursor match {
      case JsonCursor.Identity => f(self)

      case JsonCursor.DownField(parent, key) =>
        self.transformOrDelete(parent, false) { case Obj(fields) =>
          val (left, right) = fields.splitWhere(_._1 == key)

          if (right.isEmpty)
            Left(s"No such field: '$key'")
          else if (delete)
            Right(Obj(left ++ right.takeRight(right.length - 1)))
          else
            f(right.head._2).map(value => Obj(left ++ Chunk(key -> value) ++ right.takeRight(right.length - 1)))
        }

      case JsonCursor.DownElement(parent, index) =>
        self.transformOrDelete(parent, false) { case Arr(elements) =>
          val (left, right) = elements.splitAt(index)
          if (right.isEmpty)
            Left(s"The array does not have index ${index}")
          else if (delete)
            Right(Arr(left ++ right.takeRight(right.length - 1)))
          else
            f(right.head).map(value => Arr(left ++ Chunk(value) ++ right.takeRight(right.length - 1)))
        }

      case JsonCursor.FilterType(parent, _ @jsonType) =>
        self.transformOrDelete(parent, delete)(json => jsonType.get(json).flatMap(f))
    }

  /**
   * Rebuilds this value from the bottom up: each child is transformed first, and `f` is then applied to the value rebuilt from the transformed children.
   *
   * {{{
   * import zio.json._
   *
   * val json = """{"a":[1,2,3]}""".fromJson[Json].toOption.get
   *
   * json.transformUp { case Json.Arr(elements) => Json.Arr(elements.reverse); case other => other }
   * // {"a":[3,2,1]}
   * }}}
   */
  final def transformUp(f: Json => Json): Json = {
    def loop(json: Json): Json =
      json match {
        case o: Obj => f(Obj(o.fields.map(kv => (kv._1, loop(kv._2)))))
        case a: Arr => f(Arr(a.elements.map(loop(_))))
        case json   => f(json)
      }

    loop(self)
  }

  /**
   * Like `transformUp`, but values for which `pf` is not defined are kept as they are.
   */
  final def transformUpSome(pf: PartialFunction[Json, Json]): Json = {
    val lifted = pf.lift
    val total  = (json: Json) => lifted(json).getOrElse(json)

    self.transformUp(total)
  }

  /**
   * Returns this value typed as a `Json`.
   */
  final def widen: Json = this

  override def toString(): String = Json.encoder.encodeJson(this, None).toString
}

/**
 * Constructors and instances for [[Json]].
 *
 * `Json.apply` builds an object from fields; `Json.Obj`, `Json.Arr`, `Json.Str`, `Json.Num`, `Json.Bool` and `Json.Null`
 * build the individual kinds of value. `Json` also has an implicit encoder, decoder and codec.
 */
object Json {
  /**
   * A JSON object: an ordered list of field names and values.
   *
   * Field order is kept when encoding, but ignored by equality. The same name can occur more than once if the `Chunk` holds
   * it more than once.
   *
   * {{{
   * val person = Json.Obj("name" -> Json.Str("Ada"), "age" -> Json.Num(36))
   *
   * person.get("name")
   * // Some("Ada")
   *
   * person.contains("email")
   * // false
   *
   * person.add("email", Json.Str("ada@example.com"))
   * // {"name":"Ada","age":36,"email":"ada@example.com"}
   * }}}
   */
  final case class Obj(fields: Chunk[(String, Json)]) extends Json {
    /**
     * Returns `true` when a field called `key` exists.
     */
    def contains(key: String): Boolean                      = fields.exists(_._1 == key)
    /**
     * Returns the value of the first field called `key`, or `None` when there is none.
     */
    def get(key: String): Option[Json]                      = fields.find(_._1 == key).map(_._2)
    /**
     * Returns `true` when the object has no fields.
     */
    def isEmpty: Boolean                                    = fields.isEmpty
    /**
     * Returns `true` when the object has at least one field.
     */
    def nonEmpty: Boolean                                   = !isEmpty
    /**
     * The field names, in field order.
     */
    lazy val keys: Chunk[String]                            = fields.map(_._1)
    /**
     * The field values, in field order.
     */
    lazy val values: Chunk[Json]                            = fields.map(_._2)
    /**
     * Converts the fields to a `Map`. When a name occurs more than once, the last value is kept.
     */
    def toMap: Map[String, Json]                            = fields.toMap
    /**
     * Adds the field `key` with `value`, by merging a one-field object into this one.
     *
     * A name that does not exist yet is appended after the existing fields. When the name exists, the two values are combined
     * with `Json.merge`: for scalars the new `value` replaces the old one, and for objects and arrays the two are merged.
     */
    def add(key: String, value: Json): Json.Obj             = merge(Json.Obj(key -> value))
    /**
     * Alias for `add` that takes the field as a pair.
     *
     * Despite the operator name, the new field is appended after the existing fields.
     */
    def +:(entry: (String, Json)): Json.Obj                 = add(entry._1, entry._2)
    /**
     * Removes every field called `key`.
     */
    def remove(key: String): Json.Obj                       = Json.Obj(fields.filterNot(_._1 == key))
    /**
     * Applies `f` to each field value and keeps the names.
     */
    def mapValues(f: Json => Json): Json.Obj                = Json.Obj(fields.map(e => e._1 -> f(e._2)))
    /**
     * Keeps the fields for which `pred` holds.
     */
    def filter(pred: ((String, Json)) => Boolean): Json.Obj = Json.Obj(fields.filter(pred))
    /**
     * Keeps the fields whose name satisfies `pred`.
     */
    def filterKeys(pred: String => Boolean): Json.Obj       = Json.Obj(fields.filter(e => pred(e._1)))
    /**
     * Merges `that` into this object.
     *
     * The result has the fields of this object followed by the fields that only `that` has. When both objects have a field with
     * the same name, the two values are combined with `Json.merge`, so for scalars the value from `that` is used.
     */
    def merge(that: Json.Obj): Json.Obj                     = {
      val fields1  = this.fields
      val fields2  = that.fields
      val leftMap  = fields1.toMap
      val rightMap = fields2.toMap

      val lookup =
        (fields1 ++ fields2)
          .foldLeft(Map.empty[String, Int] -> 0) { case ((map, nextIndex), (name, _)) =>
            map.get(name) match {
              case Some(_) => map                          -> nextIndex
              case _       => map.updated(name, nextIndex) -> (nextIndex + 1)
            }
          }
          ._1

      val array = Array.ofDim[(String, Json)](lookup.size)

      lookup.foreach { case (key, index) =>
        array(index) = key -> {
          (leftMap.get(key), rightMap.get(key)) match {
            case (Some(l), Some(r)) => l.merge(r)
            case (None, Some(r))    => r
            case (Some(l), None)    => l
            case (None, None)       => Json.Null // can’t happen
          }
        }
      }

      Json.Obj(Chunk.fromArray(array))
    }

    override def asObject: Some[Json.Obj]                                          = new Some(this)
    override def mapObject(f: Json.Obj => Json.Obj): Json.Obj                      = f(this)
    override def mapObjectKeys(f: String => String): Json.Obj                      = Json.Obj(fields.map(e => f(e._1) -> e._2))
    override def mapObjectValues(f: Json => Json): Json.Obj                        = mapValues(f)
    override def mapObjectEntries(f: ((String, Json)) => (String, Json)): Json.Obj = Json.Obj(fields.map(f))
  }
  /**
   * Constructors and instances for [[Json.Obj]].
   */
  object Obj {
    /**
     * The object with no fields.
     */
    val empty: Obj = new Obj(Chunk.empty)

    /**
     * Creates an object from the given fields. An empty `chunk` gives `Json.Obj.empty`.
     */
    def apply(chunk: Chunk[(String, Json)]): Obj =
      if (chunk.isEmpty) empty
      else new Obj(chunk)

    /**
     * Creates an object from the given fields. Without fields it gives `Json.Obj.empty`.
     */
    def apply(fields: (String, Json)*): Obj =
      if (fields.isEmpty) Obj.empty
      else new Obj(Chunk(fields: _*))

    /**
     * Creates an object with the single field `key`.
     */
    def apply(key: String, value: Json): Obj =
      new Obj(Chunk.single(key -> value))

    private lazy val objd                  = JsonDecoder.keyValueChunk[String, Json]
    /**
     * Decodes a JSON object. It fails for any other kind of JSON value.
     */
    implicit val decoder: JsonDecoder[Obj] = new JsonDecoder.AbstractJsonDecoder[Obj] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Obj =
        Obj(objd.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Obj =
        json match {
          case obj: Obj => obj
          case _        => Lexer.error("Not an object", trace)
        }
    }
    private lazy val obje                  = JsonEncoder.keyValueChunk[String, Json]
    /**
     * Encodes the fields as a JSON object, in field order.
     */
    implicit val encoder: JsonEncoder[Obj] = new JsonEncoder.AbstractJsonEncoder[Obj] {
      def unsafeEncode(a: Obj, indent: Option[Int], out: Write): Unit =
        obje.unsafeEncode(a.fields, indent, out)

      override final def toJsonAST(a: Obj): Either[String, Json] = new Right(a)
    }

    /**
     * Codec for JSON objects, made from `encoder` and `decoder`.
     */
    implicit val codec: JsonCodec[Obj] = JsonCodec(encoder, decoder)
  }
  /**
   * A JSON array: an ordered list of values.
   *
   * {{{
   * val numbers = Json.Arr(Json.Num(1), Json.Num(2))
   *
   * numbers.isEmpty
   * // false
   *
   * numbers.merge(Json.Arr(Json.Num(9)))
   * // [9,2]
   * }}}
   */
  final case class Arr(elements: Chunk[Json]) extends Json {
    /**
     * Returns `true` when the array has no elements.
     */
    def isEmpty: Boolean  = elements.isEmpty
    /**
     * Returns `true` when the array has at least one element.
     */
    def nonEmpty: Boolean = !isEmpty

    /**
     * Merges `that` into this array, element by element.
     *
     * The element at each index is combined with the element of `that` at the same index using `Json.merge`. When one array
     * is longer, its remaining elements are kept.
     */
    def merge(that: Json.Arr): Json.Arr = {
      val elements1 = this.elements
      val elements2 = that.elements

      val leftover =
        if (elements1.length > elements2.length) elements1.drop(elements2.length)
        else elements2.drop(elements1.length)

      Json.Arr(elements1.zip(elements2).map { case (left, right) =>
        left.merge(right)
      } ++ leftover)
    }

    override def asArray: Some[Chunk[Json]]                        = new Some(elements)
    override def mapArray(f: Chunk[Json] => Chunk[Json]): Json.Arr = Json.Arr(f(elements))
    override def mapArrayValues(f: Json => Json): Json.Arr         = Json.Arr(elements.map(f))
  }
  /**
   * Constructors and instances for [[Json.Arr]].
   */
  object Arr {
    /**
     * The array with no elements.
     */
    val empty: Arr = new Arr(Chunk.empty)

    /**
     * Creates an array from the given elements. An empty `chunk` gives `Json.Arr.empty`.
     */
    def apply(chunk: Chunk[Json]): Arr =
      if (chunk.isEmpty) empty
      else new Arr(chunk)

    /**
     * Creates an array from the given elements. Without elements it gives `Json.Arr.empty`.
     */
    def apply(elements: Json*): Arr =
      if (elements.isEmpty) empty
      else new Arr(Chunk(elements: _*))

    private lazy val arrd                  = JsonDecoder.chunk[Json]
    /**
     * Decodes a JSON array. It fails for any other kind of JSON value.
     */
    implicit val decoder: JsonDecoder[Arr] = new JsonDecoder.AbstractJsonDecoder[Arr] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Arr =
        Arr(arrd.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Arr =
        json match {
          case arr: Arr => arr
          case _        => Lexer.error("Not an array", trace)
        }
    }
    private lazy val arre                  = JsonEncoder.chunk[Json]
    /**
     * Encodes the elements as a JSON array.
     */
    implicit val encoder: JsonEncoder[Arr] = new JsonEncoder.AbstractJsonEncoder[Arr] {
      def unsafeEncode(a: Arr, indent: Option[Int], out: Write): Unit =
        arre.unsafeEncode(a.elements, indent, out)

      override final def toJsonAST(a: Arr): Either[String, Json] = new Right(a)
    }

    /**
     * Codec for JSON arrays, made from `encoder` and `decoder`.
     */
    implicit val codec: JsonCodec[Arr] = JsonCodec(encoder, decoder)
  }
  /**
   * A JSON boolean.
   *
   * {{{
   * Json.Bool(true)
   * // true
   * }}}
   */
  final case class Bool(value: Boolean) extends Json {
    override def asBoolean: Some[Boolean]                     = new Some(value)
    override def mapBoolean(f: Boolean => Boolean): Json.Bool = Json.Bool(f(value))
  }

  /**
   * Constructors and instances for [[Json.Bool]].
   */
  object Bool {
    /**
     * The JSON value `false`.
     */
    val False: Bool = new Bool(false)
    /**
     * The JSON value `true`.
     */
    val True: Bool  = new Bool(true)

    /**
     * Returns `Json.Bool.True` or `Json.Bool.False`.
     */
    def apply(value: Boolean): Bool =
      if (value) True
      else False

    /**
     * Decodes a JSON boolean. It fails for any other kind of JSON value.
     */
    implicit val decoder: JsonDecoder[Bool] = new JsonDecoder.AbstractJsonDecoder[Bool] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Bool =
        Bool(JsonDecoder.boolean.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Bool =
        json match {
          case b: Bool => b
          case _       => Lexer.error("Not a bool value", trace)
        }
    }
    /**
     * Encodes the boolean as `true` or `false`.
     */
    implicit val encoder: JsonEncoder[Bool] = new JsonEncoder.AbstractJsonEncoder[Bool] {
      def unsafeEncode(a: Bool, indent: Option[Int], out: Write): Unit =
        JsonEncoder.boolean.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Bool): Either[String, Json] = new Right(a)
    }

    /**
     * Codec for JSON booleans, made from `encoder` and `decoder`.
     */
    implicit val codec: JsonCodec[Bool] = JsonCodec(encoder, decoder)
  }
  /**
   * A JSON string.
   *
   * {{{
   * Json.Str("hello")
   * // "hello"
   * }}}
   */
  final case class Str(value: String) extends Json {
    override def asString: Some[String]                   = new Some(value)
    override def mapString(f: String => String): Json.Str = new Json.Str(f(value))
  }
  /**
   * Constructors and instances for [[Json.Str]].
   */
  object Str {
    /**
     * Decodes a JSON string. It fails for any other kind of JSON value.
     */
    implicit val decoder: JsonDecoder[Str] = new JsonDecoder.AbstractJsonDecoder[Str] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Str =
        Str(JsonDecoder.string.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Str =
        json match {
          case s: Str => s
          case _      => Lexer.error("Not a string value", trace)
        }
    }
    /**
     * Encodes the string as a quoted and escaped JSON string.
     */
    implicit val encoder: JsonEncoder[Str] = new JsonEncoder.AbstractJsonEncoder[Str] {
      def unsafeEncode(a: Str, indent: Option[Int], out: Write): Unit =
        JsonEncoder.string.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Str): Either[String, Json] = new Right(a)
    }

    /**
     * Codec for JSON strings, made from `encoder` and `decoder`.
     */
    implicit val codec: JsonCodec[Str] = JsonCodec(encoder, decoder)
  }
  /**
   * A JSON number, held as a `java.math.BigDecimal`.
   *
   * Equality and hash codes follow `java.math.BigDecimal`, which compares the scale as well: `Json.Num(1.0)` is not equal
   * to `Json.Num(1)`.
   *
   * {{{
   * Json.Num(42)
   * // 42
   *
   * Json.Num(BigDecimal("3.14"))
   * // 3.14
   * }}}
   */
  final case class Num(value: java.math.BigDecimal) extends Json {
    override def asNumber: Some[Json.Num]                                             = new Some(this)
    override def mapNumber(f: java.math.BigDecimal => java.math.BigDecimal): Json.Num = new Json.Num(f(value))
  }
  /**
   * Constructors and instances for [[Json.Num]].
   */
  object Num {
    /**
     * Creates a number from a `Byte`.
     */
    @inline def apply(value: Byte): Num  = apply(value.toInt)
    /**
     * Creates a number from a `Short`.
     */
    @inline def apply(value: Short): Num = apply(value.toInt)
    /**
     * Creates a number from an `Int`.
     */
    def apply(value: Int): Num           = new Num({
      if (value < 512 && value > -512) new java.math.BigDecimal(value)
      else BigDecimal(value).bigDecimal
    })
    /**
     * Creates a number from a `Long`.
     */
    def apply(value: Long): Num = new Num({
      if (value < 512 && value > -512) new java.math.BigDecimal(value)
      else BigDecimal(value).bigDecimal
    })
    /**
     * Creates a number from a `BigDecimal`.
     */
    @inline def apply(value: BigDecimal): Num = new Num(value.bigDecimal)
    /**
     * Creates a number from a `BigInt`.
     */
    def apply(value: BigInt): Num             =
      if (value.isValidLong) apply(value.toLong)
      else new Json.Num(new java.math.BigDecimal(value.bigInteger))
    /**
     * Creates a number from a `java.math.BigInteger`.
     */
    def apply(value: java.math.BigInteger): Num =
      if (value.bitLength < 64) apply(value.longValue)
      else new Json.Num(new java.math.BigDecimal(value))
    /**
     * Creates a number from a `Float`, using the shortest decimal text of the float.
     *
     * Throws a `NumberFormatException` for `NaN` and the infinite values.
     */
    def apply(value: Float): Num  = new Num(new java.math.BigDecimal(SafeNumbers.toString(value)))
    /**
     * Creates a number from a `Double`, using the shortest decimal text of the double.
     *
     * Throws a `NumberFormatException` for `NaN` and the infinite values.
     */
    def apply(value: Double): Num = new Num(new java.math.BigDecimal(SafeNumbers.toString(value)))

    /**
     * Decodes a JSON number. It fails for any other kind of JSON value.
     */
    implicit val decoder: JsonDecoder[Num] = new JsonDecoder.AbstractJsonDecoder[Num] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Num =
        Num(JsonDecoder.bigDecimal.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Num =
        json match {
          case n: Num => n
          case _      => Lexer.error("Not a number", trace)
        }
    }
    /**
     * Encodes the number as a JSON number.
     */
    implicit val encoder: JsonEncoder[Num] = new JsonEncoder.AbstractJsonEncoder[Num] {
      def unsafeEncode(a: Num, indent: Option[Int], out: Write): Unit =
        JsonEncoder.bigDecimal.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Num): Either[String, Num] = new Right(a)
    }

    /**
     * Codec for JSON numbers, made from `encoder` and `decoder`.
     */
    implicit val codec: JsonCodec[Num] = JsonCodec(encoder, decoder)
  }
  /**
   * The type of the `Json.Null` value.
   */
  type Null = Null.type
  /**
   * The JSON `null` value.
   *
   * {{{
   * Json.Null
   * // null
   * }}}
   */
  case object Null extends Json {
    private[this] val nullChars: Array[Char]     = "null".toCharArray
    /**
     * Decodes the JSON text `null`. It fails for any other JSON value.
     */
    implicit val decoder: JsonDecoder[Null.type] = new JsonDecoder.AbstractJsonDecoder[Null.type] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Null.type = {
        Lexer.readChars(trace, in, nullChars, "null")
        Null
      }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Null.type = {
        if (json ne Null) Lexer.error("Not null", trace)
        Null
      }
    }
    /**
     * Encodes the value as the JSON text `null`.
     */
    implicit val encoder: JsonEncoder[Null.type] = new JsonEncoder.AbstractJsonEncoder[Null.type] {
      def unsafeEncode(a: Null.type, indent: Option[Int], out: Write): Unit =
        out.write("null")

      override final def toJsonAST(a: Null.type): Either[String, Json] = new Right(a)
    }

    /**
     * Codec for the JSON `null` value, made from `encoder` and `decoder`.
     */
    implicit val codec: JsonCodec[Null.type] = JsonCodec(encoder, decoder)

    override def asNull: Some[Unit] = new Some(())
  }

  /**
   * Decodes any JSON value, choosing the kind of value from its first character.
   */
  implicit val decoder: JsonDecoder[Json] = new JsonDecoder.AbstractJsonDecoder[Json] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Json = {
      val c = in.nextNonWhitespace()
      in.retract()
      (c: @switch) match {
        case 'n'                                                             => Null.decoder.unsafeDecode(trace, in)
        case 'f' | 't'                                                       => Bool.decoder.unsafeDecode(trace, in)
        case '{'                                                             => Obj.decoder.unsafeDecode(trace, in)
        case '['                                                             => Arr.decoder.unsafeDecode(trace, in)
        case '"'                                                             => Str.decoder.unsafeDecode(trace, in)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          Num.decoder.unsafeDecode(trace, in)
        case c =>
          Lexer.error(s"unexpected '$c'", trace)
      }
    }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Json =
      json
  }
  /**
   * Encodes any JSON value as JSON text.
   */
  implicit val encoder: JsonEncoder[Json] = new JsonEncoder.AbstractJsonEncoder[Json] {
    def unsafeEncode(a: Json, indent: Option[Int], out: Write): Unit =
      a match {
        case j: Obj  => Obj.encoder.unsafeEncode(j, indent, out)
        case j: Arr  => Arr.encoder.unsafeEncode(j, indent, out)
        case j: Bool => Bool.encoder.unsafeEncode(j, indent, out)
        case j: Str  => Str.encoder.unsafeEncode(j, indent, out)
        case j: Num  => Num.encoder.unsafeEncode(j, indent, out)
        case Null    => Null.encoder.unsafeEncode(Null, indent, out)
      }

    override final def toJsonAST(a: Json): Either[String, Json] = new Right(a)
  }

  /**
   * Codec for any JSON value, made from `encoder` and `decoder`.
   */
  implicit val codec: JsonCodec[Json] = JsonCodec(encoder, decoder)

  /**
   * Creates a JSON object from the given fields. Without fields it gives the empty object `{}`.
   *
   * {{{
   * Json("name" -> Json.Str("Ada"), "age" -> Json.Num(36))
   * // {"name":"Ada","age":36}
   * }}}
   */
  def apply(fields: (String, Json)*): Json =
    if (fields.isEmpty) Obj.empty
    else new Obj(Chunk(fields: _*))
}
