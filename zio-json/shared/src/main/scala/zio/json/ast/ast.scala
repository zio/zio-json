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
import zio.json.internal._

import scala.annotation._

/**
 * This AST of JSON is made available so that arbitrary JSON may be included as part of a business object, it is not
 * used as an intermediate representation, unlike most other JSON libraries. It is not advised to `.map` or `.mapOrFail`
 * from these decoders, since a higher performance decoder is often available.
 *
 * Beware of the potential for DOS attacks, since an attacker can provide much more data than is perhaps needed.
 *
 * Also beware of converting `Num` (a `BigDecimal`) into any other kind of number, since many of the stdlib functions
 * are non-total or are known DOS vectors (e.g. calling `.toBigInteger` on a "1e214748364" will consume an excessive
 * amount of heap memory). JsonValue / Json / JValue
 */
sealed abstract class Json { self =>
  final def as[A](implicit decoder: JsonDecoder[A]): Either[String, A] = decoder.fromJsonAST(self)

  def asNull: Option[Unit]         = None
  def asBoolean: Option[Boolean]   = None
  def asNumber: Option[Json.Num]   = None
  def asString: Option[String]     = None
  def asArray: Option[Chunk[Json]] = None
  def asObject: Option[Json.Obj]   = None

  def mapBoolean(@nowarn f: Boolean => Boolean): Json                          = self
  def mapNumber(@nowarn f: java.math.BigDecimal => java.math.BigDecimal): Json = self
  def mapString(@nowarn f: String => String): Json                             = self
  def mapArray(@nowarn f: Chunk[Json] => Chunk[Json]): Json                    = self
  def mapArrayValues(@nowarn f: Json => Json): Json                            = self
  def mapObject(@nowarn f: Json.Obj => Json.Obj): Json                         = self
  def mapObjectKeys(@nowarn f: String => String): Json                         = self
  def mapObjectValues(@nowarn f: Json => Json): Json                           = self
  def mapObjectEntries(@nowarn f: ((String, Json)) => (String, Json)): Json    = self

  final def arrayOrObject[X](
    or: => X,
    jsonArray: Chunk[Json] => X,
    jsonObject: Json.Obj => X
  ): X = self match {
    case Json.Arr(a) => jsonArray(a)
    case o: Json.Obj => jsonObject(o)
    case _           => or
  }

  /**
   * Deletes json node specified by given cursor
   * @param cursor
   *   Cursor which specifies node to delete
   * @return
   *   Json without specified node if node specified by cursor exists, error otherwise
   */
  final def delete(cursor: JsonCursor[_, _]): Either[String, Json] = {
    val c = cursor.asInstanceOf[JsonCursor[_, Json]]

    transformOrDelete(c, delete = true)(_ => Right(Json.Null))
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

  final def foldDown[A](initial: A)(f: (A, Json) => A): A = {
    val a = f(initial, self)

    self match {
      case Obj(fields)   => fields.map(_._2).foldLeft(a)((a, json) => json.foldDown(a)(f))
      case Arr(elements) => elements.foldLeft(a)((a, json) => json.foldDown(a)(f))
      case _             => a
    }
  }

  final def foldDownSome[A](initial: A)(pf: PartialFunction[(A, Json), A]): A = {
    val lifted = pf.lift
    val total  = (a: A, json: Json) => lifted((a, json)).getOrElse(a)

    foldDown(initial)(total)
  }

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

  final def foldUpSome[A](initial: A)(pf: PartialFunction[(A, Json), A]): A = {
    val lifted = pf.lift
    val total  = (a: A, json: Json) => lifted((a, json)).getOrElse(a)
    foldUp(initial)(total)
  }

  final def get[A <: Json](cursor: JsonCursor[_, A]): Either[String, A] =
    cursor match {
      case JsonCursor.Identity => Right(self)

      case JsonCursor.DownField(parent, field) =>
        self.get(parent).flatMap { case Obj(fields) =>
          fields.collectFirst { case (key, value) if key == field => Right(value) } match {
            case Some(x) => x
            case None    => Left(s"No such field: '$field'")
          }
        }

      case JsonCursor.DownElement(parent, index) =>
        self.get(parent).flatMap { case Arr(elements) =>
          elements.lift(index).map(Right(_)).getOrElse(Left(s"The array does not have index ${index}"))
        }

      case JsonCursor.FilterType(parent, t @ jsonType) =>
        self.get(parent).flatMap(x => jsonType.get(x))
    }

  override final def hashCode: Int =
    31 * {
      self match {
        case Obj(fields) =>
          var result = 0
          fields.foreach(tuple => result = result ^ tuple.hashCode)
          result
        case Arr(elements) =>
          var result = 0
          var index  = 0
          elements.foreach { json =>
            result = result ^ (index, json).hashCode
            index += 1
          }
          result
        case Bool(value) => value.hashCode
        case Str(value)  => value.hashCode
        case Num(value)  => value.hashCode
        case Json.Null   => 1
      }
    }

  /**
   * Intersects JSON values. If both values are `Obj` or `Arr` method returns intersections of its fields/elements,
   * otherwise it returns error
   * @param that
   * @return
   *   Intersected json if type are compatible, error otherwise
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
   *   - merging objects results in a new objects with all pairs of both sides, with the right hand side being used on
   *     key conflicts
   *
   *   - merging arrays results in all of the individual elements being merged
   *
   *   - scalar values will be replaced by the right hand side
   */
  final def merge(that: Json): Json =
    (self, that) match {
      case (a: Obj, b: Obj) => a merge b
      case (a: Arr, b: Arr) => a merge b
      case (_, r)           => r
    }

  /**
   * Relocates Json node from location specified by `from` cursor to location specified by `to` cursor.
   *
   * @param from
   *   Cursor which specifies node to relocate
   * @return
   *   Json without specified node if node specified by cursor exists, error otherwise
   * @param to
   *   Cursor which specifies location where to relocate node
   * @return
   *   Json with relocated node if node specified by cursors exist, error otherwise
   */
  final def relocate(from: JsonCursor[_, _], to: JsonCursor[_, _]): Either[String, Json] = {
    val f = from.asInstanceOf[JsonCursor[_, Json]]
    val t = to.asInstanceOf[JsonCursor[_, Json]]
    self.get(f).flatMap(json => self.transformAt(t)(_ => json).flatMap(_.delete(f)))
  }

  /**
   * Transforms json node specified by given cursor
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

  final def transformDown(f: Json => Json): Json = {
    def loop(json: Json): Json =
      f(json) match {
        case Obj(fields)   => Obj(fields.map { case (name, value) => (name, loop(value)) })
        case Arr(elements) => Arr(elements.map(loop(_)))
        case json          => json
      }

    loop(self)
  }

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

      case JsonCursor.FilterType(parent, t @ jsonType) =>
        self.transformOrDelete(parent, delete)(json => jsonType.get(json).flatMap(f))
    }

  final def transformUp(f: Json => Json): Json = {
    def loop(json: Json): Json =
      json match {
        case Obj(fields)   => f(Obj(fields.map { case (name, value) => (name, loop(value)) }))
        case Arr(elements) => f(Arr(elements.map(loop(_))))
        case json          => f(json)
      }

    loop(self)
  }

  final def transformUpSome(pf: PartialFunction[Json, Json]): Json = {
    val lifted = pf.lift
    val total  = (json: Json) => lifted(json).getOrElse(json)

    self.transformUp(total)
  }

  final def widen: Json = this

  override def toString(): String = Json.encoder.encodeJson(this, None).toString
}

object Json {
  final case class Obj(fields: Chunk[(String, Json)]) extends Json {
    def contains(key: String): Boolean                      = fields.exists(_._1 == key)
    def get(key: String): Option[Json]                      = fields.find(_._1 == key).map(_._2)
    def isEmpty: Boolean                                    = fields.isEmpty
    def nonEmpty: Boolean                                   = !isEmpty
    lazy val keys: Chunk[String]                            = fields.map(_._1)
    lazy val values: Chunk[Json]                            = fields.map(_._2)
    def toMap: Map[String, Json]                            = fields.toMap
    def add(key: String, value: Json): Json.Obj             = merge(Json.Obj(key -> value))
    def +:(entry: (String, Json)): Json.Obj                 = add(entry._1, entry._2)
    def remove(key: String): Json.Obj                       = Json.Obj(fields.filterNot(_._1 == key))
    def mapValues(f: Json => Json): Json.Obj                = Json.Obj(fields.map(e => e._1 -> f(e._2)))
    def filter(pred: ((String, Json)) => Boolean): Json.Obj = Json.Obj(fields.filter(pred))
    def filterKeys(pred: String => Boolean): Json.Obj       = Json.Obj(fields.filter(e => pred(e._1)))
    def merge(that: Json.Obj): Json.Obj = {
      val fields1  = this.fields
      val fields2  = that.fields
      val leftMap  = fields1.toMap
      val rightMap = fields2.toMap

      val lookup =
        (fields1 ++ fields2)
          .foldLeft(Map.empty[String, Int] -> 0) { case ((map, nextIndex), (name, _)) =>
            map.get(name) match {
              case None    => map.updated(name, nextIndex) -> (nextIndex + 1)
              case Some(_) => map                          -> nextIndex
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

    override def asObject: Some[Json.Obj]                                          = Some(this)
    override def mapObject(f: Json.Obj => Json.Obj): Json.Obj                      = f(this)
    override def mapObjectKeys(f: String => String): Json.Obj                      = Json.Obj(fields.map(e => f(e._1) -> e._2))
    override def mapObjectValues(f: Json => Json): Json.Obj                        = mapValues(f)
    override def mapObjectEntries(f: ((String, Json)) => (String, Json)): Json.Obj = Json.Obj(fields.map(f))
  }
  object Obj {
    def apply(fields: (String, Json)*): Obj = Obj(Chunk(fields: _*))

    private lazy val objd = JsonDecoder.keyValueChunk[String, Json]
    implicit val decoder: JsonDecoder[Obj] = new JsonDecoder[Obj] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Obj =
        Obj(objd.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Obj =
        json match {
          case obj @ Obj(_) => obj
          case _            => Lexer.error("Not an object", trace)
        }
    }
    private lazy val obje = JsonEncoder.keyValueChunk[String, Json]
    implicit val encoder: JsonEncoder[Obj] = new JsonEncoder[Obj] {
      def unsafeEncode(a: Obj, indent: Option[Int], out: Write): Unit =
        obje.unsafeEncode(a.fields, indent, out)

      override final def toJsonAST(a: Obj): Either[String, Json] = Right(a)
    }

    implicit val codec: JsonCodec[Obj] = JsonCodec(encoder, decoder)
  }
  final case class Arr(elements: Chunk[Json]) extends Json {
    def isEmpty: Boolean  = elements.isEmpty
    def nonEmpty: Boolean = !isEmpty

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

    override def asArray: Some[Chunk[Json]]                        = Some(elements)
    override def mapArray(f: Chunk[Json] => Chunk[Json]): Json.Arr = Json.Arr(f(elements))
    override def mapArrayValues(f: Json => Json): Json.Arr         = Json.Arr(elements.map(f))
  }
  object Arr {
    def apply(elements: Json*): Arr = Arr(Chunk(elements: _*))

    private lazy val arrd = JsonDecoder.chunk[Json]
    implicit val decoder: JsonDecoder[Arr] = new JsonDecoder[Arr] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Arr =
        Arr(arrd.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Arr =
        json match {
          case arr @ Arr(_) => arr
          case _            => Lexer.error("Not an array", trace)
        }
    }
    private lazy val arre = JsonEncoder.chunk[Json]
    implicit val encoder: JsonEncoder[Arr] = new JsonEncoder[Arr] {
      def unsafeEncode(a: Arr, indent: Option[Int], out: Write): Unit =
        arre.unsafeEncode(a.elements, indent, out)

      override final def toJsonAST(a: Arr): Either[String, Json] = Right(a)
    }

    implicit val codec: JsonCodec[Arr] = JsonCodec(encoder, decoder)
  }
  final case class Bool(value: Boolean) extends Json {
    override def asBoolean: Some[Boolean]                     = Some(value)
    override def mapBoolean(f: Boolean => Boolean): Json.Bool = Json.Bool(f(value))
  }

  object Bool {
    val False: Bool = Bool(false)
    val True: Bool  = Bool(true)

    implicit val decoder: JsonDecoder[Bool] = new JsonDecoder[Bool] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Bool =
        Bool(JsonDecoder.boolean.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Bool =
        json match {
          case b @ Bool(_) => b
          case _           => Lexer.error("Not a bool value", trace)
        }
    }
    implicit val encoder: JsonEncoder[Bool] = new JsonEncoder[Bool] {
      def unsafeEncode(a: Bool, indent: Option[Int], out: Write): Unit =
        JsonEncoder.boolean.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Bool): Either[String, Json] = Right(a)
    }

    implicit val codec: JsonCodec[Bool] = JsonCodec(encoder, decoder)
  }
  final case class Str(value: String) extends Json {
    override def asString: Some[String]                   = Some(value)
    override def mapString(f: String => String): Json.Str = Json.Str(f(value))
  }
  object Str {
    implicit val decoder: JsonDecoder[Str] = new JsonDecoder[Str] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Str =
        Str(JsonDecoder.string.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Str =
        json match {
          case s @ Str(_) => s
          case _          => Lexer.error("Not a string value", trace)
        }
    }
    implicit val encoder: JsonEncoder[Str] = new JsonEncoder[Str] {
      def unsafeEncode(a: Str, indent: Option[Int], out: Write): Unit =
        JsonEncoder.string.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Str): Either[String, Json] = Right(a)
    }

    implicit val codec: JsonCodec[Str] = JsonCodec(encoder, decoder)
  }
  final case class Num(value: java.math.BigDecimal) extends Json {
    override def asNumber: Some[Json.Num]                                             = Some(this)
    override def mapNumber(f: java.math.BigDecimal => java.math.BigDecimal): Json.Num = Json.Num(f(value))
  }
  object Num {
    def apply(value: Byte): Num       = Num(BigDecimal(value.toInt).bigDecimal)
    def apply(value: Short): Num      = Num(BigDecimal(value.toInt).bigDecimal)
    def apply(value: Int): Num        = Num(BigDecimal(value).bigDecimal)
    def apply(value: Long): Num       = Num(BigDecimal(value).bigDecimal)
    def apply(value: BigDecimal): Num = Num(value.bigDecimal)
    def apply(value: Float): Num      = Num(BigDecimal.decimal(value).bigDecimal)
    def apply(value: Double): Num     = Num(BigDecimal(value).bigDecimal)

    implicit val decoder: JsonDecoder[Num] = new JsonDecoder[Num] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Num =
        Num(JsonDecoder.bigDecimal.unsafeDecode(trace, in))

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Num =
        json match {
          case n @ Num(_) => n
          case _          => Lexer.error("Not a number", trace)
        }
    }
    implicit val encoder: JsonEncoder[Num] = new JsonEncoder[Num] {
      def unsafeEncode(a: Num, indent: Option[Int], out: Write): Unit =
        JsonEncoder.bigDecimal.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Num): Either[String, Num] = Right(a)
    }

    implicit val codec: JsonCodec[Num] = JsonCodec(encoder, decoder)
  }
  type Null = Null.type
  case object Null extends Json {
    private[this] val nullChars: Array[Char] = "null".toCharArray
    implicit val decoder: JsonDecoder[Null.type] = new JsonDecoder[Null.type] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Null.type = {
        Lexer.readChars(trace, in, nullChars, "null")
        Null
      }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Null.type =
        json match {
          case Null => Null
          case _    => Lexer.error("Not null", trace)
        }
    }
    implicit val encoder: JsonEncoder[Null.type] = new JsonEncoder[Null.type] {
      def unsafeEncode(a: Null.type, indent: Option[Int], out: Write): Unit =
        out.write("null")

      override final def toJsonAST(a: Null.type): Either[String, Json] = Right(a)
    }

    implicit val codec: JsonCodec[Null.type] = JsonCodec(encoder, decoder)

    override def asNull: Some[Unit] = Some(())
  }

  implicit val decoder: JsonDecoder[Json] = new JsonDecoder[Json] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Json = {
      val c = in.nextNonWhitespace()
      in.retract()
      (c: @switch) match {
        case 'n'       => Null.decoder.unsafeDecode(trace, in)
        case 'f' | 't' => Bool.decoder.unsafeDecode(trace, in)
        case '{'       => Obj.decoder.unsafeDecode(trace, in)
        case '['       => Arr.decoder.unsafeDecode(trace, in)
        case '"'       => Str.decoder.unsafeDecode(trace, in)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          Num.decoder.unsafeDecode(trace, in)
        case c =>
          Lexer.error(s"unexpected '$c'", trace)
      }
    }

    override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): Json =
      json
  }
  implicit val encoder: JsonEncoder[Json] = new JsonEncoder[Json] {
    def unsafeEncode(a: Json, indent: Option[Int], out: Write): Unit =
      a match {
        case j: Obj  => Obj.encoder.unsafeEncode(j, indent, out)
        case j: Arr  => Arr.encoder.unsafeEncode(j, indent, out)
        case j: Bool => Bool.encoder.unsafeEncode(j, indent, out)
        case j: Str  => Str.encoder.unsafeEncode(j, indent, out)
        case j: Num  => Num.encoder.unsafeEncode(j, indent, out)
        case Null    => Null.encoder.unsafeEncode(Null, indent, out)
      }

    override final def toJsonAST(a: Json): Either[String, Json] = Right(a)
  }

  implicit val codec: JsonCodec[Json] = JsonCodec(encoder, decoder)

  def apply(fields: (String, Json)*): Json = Json.Obj(Chunk(fields: _*))
}
