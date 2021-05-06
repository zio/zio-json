package zio.json.ast

import scala.annotation._

import zio.Chunk
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json._
import zio.json.ast.Json._
import zio.json.internal._

/**
 * This AST of JSON is made available so that arbitrary JSON may be included as
 * part of a business object, it is not used as an intermediate representation,
 * unlike most other JSON libraries. It is not advised to `.map` or `.emap`
 * from these decoders, since a higher performance decoder is often available.
 *
 * Beware of the potential for DOS attacks, since an attacker can provide much
 * more data than is perhaps needed.
 *
 * Also beware of converting `Num` (a `BigDecimal`) into any other kind of
 * number, since many of the stdlib functions are non-total or are known DOS
 * vectors (e.g. calling `.toBigInteger` on a "1e214748364" will consume an
 * excessive amount of heap memory).
 * JsonValue / Json / JValue
 */
sealed abstract class Json { self =>
  final def delete(cursor: JsonCursor[_, _]): Either[String, Json] = ???

  final def diff(that: Json): JsonDiff = JsonDiff(self, that)

  override final def equals(that: Any): Boolean = {
    def objEqual(left: Map[String, Json], right: Chunk[(String, Json)]): Boolean =
      if (right.isEmpty) true
      else
        right.find { case (key, r) =>
          left.get(key) match {
            case Some(l) if l != r => true
            case _                 => false
          }
        }.isEmpty

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
          fields.find(_._1 == field).map(_._2) match {
            case Some(x) => Right(x)
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

  final def intersect(that: Json): Either[String, Json] = ???

  // TODO: Return Either[String, Json]
  final def merge(that: Json): Json =
    (self, that) match {
      case (Obj(fields1), Obj(fields2)) =>
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
              case (None, None)       => Json.Null
            }
          }
        }

        Obj(Chunk.fromArray(array))

      case (Arr(elements1), Arr(elements2)) =>
        val leftover =
          if (elements1.length > elements2.length) elements1.drop(elements2.length)
          else elements2.drop(elements1.length)

        Arr(elements1.zip(elements2).map { case (left, right) =>
          left.merge(right)
        } ++ leftover)

      case (l, r) => ???
    }

  // TODO: Return Either[String, Json]
  final def relocate(from: JsonCursor[_, _], to: JsonCursor[_, _]): Either[String, Json] = ???

  // TODO: Return Either[String, Json]
  final def transformAt[A <: Json](cursor: JsonCursor[_, A])(f: A => Json): Either[String, Json] = ???

  // TODO: Add cursor to all transform / fold methods
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

  // TODO: Add stateful transformations
  //       Stateful & errorful transformations???

  final def widen: Json = this

  override def toString(): String = Json.encoder.encodeJson(this, None).toString
}

object Json {
  implicit val orderingJson: Ordering[Json] =
    new Ordering[Json] {
      def compare(x: Json, y: Json): Int = ???
    }

  final case class Obj(fields: Chunk[(String, Json)]) extends Json
  object Obj {
    def apply(fields: (String, Json)*): Obj = Obj(Chunk(fields: _*))

    private lazy val objd = JsonDecoder.keyValueChunk[String, Json]
    implicit val decoder: JsonDecoder[Obj] = new JsonDecoder[Obj] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Obj =
        Obj(objd.unsafeDecode(trace, in))

      override final def fromJsonAST(json: Json): Either[String, Obj] =
        json match {
          case obj @ Obj(_) => Right(obj)
          case _            => Left(s"Not an object")
        }
    }
    private lazy val obje = JsonEncoder.keyValueChunk[String, Json]
    implicit val encoder: JsonEncoder[Obj] = new JsonEncoder[Obj] {
      def unsafeEncode(a: Obj, indent: Option[Int], out: Write): Unit =
        obje.unsafeEncode(a.fields, indent, out)

      override final def toJsonAST(a: Obj): Either[String, Json] = Right(a)
    }
  }
  final case class Arr(elements: Chunk[Json]) extends Json
  object Arr {
    def apply(elements: Json*): Arr = Arr(Chunk(elements: _*))

    private lazy val arrd = JsonDecoder.chunk[Json]
    implicit val decoder: JsonDecoder[Arr] = new JsonDecoder[Arr] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Arr =
        Arr(arrd.unsafeDecode(trace, in))

      override final def fromJsonAST(json: Json): Either[String, Arr] =
        json match {
          case arr @ Arr(_) => Right(arr)
          case _            => Left(s"Not an array")
        }
    }
    private lazy val arre = JsonEncoder.chunk[Json]
    implicit val encoder: JsonEncoder[Arr] = new JsonEncoder[Arr] {
      def unsafeEncode(a: Arr, indent: Option[Int], out: Write): Unit =
        arre.unsafeEncode(a.elements, indent, out)

      override final def toJsonAST(a: Arr): Either[String, Json] = Right(a)
    }
  }
  final case class Bool(value: Boolean) extends Json
  object Bool {
    implicit val decoder: JsonDecoder[Bool] = new JsonDecoder[Bool] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Bool =
        Bool(JsonDecoder.boolean.unsafeDecode(trace, in))

      override final def fromJsonAST(json: Json): Either[String, Bool] =
        json match {
          case b @ Bool(_) => Right(b)
          case _           => Left(s"Not a bool value")
        }
    }
    implicit val encoder: JsonEncoder[Bool] = new JsonEncoder[Bool] {
      def unsafeEncode(a: Bool, indent: Option[Int], out: Write): Unit =
        JsonEncoder.boolean.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Bool): Either[String, Json] = Right(a)
    }
  }
  final case class Str(value: String) extends Json
  object Str {
    implicit val decoder: JsonDecoder[Str] = new JsonDecoder[Str] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Str =
        Str(JsonDecoder.string.unsafeDecode(trace, in))

      override final def fromJsonAST(json: Json): Either[String, Str] =
        json match {
          case s @ Str(_) => Right(s)
          case _          => Left(s"Not a string value")
        }
    }
    implicit val encoder: JsonEncoder[Str] = new JsonEncoder[Str] {
      def unsafeEncode(a: Str, indent: Option[Int], out: Write): Unit =
        JsonEncoder.string.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Str): Either[String, Json] = Right(a)
    }
  }
  final case class Num(value: java.math.BigDecimal) extends Json
  object Num {
    def apply(value: Byte): Num       = Num(BigDecimal(value.toInt).bigDecimal)
    def apply(value: Short): Num      = Num(BigDecimal(value.toInt).bigDecimal)
    def apply(value: Int): Num        = Num(BigDecimal(value).bigDecimal)
    def apply(value: Long): Num       = Num(BigDecimal(value).bigDecimal)
    def apply(value: BigDecimal): Num = Num(value.bigDecimal)
    def apply(value: Float): Num      = Num(BigDecimal(value.toDouble).bigDecimal)
    def apply(value: Double): Num     = Num(BigDecimal(value).bigDecimal)

    implicit val decoder: JsonDecoder[Num] = new JsonDecoder[Num] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Num =
        Num(JsonDecoder.bigDecimal.unsafeDecode(trace, in))

      override final def fromJsonAST(json: Json): Either[String, Num] =
        json match {
          case n @ Num(_) => Right(n)
          case _          => Left(s"Not a number")
        }
    }
    implicit val encoder: JsonEncoder[Num] = new JsonEncoder[Num] {
      def unsafeEncode(a: Num, indent: Option[Int], out: Write): Unit =
        JsonEncoder.bigDecimal.unsafeEncode(a.value, indent, out)

      override final def toJsonAST(a: Num): Either[String, Num] = Right(a)
    }
  }
  type Null = Null.type
  case object Null extends Json {
    private[this] val nullChars: Array[Char] = "null".toCharArray
    implicit val decoder: JsonDecoder[Null.type] = new JsonDecoder[Null.type] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): Null.type = {
        Lexer.readChars(trace, in, nullChars, "null")
        Null
      }

      override final def fromJsonAST(json: Json): Either[String, Null.type] =
        json match {
          case Null => Right(Null)
          case _    => Left(s"Not null")
        }
    }
    implicit val encoder: JsonEncoder[Null.type] = new JsonEncoder[Null.type] {
      def unsafeEncode(a: Null.type, indent: Option[Int], out: Write): Unit =
        out.write("null")

      override final def toJsonAST(a: Null.type): Either[String, Json] = Right(a)
    }
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
          throw UnsafeJson(JsonError.Message(s"unexpected '$c'") :: trace)
      }
    }

    override final def fromJsonAST(json: Json): Either[String, Json] =
      Right(json)
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
}
