package zio.json.ast

import scala.annotation._

import zio.Chunk
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json._
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
sealed abstract class Json {
  def widen: Json = this

  override def toString(): String = Json.encoder.encodeJson(this, None).toString
}

object Json {
  // TODO lens-like accessors for working with arbitrary json values
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
