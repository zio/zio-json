package zio.json

import zio.Chunk

import Decoder.{ JsonError, UnsafeJson }
import zio.json.internal._
import scala.annotation._

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

  override def toString(): String = Json.encoder.encodeJson(this, None)
}

object Json {
  // TODO lens-like accessors for working with arbitrary json values
  final case class Obj(fields: Chunk[(String, Json)]) extends Json
  object Obj {
    private lazy val objd = Decoder.keyValueChunk[String, Json]
    implicit val decoder: Decoder[Obj] = new Decoder[Obj] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Obj =
        Obj(objd.unsafeDecode(trace, in))
    }
    private lazy val obje = Encoder.keyValueChunk[String, Json]
    implicit val encoder: Encoder[Obj] = new Encoder[Obj] {
      def unsafeEncode(a: Obj, indent: Option[Int], out: java.io.Writer): Unit =
        obje.unsafeEncode(a.fields, indent, out)
    }
  }
  final case class Arr(elements: Chunk[Json]) extends Json
  object Arr {
    private lazy val arrd = Decoder.chunk[Json]
    implicit val decoder: Decoder[Arr] = new Decoder[Arr] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Arr =
        Arr(arrd.unsafeDecode(trace, in))
    }
    private lazy val arre = Encoder.chunk[Json]
    implicit val encoder: Encoder[Arr] = new Encoder[Arr] {
      def unsafeEncode(a: Arr, indent: Option[Int], out: java.io.Writer): Unit =
        arre.unsafeEncode(a.elements, indent, out)
    }
  }
  final case class Bool(value: Boolean) extends Json
  object Bool {
    implicit val decoder: Decoder[Bool] = new Decoder[Bool] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Bool =
        Bool(Decoder.boolean.unsafeDecode(trace, in))
    }
    implicit val encoder: Encoder[Bool] = new Encoder[Bool] {
      def unsafeEncode(a: Bool, indent: Option[Int], out: java.io.Writer): Unit =
        Encoder.boolean.unsafeEncode(a.value, indent, out)
    }
  }
  final case class Str(value: String) extends Json
  object Str {
    implicit val decoder: Decoder[Str] = new Decoder[Str] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Str =
        Str(Decoder.string.unsafeDecode(trace, in))
    }
    implicit val encoder: Encoder[Str] = new Encoder[Str] {
      def unsafeEncode(a: Str, indent: Option[Int], out: java.io.Writer): Unit =
        Encoder.string.unsafeEncode(a.value, indent, out)
    }
  }
  final case class Num(value: java.math.BigDecimal) extends Json
  object Num {
    implicit val decoder: Decoder[Num] = new Decoder[Num] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Num =
        Num(Decoder.bigDecimal.unsafeDecode(trace, in))
    }
    implicit val encoder: Encoder[Num] = new Encoder[Num] {
      def unsafeEncode(a: Num, indent: Option[Int], out: java.io.Writer): Unit =
        Encoder.bigDecimal.unsafeEncode(a.value, indent, out)
    }
  }
  case object Null extends Json {
    private[this] val nullChars: Array[Char] = "null".toCharArray
    implicit val decoder: Decoder[Null.type] = new Decoder[Null.type] {
      def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Null.type = {
        Lexer.readChars(trace, in, nullChars, "null")
        Null
      }
    }
    implicit val encoder: Encoder[Null.type] = new Encoder[Null.type] {
      def unsafeEncode(a: Null.type, indent: Option[Int], out: java.io.Writer): Unit =
        out.write("null")
    }
  }

  implicit val decoder: Decoder[Json] = new Decoder[Json] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): Json = {
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
          throw UnsafeJson(trace :+ JsonError.Message(s"unexpected '$c'"))
      }
    }
  }
  implicit val encoder: Encoder[Json] = new Encoder[Json] {
    def unsafeEncode(a: Json, indent: Option[Int], out: java.io.Writer): Unit =
      a match {
        case j: Obj  => Obj.encoder.unsafeEncode(j, indent, out)
        case j: Arr  => Arr.encoder.unsafeEncode(j, indent, out)
        case j: Bool => Bool.encoder.unsafeEncode(j, indent, out)
        case j: Str  => Str.encoder.unsafeEncode(j, indent, out)
        case j: Num  => Num.encoder.unsafeEncode(j, indent, out)
        case Null    => Null.encoder.unsafeEncode(Null, indent, out)
      }
  }
}
