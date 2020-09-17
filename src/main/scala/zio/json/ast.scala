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
 * Also beware of converting `JsNumber` (a `BigDecimal`) into any other kind of
 * number, since many of the stdlib functions are non-total or are known DOS
 * vectors (e.g. calling `.toBigInteger` on a "1e214748364" will consume an
 * excessive amount of heap memory).
 * JsonValue / JsValue / JValue
 */
sealed abstract class JsValue {
  def widen: JsValue    = this
  override def toString = JsValue.encoder.encodeJson(this, None)
}

// TODO lens-like accessors for working with arbitrary json values

final case class JsObject(fields: Chunk[(String, JsValue)]) extends JsValue
final case class JsArray(elements: Chunk[JsValue])          extends JsValue
final case class JsBoolean(value: Boolean)                  extends JsValue
final case class JsString(value: String)                    extends JsValue
final case class JsNumber(value: java.math.BigDecimal)      extends JsValue
final case object JsNull                                    extends JsValue with JsNullCompanion

object JsValue {
  implicit val decoder: Decoder[JsValue] = new Decoder[JsValue] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsValue = {
      val c = in.nextNonWhitespace()
      in.retract()
      (c: @switch) match {
        case 'n'       => JsNull.decoder.unsafeDecode(trace, in)
        case 'f' | 't' => JsBoolean.decoder.unsafeDecode(trace, in)
        case '{'       => JsObject.decoder.unsafeDecode(trace, in)
        case '['       => JsArray.decoder.unsafeDecode(trace, in)
        case '"'       => JsString.decoder.unsafeDecode(trace, in)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          JsNumber.decoder.unsafeDecode(trace, in)
        case c =>
          throw UnsafeJson(trace :+ JsonError.Message(s"unexpected '$c'"))
      }
    }
  }
  implicit val encoder: Encoder[JsValue] = new Encoder[JsValue] {
    def unsafeEncode(a: JsValue, indent: Option[Int], out: java.io.Writer): Unit =
      a match {
        case j: JsObject  => JsObject.encoder.unsafeEncode(j, indent, out)
        case j: JsArray   => JsArray.encoder.unsafeEncode(j, indent, out)
        case j: JsBoolean => JsBoolean.encoder.unsafeEncode(j, indent, out)
        case j: JsString  => JsString.encoder.unsafeEncode(j, indent, out)
        case j: JsNumber  => JsNumber.encoder.unsafeEncode(j, indent, out)
        case JsNull       => JsNull.encoder.unsafeEncode(JsNull, indent, out)
      }
  }
}
object JsObject {
  private lazy val objd = Decoder.keyValueChunk[String, JsValue]
  implicit val decoder: Decoder[JsObject] = new Decoder[JsObject] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsObject =
      JsObject(objd.unsafeDecode(trace, in))
  }
  private lazy val obje = Encoder.keyValueChunk[String, JsValue]
  implicit val encoder: Encoder[JsObject] = new Encoder[JsObject] {
    def unsafeEncode(a: JsObject, indent: Option[Int], out: java.io.Writer): Unit =
      obje.unsafeEncode(a.fields, indent, out)
  }
}
object JsArray {
  private lazy val arrd = Decoder.chunk[JsValue]
  implicit val decoder: Decoder[JsArray] = new Decoder[JsArray] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsArray =
      JsArray(arrd.unsafeDecode(trace, in))
  }
  private lazy val arre = Encoder.chunk[JsValue]
  implicit val encoder: Encoder[JsArray] = new Encoder[JsArray] {
    def unsafeEncode(a: JsArray, indent: Option[Int], out: java.io.Writer): Unit =
      arre.unsafeEncode(a.elements, indent, out)
  }
}
object JsBoolean {
  implicit val decoder: Decoder[JsBoolean] = new Decoder[JsBoolean] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsBoolean =
      JsBoolean(Decoder.boolean.unsafeDecode(trace, in))
  }
  implicit val encoder: Encoder[JsBoolean] = new Encoder[JsBoolean] {
    def unsafeEncode(a: JsBoolean, indent: Option[Int], out: java.io.Writer): Unit =
      Encoder.boolean.unsafeEncode(a.value, indent, out)
  }
}
object JsString {
  implicit val decoder: Decoder[JsString] = new Decoder[JsString] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsString =
      JsString(Decoder.string.unsafeDecode(trace, in))
  }
  implicit val encoder: Encoder[JsString] = new Encoder[JsString] {
    def unsafeEncode(a: JsString, indent: Option[Int], out: java.io.Writer): Unit =
      Encoder.string.unsafeEncode(a.value, indent, out)
  }
}
object JsNumber {
  implicit val decoder: Decoder[JsNumber] = new Decoder[JsNumber] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsNumber =
      JsNumber(Decoder.bigDecimal.unsafeDecode(trace, in))
  }
  implicit val encoder: Encoder[JsNumber] = new Encoder[JsNumber] {
    def unsafeEncode(a: JsNumber, indent: Option[Int], out: java.io.Writer): Unit =
      Encoder.bigDecimal.unsafeEncode(a.value, indent, out)
  }
}
trait JsNullCompanion {
  this: JsNull.type =>

  private[this] val nullChars: Array[Char] = "null".toCharArray
  implicit val decoder: Decoder[JsNull.type] = new Decoder[JsNull.type] {
    def unsafeDecode(trace: Chunk[JsonError], in: RetractReader): JsNull.type = {
      Lexer.readChars(trace, in, nullChars, "null")
      JsNull
    }
  }
  implicit val encoder: Encoder[JsNull.type] = new Encoder[JsNull.type] {
    def unsafeEncode(a: JsNull.type, indent: Option[Int], out: java.io.Writer): Unit =
      out.write("null")
  }
}
