package zio.json

import scala.collection.mutable.{ListBuffer}

import Decoder.{JsonError, UnsafeJson}
import zio.json.internal._
import scala.annotation._

sealed abstract class JsValue {
  def widen: JsValue = this
}

// TODO use Encoder in toString, this is very slow
final case class JsObject(fields: List[(String, JsValue)]) extends JsValue {
  override def toString =
    fields.map { case (f, v) => s""""${f}":${v}""" }.mkString("{", ",", "}")
}
final case class JsArray(elements: List[JsValue]) extends JsValue {
  override def toString = elements.mkString("[", ",", "]")
}
final case class JsBoolean(value: Boolean) extends JsValue {
  override def toString = value.toString
}
final case class JsString(value: String) extends JsValue {
  override def toString = {
    val len = value.length
    val sb = new StringBuilder(len)
    sb.append('"')
    var i = 0
    while (i < len) {
      (value.charAt(i): @switch) match {
        case '"'  => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ') sb.append("\\u%04x".format(c.toInt))
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')
    sb.toString
  }

}
final case class JsNumber(value: BigDecimal) extends JsValue {
  override def toString = value.toString
}
final case object JsNull extends JsValue {
  override def toString = "null"

  private[this] val nullChars: Array[Char] = "null".toCharArray
  implicit val decoder: Decoder[JsNull.type] = new Decoder[JsNull.type] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsNull.type = {
      Lexer.readChars(trace, in, nullChars, "null")
      JsNull
    }
  }
}

object JsValue {
  implicit val decoder: Decoder[JsValue] = new Decoder[JsValue] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsValue = {
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
          throw UnsafeJson(JsonError.Message(s"unexpected '$c'") :: trace)
      }
    }
  }
}
object JsObject {
  private lazy val objd = Decoder.keylist[String, JsValue]
  implicit val decoder: Decoder[JsObject] = new Decoder[JsObject] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsObject =
      JsObject(objd.unsafeDecode(trace, in))
  }
}
object JsArray {
  private lazy val arrd = Decoder.list[JsValue]
  implicit val decoder: Decoder[JsArray] = new Decoder[JsArray] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsArray =
      JsArray(arrd.unsafeDecode(trace, in))
  }
}
object JsBoolean {
  implicit val decoder: Decoder[JsBoolean] = new Decoder[JsBoolean] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsBoolean =
      JsBoolean(Lexer.boolean(trace, in))
  }
}
object JsString {
  implicit val decoder: Decoder[JsString] = new Decoder[JsString] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsString =
      JsString(Lexer.string(trace, in).toString)
  }
}
object JsNumber {
  implicit val decoder: Decoder[JsNumber] = new Decoder[JsNumber] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): JsNumber =
      JsNumber(Lexer.bigdecimal(trace, in))
  }
}
