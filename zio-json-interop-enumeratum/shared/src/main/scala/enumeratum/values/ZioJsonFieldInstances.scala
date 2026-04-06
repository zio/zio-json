package enumeratum.values

import zio.json.{ JsonError, JsonFieldDecoder, JsonFieldEncoder }
import zio.json.internal.Lexer

private[enumeratum] object ZioJsonFieldInstances {

  implicit val shortFieldEncoder: JsonFieldEncoder[Short] = new JsonFieldEncoder[Short] {
    def unsafeEncodeField(in: Short): String = in.toString
  }

  implicit val shortFieldDecoder: JsonFieldDecoder[Short] = new JsonFieldDecoder[Short] {
    def unsafeDecodeField(trace: List[JsonError], in: String): Short =
      try in.toShort
      catch {
        case _: NumberFormatException => Lexer.error(s"Invalid Short: $in", trace)
      }
  }

  implicit val byteFieldEncoder: JsonFieldEncoder[Byte] = new JsonFieldEncoder[Byte] {
    def unsafeEncodeField(in: Byte): String = in.toString
  }

  implicit val byteFieldDecoder: JsonFieldDecoder[Byte] = new JsonFieldDecoder[Byte] {
    def unsafeDecodeField(trace: List[JsonError], in: String): Byte =
      try in.toByte
      catch {
        case _: NumberFormatException => Lexer.error(s"Invalid Byte: $in", trace)
      }
  }

  implicit val charFieldEncoder: JsonFieldEncoder[Char] = new JsonFieldEncoder[Char] {
    def unsafeEncodeField(in: Char): String = in.toString
  }

  implicit val charFieldDecoder: JsonFieldDecoder[Char] = new JsonFieldDecoder[Char] {
    def unsafeDecodeField(trace: List[JsonError], in: String): Char =
      if (in.length == 1) in.charAt(0)
      else Lexer.error(s"Invalid Char: $in", trace)
  }

}
