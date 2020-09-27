package zio.json

sealed trait JsonStreamDelimiter

object JsonStreamDelimiter {
  case object Newline extends JsonStreamDelimiter
  case object Array   extends JsonStreamDelimiter
}
