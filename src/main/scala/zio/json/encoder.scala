package zio.json

trait Encoder[A] {
  def toJson(a: A): String

  def unsafeToJson(a: A, out: java.io.Writer): Unit
  def unsafeToJsonField(a: A, field: String, out: java.io.Writer): Unit // = ... // default impl
}
