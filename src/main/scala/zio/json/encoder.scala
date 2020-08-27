package zio.json

trait Encoder[A] {
  def toJson(a: A): String

  private[json] def unsafeToJson(a: A, out: java.io.Writer): Unit
  private[json] def unsafeToJsonField(a: A, field: String, out: java.io.Writer): Unit // = ... // default impl
}
