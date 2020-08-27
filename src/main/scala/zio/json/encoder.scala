package zio.json

trait Encoder[A] {
  def toJson(a: A): String

  private[zio] def unsafeToJson(a: A, out: java.io.Writer): Unit
  private[zio] def unsafeToJsonField(a: A, field: String, out: java.io.Writer): Unit // = ... // default impl
}
