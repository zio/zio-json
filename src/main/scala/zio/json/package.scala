package zio

package object json {
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {
    def toJson(implicit A: Encoder[A]): String = A.toJson(a, None)

    // Jon Pretty's better looking brother, but a bit slower
    def toJsonPretty(implicit A: Encoder[A]): String = A.toJson(a, Some(0))
  }
}
