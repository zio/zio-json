package zio

package object json {
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {
    def toJson(implicit A: JsonEncoder[A]): String = A.encodeJson(a, None)

    // Jon Pretty's better looking brother, but a bit slower
    def toJsonPretty(implicit A: JsonEncoder[A]): String = A.encodeJson(a, Some(0))
  }
}
