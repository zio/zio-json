package zio

package object json extends JsonPackagePlatformSpecific {
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {
    def toJson(implicit A: JsonEncoder[A]): String = A.encodeJson(a, None).toString

    // Jon Pretty's better looking brother, but a bit slower
    def toJsonPretty(implicit A: JsonEncoder[A]): String = A.encodeJson(a, Some(0)).toString
  }

  implicit final class DecoderOps(private val json: CharSequence) extends AnyVal {

    /**
     * Attempts to decode the raw JSON string as an `A`.
     *
     * On failure a human readable message is returned using a jq friendly
     * format. For example the error
     * `.rows[0].elements[0].distance.value(missing)"` tells us the location of a
     * missing field named "value". We can use part of the error message in the
     * `jq` command line tool for further inspection, e.g.
     *
     * {{{jq '.rows[0].elements[0].distance' input.json}}}
     */
    def fromJson[A](implicit A: JsonDecoder[A]): Either[String, A] = A.decodeJson(json)
  }
}
