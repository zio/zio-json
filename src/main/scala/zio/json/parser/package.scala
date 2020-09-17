package zio.json

// convenience to match the circe api
package object parser {

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
  def decode[A](str: CharSequence)(implicit D: Decoder[A]): Either[String, A] =
    D.decodeJson(str)
}
