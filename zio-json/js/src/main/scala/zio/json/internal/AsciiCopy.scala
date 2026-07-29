package zio.json.internal

private[zio] object AsciiCopy {

  /**
   * Copies `s.charAt(from until until)`, which the caller must have checked is all ASCII, into `dst` as UTF-8.
   *
   * Scala.js has no way to bulk-copy a `String` into an existing byte array, so this is the plain loop.
   */
  @inline def copy(s: String, from: Int, until: Int, dst: Array[Byte], dstFrom: Int): Unit = {
    var i = from
    var n = dstFrom
    while (i < until) {
      dst(n) = s.charAt(i).toByte
      i += 1
      n += 1
    }
  }
}
