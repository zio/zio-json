package zio.json.internal

private[zio] object AsciiCopy {

  /**
   * Copies `s.charAt(from until until)`, which the caller must have checked is all ASCII, into `dst` as UTF-8.
   *
   * An all-ASCII string is byte-identical in Latin-1 and UTF-8, so the deprecated truncating `getBytes` is exact here,
   * and on the JVM it is a single `System.arraycopy` from the string's backing array -- the bulk intrinsic a
   * char-by-char loop cannot get.
   */
  @annotation.nowarn("cat=deprecation")
  @inline def copy(s: String, from: Int, until: Int, dst: Array[Byte], dstFrom: Int): Unit =
    s.getBytes(from, until, dst, dstFrom)
}
