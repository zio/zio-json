package zio.json.internal

import java.util.Arrays

/**
 * Transcodes the contents of a [[FastStringWrite]] to UTF-8 bytes in a single pass, without building a `String`.
 *
 * The transcode implements exactly what `String.getBytes(UTF_8)` does — including replacing a lone (unpaired) surrogate
 * with a single `'?'`, not the `U+FFFD` used on the decode direction. A surrogate pair split across two `write` calls
 * (the escaped-string path writes char by char) simply lands as two adjacent chars in the buffer, so no cross-call
 * state is needed.
 *
 * The scratch is sized to the worst case up front (3 bytes per char, the same bound `String.getBytes` allocates for) so
 * the loop carries no capacity checks, and is pooled per thread behind a weak reference; the only per-call allocation
 * is the exact-size result. A single slot suffices even for recursive encodes: the transcode runs after its encode has
 * completed and never calls back into user code, so uses of the scratch are strictly sequential.
 */
private[zio] object Utf8Bytes {

  private[this] val scratchRef = new ThreadLocal[java.lang.ref.WeakReference[Array[Byte]]] {
    override def initialValue(): java.lang.ref.WeakReference[Array[Byte]] =
      new java.lang.ref.WeakReference(new Array[Byte](256))
  }

  def fromWrite(write: FastStringWrite): Array[Byte] = transcode(write.unsafeChars, write.unsafeLength)

  private[this] def scratch(n: Int): Array[Byte] = {
    var bs = scratchRef.get.get
    if ((bs eq null) || bs.length < n) {
      var len = 256
      while (len < n && len < 0x40000000) len <<= 1
      if (len < n) len = n // near the top of the Int range doubling would overflow, and over-sizing is pointless
      bs = new Array[Byte](len)
      scratchRef.set(new java.lang.ref.WeakReference(bs))
    }
    bs
  }

  private[this] def transcode(cs: Array[Char], len: Int): Array[Byte] = {
    val bs = scratch(len * 3 + 1)
    var n  = 0
    var i  = 0
    while (i < len) {
      val c = cs(i)
      if (c < 0x80) {
        bs(n) = c.toByte
        n += 1
        i += 1
      } else if (c < 0x800) {
        bs(n) = (0xc0 | (c >> 6)).toByte
        bs(n + 1) = (0x80 | (c & 0x3f)).toByte
        n += 2
        i += 1
      } else if (c < 0xd800 || c > 0xdfff) {
        bs(n) = (0xe0 | (c >> 12)).toByte
        bs(n + 1) = (0x80 | ((c >> 6) & 0x3f)).toByte
        bs(n + 2) = (0x80 | (c & 0x3f)).toByte
        n += 3
        i += 1
      } else if (c < 0xdc00 && i + 1 < len && cs(i + 1) >= 0xdc00 && cs(i + 1) <= 0xdfff) {
        val cp = 0x10000 + ((c - 0xd800) << 10) + (cs(i + 1) - 0xdc00)
        bs(n) = (0xf0 | (cp >> 18)).toByte
        bs(n + 1) = (0x80 | ((cp >> 12) & 0x3f)).toByte
        bs(n + 2) = (0x80 | ((cp >> 6) & 0x3f)).toByte
        bs(n + 3) = (0x80 | (cp & 0x3f)).toByte
        n += 4
        i += 2
      } else { // lone surrogate, matching String.getBytes(UTF_8)
        bs(n) = '?'
        n += 1
        i += 1
      }
    }
    Arrays.copyOf(bs, n)
  }
}
