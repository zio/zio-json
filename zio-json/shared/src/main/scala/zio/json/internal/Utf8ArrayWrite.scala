package zio.json.internal

import java.util.Arrays

/**
 * A `Write` that encodes directly to UTF-8 bytes in a growable array, instead of building a `String` and re-encoding it
 * afterwards.
 *
 * A lone (unpaired) surrogate is replaced with a single `?`, matching `String.getBytes(UTF_8)` (which differs from the
 * `U+FFFD` used on the decode direction). Because a surrogate pair can arrive as two separate `write(Char)` calls, an
 * unresolved leading (high) surrogate is held in `pending` until the next char either completes the pair or proves it
 * was never going to be completed; [[finish]] must be called once encoding is done to flush a pair left incomplete at
 * the very end.
 */
private[zio] final class Utf8ArrayWrite(initial: Int) extends Write {
  require(initial >= 8)
  private[this] var bytes: Array[Byte] = new Array(initial)
  private[this] var count: Int         = 0
  private[this] var pending: Int       = -1 // -1: none, else the pending high surrogate

  @inline def reset(): Unit = {
    count = 0
    pending = -1
  }

  private[this] def ensure(n: Int): Array[Byte] = {
    var bs = bytes
    if (count + n > bs.length) {
      var len = bs.length << 1
      while (len < count + n) len <<= 1
      bs = Arrays.copyOf(bs, len)
      bytes = bs
    }
    bs
  }

  // cp is never a surrogate: either a BMP scalar value or a combined astral code point
  private[this] def putCodePoint(cp: Int): Unit =
    if (cp < 0x80) {
      val bs = ensure(1)
      bs(count) = cp.toByte
      count += 1
    } else if (cp < 0x800) {
      val bs = ensure(2)
      val i  = count
      bs(i) = (0xc0 | (cp >> 6)).toByte
      bs(i + 1) = (0x80 | (cp & 0x3f)).toByte
      count = i + 2
    } else if (cp < 0x10000) {
      val bs = ensure(3)
      val i  = count
      bs(i) = (0xe0 | (cp >> 12)).toByte
      bs(i + 1) = (0x80 | ((cp >> 6) & 0x3f)).toByte
      bs(i + 2) = (0x80 | (cp & 0x3f)).toByte
      count = i + 3
    } else {
      val bs = ensure(4)
      val i  = count
      bs(i) = (0xf0 | (cp >> 18)).toByte
      bs(i + 1) = (0x80 | ((cp >> 12) & 0x3f)).toByte
      bs(i + 2) = (0x80 | ((cp >> 6) & 0x3f)).toByte
      bs(i + 3) = (0x80 | (cp & 0x3f)).toByte
      count = i + 4
    }

  // matches String.getBytes(UTF_8): a single '?' per malformed unit, not the U+FFFD used on the decode direction
  @inline private[this] def putReplacement(): Unit = {
    val bs = ensure(1)
    bs(count) = '?'
    count += 1
  }

  @inline private[this] def isHighSurrogate(c: Char): Boolean = c >= 0xd800 && c <= 0xdbff
  @inline private[this] def isLowSurrogate(c: Char): Boolean  = c >= 0xdc00 && c <= 0xdfff

  def write(c: Char): Unit = {
    val p = pending
    if (p >= 0) {
      pending = -1
      if (isLowSurrogate(c)) {
        putCodePoint(0x10000 + ((p - 0xd800) << 10) + (c - 0xdc00))
        return
      }
      putReplacement() // p was never completed
    }
    if (isHighSurrogate(c)) pending = c
    else if (isLowSurrogate(c)) putReplacement() // lone low surrogate
    else putCodePoint(c.toInt)
  }

  def write(s: String): Unit = {
    val len = s.length
    if (len == 0) return
    ensure(len) // best case (pure ASCII, the common case for JSON) needs exactly this many bytes
    var i = 0
    if (pending >= 0) {
      write(s.charAt(0)) // resolves against, or replaces, the pending surrogate from a previous write
      i = 1
    }
    var bs = bytes
    while (i < len) {
      val c = s.charAt(i)
      // pending must also route through write(c): an ASCII char does not excuse resolving a surrogate left over
      // from the char before it
      if (c < 0x80 && pending < 0) {
        if (count == bs.length) bs = ensure(1)
        bs(count) = c.toByte
        count += 1
        i += 1
      } else {
        write(c) // multi-byte, surrogate, or resolving a pending surrogate: the general, stateful path
        bs = bytes
        i += 1
      }
    }
  }

  /** Call once encoding is complete, to replace a high surrogate left unpaired at the very end. */
  def finish(): Unit =
    if (pending >= 0) {
      putReplacement()
      pending = -1
    }

  /** The encoded bytes, trimmed to the data actually written. Not copied when the backing array is already exact. */
  def result(): Array[Byte] = {
    val bs = bytes
    if (count == bs.length) bs else Arrays.copyOf(bs, count)
  }
}
