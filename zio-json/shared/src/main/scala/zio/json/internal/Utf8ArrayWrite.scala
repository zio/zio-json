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
 *
 * The hot paths (ASCII, which is most of any JSON document: structure, field names, numbers, unescaped string content)
 * check `pending` once, hoist the capacity check out of the per-char work and write straight into the array; everything
 * else falls back to the stateful char-at-a-time path.
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

  def write(c: Char): Unit =
    if (pending < 0 && c < 0x80) {
      val bs = ensure(1)
      bs(count) = c.toByte
      count += 1
    } else writeSlow(c)

  // multi-byte, surrogate, or resolving a pending surrogate: the general, stateful path
  private[this] def writeSlow(c: Char): Unit = {
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
    var i   = 0
    // a pending surrogate from a previous write must resolve through the stateful path before the fast loop can run
    while (pending >= 0 && i < len) {
      writeSlow(s.charAt(i))
      i += 1
    }
    while (i < len) {
      // find the run of ASCII chars starting at i, then copy it in bulk (on the JVM, a single System.arraycopy
      // from the string's backing array -- the bulk intrinsic a char-by-char loop cannot get)
      var j = i
      while (j < len && s.charAt(j) < 0x80) j += 1
      if (j > i) {
        val n  = j - i
        val bs = ensure(n)
        AsciiCopy.copy(s, i, j, bs, count)
        count += n
        i = j
      }
      if (i < len) {
        writeSlow(s.charAt(i)) // may leave a pending high surrogate, which the loop below must resolve
        i += 1
        while (pending >= 0 && i < len) {
          writeSlow(s.charAt(i))
          i += 1
        }
      }
    }
  }

  override def write(c1: Char, c2: Char): Unit =
    if (pending < 0 && (c1 | c2) < 0x80) {
      val bs = ensure(2)
      val i  = count
      bs(i) = c1.toByte
      bs(i + 1) = c2.toByte
      count = i + 2
    } else {
      write(c1)
      write(c2)
    }

  override def write(c1: Char, c2: Char, c3: Char): Unit =
    if (pending < 0 && (c1 | c2 | c3) < 0x80) {
      val bs = ensure(3)
      val i  = count
      bs(i) = c1.toByte
      bs(i + 1) = c2.toByte
      bs(i + 2) = c3.toByte
      count = i + 3
    } else {
      write(c1)
      write(c2)
      write(c3)
    }

  override def write(c1: Char, c2: Char, c3: Char, c4: Char): Unit =
    if (pending < 0 && (c1 | c2 | c3 | c4) < 0x80) {
      val bs = ensure(4)
      val i  = count
      bs(i) = c1.toByte
      bs(i + 1) = c2.toByte
      bs(i + 2) = c3.toByte
      bs(i + 3) = c4.toByte
      count = i + 4
    } else {
      write(c1)
      write(c2)
      write(c3)
      write(c4)
    }

  override def write(c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): Unit =
    if (pending < 0 && (c1 | c2 | c3 | c4 | c5) < 0x80) {
      val bs = ensure(5)
      val i  = count
      bs(i) = c1.toByte
      bs(i + 1) = c2.toByte
      bs(i + 2) = c3.toByte
      bs(i + 3) = c4.toByte
      bs(i + 4) = c5.toByte
      count = i + 5
    } else {
      write(c1)
      write(c2)
      write(c3)
      write(c4)
      write(c5)
    }

  // (s & 0x8080) == 0 <=> both packed chars, (s & 0xff) and (s >> 8), are ASCII: bit 7 is the low char's non-ASCII
  // bit, and bit 15 is the sign, so its absence means s >= 0, which caps the high char at 0x7f (a negative short
  // would sign-extend, putting the high char at 0xff00 or above)
  override def write(s: Short): Unit =
    if (pending < 0 && (s & 0x8080) == 0) {
      val bs = ensure(2)
      val i  = count
      bs(i) = (s & 0xff).toByte
      bs(i + 1) = (s >> 8).toByte
      count = i + 2
    } else {
      write((s & 0xff).toChar)
      write((s >> 8).toChar)
    }

  override def write(s1: Short, s2: Short): Unit =
    if (pending < 0 && ((s1 | s2) & 0x8080) == 0) {
      val bs = ensure(4)
      val i  = count
      bs(i) = (s1 & 0xff).toByte
      bs(i + 1) = (s1 >> 8).toByte
      bs(i + 2) = (s2 & 0xff).toByte
      bs(i + 3) = (s2 >> 8).toByte
      count = i + 4
    } else {
      write(s1)
      write(s2)
    }

  override def write(s1: Short, s2: Short, s3: Short): Unit =
    if (pending < 0 && ((s1 | s2 | s3) & 0x8080) == 0) {
      val bs = ensure(6)
      val i  = count
      bs(i) = (s1 & 0xff).toByte
      bs(i + 1) = (s1 >> 8).toByte
      bs(i + 2) = (s2 & 0xff).toByte
      bs(i + 3) = (s2 >> 8).toByte
      bs(i + 4) = (s3 & 0xff).toByte
      bs(i + 5) = (s3 >> 8).toByte
      count = i + 6
    } else {
      write(s1)
      write(s2)
      write(s3)
    }

  override def write(s1: Short, s2: Short, s3: Short, s4: Short): Unit =
    if (pending < 0 && ((s1 | s2 | s3 | s4) & 0x8080) == 0) {
      val bs = ensure(8)
      val i  = count
      bs(i) = (s1 & 0xff).toByte
      bs(i + 1) = (s1 >> 8).toByte
      bs(i + 2) = (s2 & 0xff).toByte
      bs(i + 3) = (s2 >> 8).toByte
      bs(i + 4) = (s3 & 0xff).toByte
      bs(i + 5) = (s3 >> 8).toByte
      bs(i + 6) = (s4 & 0xff).toByte
      bs(i + 7) = (s4 >> 8).toByte
      count = i + 8
    } else {
      write(s1)
      write(s2)
      write(s3)
      write(s4)
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
