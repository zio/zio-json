// Implementations of java.io.Writer that are faster (2x) because they do not
// synchronise on a lock
package zio.json.internal

import java.nio.CharBuffer
import java.util.Arrays

// like StringBuilder but doesn't have any encoding or range checks
final class FastStringWriter(initial: Int) extends java.io.Writer {
  private[this] var chars: Array[Char] = Array.ofDim(initial)
  private[this] var i: Int             = 0

  def close(): Unit = {}
  def flush(): Unit = {}

  def write(cs: Array[Char], off: Int, len: Int): Unit = {
    if (i + len > chars.length) {
      val len_ = (chars.length * 2) max (chars.length + len)
      chars = Arrays.copyOf(chars, len_)
    }
    System.arraycopy(cs, off, chars, i, len)
    i += len
  }

  override def append(c: Char): java.io.Writer = {
    if (i == chars.length)
      chars = Arrays.copyOf(chars, chars.length * 2)
    chars(i) = c
    i += 1
    this
  }

  override def write(s: String, off: Int, len: Int): Unit = {
    if (i + len > chars.length) {
      val len_ = (chars.length * 2) max (chars.length + len)
      chars = Arrays.copyOf(chars, len_)
    }
    s.getChars(off, off + len, chars, i)
    i += len
  }

  def buffer: CharBuffer = CharBuffer.wrap(chars, 0, i)

  override def toString: String = new java.lang.String(chars, 0, i)
}
