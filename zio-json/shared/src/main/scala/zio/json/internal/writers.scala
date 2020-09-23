// Implementations of java.io.Writer that are faster (2x) because they do not
// synchronize on a lock
package zio.json.internal

import scala.annotation.tailrec

import java.nio.CharBuffer
import java.util.Arrays

// a minimal subset of java.io.Writer that can be optimised
trait Write {
  def write(c: Char): Unit
  def write(s: String): Unit
}

// wrapper to implement the legacy Java API
final class WriteWriter(out: java.io.Writer) extends Write {
  def write(s: String): Unit = out.write(s)
  def write(c: Char): Unit   = out.write(c.toInt)
}

// optimised in-memory impl of Write that minimises copies
final class FastStringWrite(initial: Int) extends Write {
  // the idea is to keep all the strings in memory (reverse order for fast
  // append) and to also support chars. Chars are held in an Array and converted
  // into a String if a non-char is written.
  private[this] var strings: List[String] = Nil
  private[this] var chars: Array[Char]    = Array.ofDim(initial)
  private[this] var i: Int                = 0

  private def reconcile(): Unit =
    if (i > 0) {
      strings = new String(chars, 0, i) :: strings
      i = 0
    }

  def write(s: String): Unit = {
    reconcile()
    strings = s :: strings
  }
  def write(c: Char): Unit = {
    if (i == chars.length)
      chars = Arrays.copyOf(chars, chars.length * 2)
    chars(i) = c
    i += 1
  }

  override def toString: String = {
    reconcile()

    @tailrec def len(ss: List[String], acc: Int): Int = ss match {
      case Nil          => acc
      case head :: tail => len(tail, head.length + acc)
    }
    val output: Array[Char] = Array.ofDim(len(strings, 0))
    var i                   = 0
    strings.reverse.foreach { s =>
      s.getChars(0, s.length, output, i)
      i += s.length
    }
    val combined = new String(output) // unavoidable copy...
    strings = List(combined) // allow early GC
    combined
  }
}

// like StringBuilder but doesn't have any encoding or range checks
private[zio] final class FastStringBuilder(initial: Int) {
  private[this] var chars: Array[Char] = Array.ofDim(initial)
  private[this] var i: Int             = 0

  def append(c: Char): Unit = {
    if (i == chars.length)
      chars = Arrays.copyOf(chars, chars.length * 2)
    chars(i) = c
    i += 1
  }

  def buffer: CharBuffer = CharBuffer.wrap(chars, 0, i)
}
