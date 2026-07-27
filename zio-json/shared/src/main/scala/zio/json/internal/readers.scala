/*
 * Copyright 2019-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.json.internal

// Implementations of java.io.Reader such as alternatives to StringReader,
// BufferedReader and PushbackReader that are faster (2x) because they do not
// synchronise on a lock, and do not require up-front decisions about buffer
// sizes.

import zio.Chunk

import java.util.Arrays
import scala.annotation._
import scala.util.control.NoStackTrace

// https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/io/PushbackReader.java

private[zio] trait OneCharReader extends java.io.Reader {
  def read(cbuf: Array[Char], off: Int, len: Int): Int =
    throw new UnsupportedOperationException

  override def read(): Int =
    try readChar().toInt
    catch { case _: UnexpectedEnd => -1 }

  // for cases where EOB is not expected, throwing (stackless) UnexpectedEnd.
  def readChar(): Char
  // {
  //   val v = read()
  //   if (v == -1) throw new UnexpectedEnd
  //   v.toChar
  // }

  def nextNonWhitespace(): Char = {
    var c: Char = 0
    while ({ c = readChar(); isWhitespace(c) }) ()
    c
  }

  // profiled to be faster than Character.isWhitespace
  // also this is defined in the json spec and may differ from Java
  @inline protected def isWhitespace(c: Char): Boolean =
    (c: @switch) match {
      case ' '  => true
      case '\r' => true
      case '\n' => true
      case '\t' => true
      case _    => false
    }

}

private[zio] final class UnexpectedEnd
    extends Exception(
      "if you see this a dev made a mistake using OneCharReader"
    )
    with NoStackTrace

private[zio] final class RewindTwice
    extends Exception(
      "RecordingReader's rewind was called twice"
    )
    with NoStackTrace

/**
 * A Reader that can retract and replay the last char that it read.
 *
 * This is essential when parsing contents that do not have a terminator character, e.g. numbers, whilst preserving the
 * non-significant character for further processing.
 */
sealed trait RetractReader extends OneCharReader {

  /** Behaviour is undefined if called more than once without a read() */
  def retract(): Unit
}

final class FastCharSequence(s: Array[Char]) extends CharSequence {
  def length: Int                                     = s.length
  def charAt(i: Int): Char                            = s(i)
  def subSequence(start: Int, end: Int): CharSequence =
    new FastCharSequence(Arrays.copyOfRange(s, start, end))
}

// java.io.StringReader uses a lock, which reduces perf by x2, this also allows
// fast retraction and access to raw char arrays (which are faster than Strings)
private[zio] final class FastStringReader(s: CharSequence) extends RetractReader with PlaybackReader {
  private[this] var i: Int   = 0
  private[this] val len: Int = s.length

  def offset(): Int = i

  def close(): Unit = ()

  override def read(): Int = {
    val i = this.i
    if (i < len) {
      this.i = i + 1
      return s.charAt(i).toInt
    }
    -1
  }

  override def readChar(): Char = {
    val i = this.i
    if (i < len) {
      this.i = i + 1
      return s.charAt(i)
    }
    throw new UnexpectedEnd
  }

  override def nextNonWhitespace(): Char = {
    var i = this.i
    while (i < len) {
      val c = s.charAt(i)
      i += 1
      if (c != ' ' && c != '\n' && (c | 0x4) != '\r') {
        this.i = i
        return c
      }
    }
    this.i = i
    throw new UnexpectedEnd
  }

  def retract(): Unit = i -= 1

  def history(idx: Int): Char = s.charAt(idx)
}

/**
 * A Reader over a `Chunk[Byte]` holding UTF-8 encoded text, decoding to chars on the fly.
 *
 * This exists so that bytes coming off the wire can be parsed without first being copied into a `String`: peak memory
 * is the chunk plus a constant, instead of the chunk plus a full sized copy of it.
 *
 * Malformed input produces `�`, one per malformed sequence, as `new String(bytes, UTF_8)` does. The two agree on well
 * formed input and on the usual damage (truncated tails, stray continuation bytes, overlong forms), but not byte for
 * byte on every malformed sequence: `CharsetDecoder`'s maximal subpart rule emits three replacements for a CESU-8
 * encoded surrogate such as `ED A0 80` where this emits one.
 */
private[zio] final class Utf8ChunkReader(chunk: Chunk[Byte]) extends RetractReader {
  private[this] val len: Int     = chunk.length
  private[this] var i: Int       = 0
  private[this] var pending: Int = -1 // low surrogate owed to the next read

  // position to rewind to on retract(). Only moved when a char is actually produced, so that retracting after the end
  // of input replays the last real char, as FastStringReader's `i -= 1` does.
  private[this] var markI: Int       = 0
  private[this] var markPending: Int = -1

  // Bytes are pulled through a window rather than read one at a time off the Chunk: Chunk#byte is a virtual call
  // taking an implicit witness, and on a Chunk.Concat it walks the tree on every byte, which costs several times more
  // than the parse itself. Bulk copying keeps that to one arraycopy per window and makes reads plain array loads.
  private[this] val buf: Array[Byte] = new Array(math.min(len, Utf8ChunkReader.WindowSize))
  private[this] var base: Int        = 0 // index in the chunk of buf(0)
  private[this] var limit: Int       = 0 // number of valid bytes in buf

  def close(): Unit = ()

  override def read(): Int = readNext()

  def readChar(): Char = {
    val c = readNext()
    if (c == -1) throw new UnexpectedEnd
    c.toChar
  }

  override def nextNonWhitespace(): Char = {
    // whitespace is always single byte, so it can be skipped without decoding. Not safe while a low surrogate is
    // owed, since that has to come out before anything after it is consumed; the loop below handles that case.
    if (pending < 0) {
      val from = this.i
      var i    = from
      while (i < len && isAsciiWhitespace(byteAt(i)))
        i += 1
      if (i != from) {
        markI = i - 1
        markPending = -1
      }
      this.i = i
    }
    var c = readChar()
    while (isWhitespace(c))
      c = readChar()
    c
  }

  def retract(): Unit = {
    i = markI
    pending = markPending
  }

  // callers must have checked k < len. k may be behind the window after a retract, hence the lower bound check
  @inline private[this] def byteAt(k: Int): Byte = {
    val j = k - base
    if (j >= 0 && j < limit) buf(j)
    else fill(k)
  }

  private[this] def fill(k: Int): Byte = {
    val n = math.min(buf.length, len - k)
    chunk.toArray(k, buf, 0, n)
    base = k
    limit = n
    buf(0)
  }

  @inline private[this] def isAsciiWhitespace(b: Byte): Boolean =
    b == ' ' || b == '\n' || b == '\r' || b == '\t'

  private[this] def readNext(): Int = {
    val pending = this.pending
    val i       = this.i
    if (pending >= 0) {
      markI = i
      markPending = pending
      this.pending = -1
      return pending
    }
    if (i >= len) return -1 // mark deliberately left alone, see above
    markI = i
    markPending = -1
    val b0 = byteAt(i)
    this.i = i + 1
    if (b0 >= 0) b0.toInt // 0xxxxxxx, the overwhelmingly common case for JSON
    else decodeMultiByte(b0 & 0xff)
  }

  @noinline private[this] def decodeMultiByte(b0: Int): Int =
    if (b0 < 0xc2) Utf8ChunkReader.Replacement // stray continuation byte, or an overlong 0xc0/0xc1 lead
    else if (b0 < 0xe0) {
      val b1 = continuation()
      if (b1 < 0) Utf8ChunkReader.Replacement
      else ((b0 & 0x1f) << 6) | b1
    } else if (b0 < 0xf0) {
      val b1 = continuation()
      if (b1 < 0) return Utf8ChunkReader.Replacement
      val b2 = continuation()
      if (b2 < 0) return Utf8ChunkReader.Replacement
      val cp = ((b0 & 0x0f) << 12) | (b1 << 6) | b2
      if (cp < 0x800 || (cp >= 0xd800 && cp <= 0xdfff)) Utf8ChunkReader.Replacement // overlong, or a lone surrogate
      else cp
    } else if (b0 < 0xf5) {
      val b1 = continuation()
      if (b1 < 0) return Utf8ChunkReader.Replacement
      val b2 = continuation()
      if (b2 < 0) return Utf8ChunkReader.Replacement
      val b3 = continuation()
      if (b3 < 0) return Utf8ChunkReader.Replacement
      val cp = ((b0 & 0x07) << 18) | (b1 << 12) | (b2 << 6) | b3
      if (cp < 0x10000 || cp > 0x10ffff) Utf8ChunkReader.Replacement // overlong, or beyond the Unicode range
      else {
        val u = cp - 0x10000
        pending = 0xdc00 | (u & 0x3ff)
        0xd800 | (u >> 10)
      }
    } else Utf8ChunkReader.Replacement // would encode beyond U+10FFFF

  // the offending byte is deliberately left unconsumed so that it is re-examined as a fresh lead byte
  private[this] def continuation(): Int = {
    val i = this.i
    if (i >= len) return -1
    val b = byteAt(i) & 0xff
    if ((b & 0xc0) != 0x80) return -1
    this.i = i + 1
    b & 0x3f
  }
}

private[zio] object Utf8ChunkReader {
  private final val Replacement = 0xfffd

  // large enough that the copies amortise away, small enough to stay irrelevant next to the chunk itself
  private final val WindowSize = 8192
}

// this tends to be a bit slower than creating an implementation that implements
// all Reader interfaces that are required.
final class WithRetractReader(in: java.io.Reader) extends RetractReader with AutoCloseable {
  private[this] var last   = -2
  private[this] var replay = false

  def close(): Unit = in.close()

  override def read(): Int = {
    if (replay)
      replay = false
    else
      last = in.read()
    last
  }

  def readChar(): Char = {
    val v = read()
    if (v == -1) throw new UnexpectedEnd
    v.toChar
  }

  def retract(): Unit = replay = true
}

/**
 * Records the contents of an underlying Reader and allows rewinding back to the beginning once. If rewound and reading
 * continues past the recording, the recording no longer continues.
 *
 * To avoid feature interaction edge cases, `retract` is not allowed as the first action nor is `retract` allowed to
 * happen immediately before or after a `rewind`.
 */
private[zio] sealed trait RecordingReader extends RetractReader {
  def rewind(): Unit
}
private[zio] object RecordingReader {
  @inline def apply(in: OneCharReader): RecordingReader = new WithRecordingReader(in, 64)
}

// used to optimise RecordingReader
private[zio] sealed trait PlaybackReader extends OneCharReader {
  def offset(): Int

  // i must be < offset
  def history(i: Int): Char
}

/*
 * A reader that can copy another one and rewing when needed.
 * `initial` is the initial size of the buffer used for Reader copy.
 * It must be >= 1 and if not, it will be set to 1.
 */
private[zio] final class WithRecordingReader(in: OneCharReader, initial: Int)
    extends RecordingReader
    with PlaybackReader {
  private[this] var state: Int        = 0 // -1: neither recording nor replaying, 0: recording, 1: replaying
  private[this] var tape: Array[Char] = new Array(Math.max(initial, 1))
  private[this] var reading: Int      = 0
  private[this] var writing: Int      = 0

  def close(): Unit = in.close()

  override def read(): Int =
    if (state < 0) in.read()
    else if (state > 0) {
      var reading = this.reading
      val c       = tape(reading).toInt
      reading += 1
      this.reading = reading
      if (reading == writing) state = -1 // chatch up, stop replaying
      c
    } else {
      val writing = this.writing
      if (writing == tape.length) tape = Arrays.copyOf(tape, writing << 1)
      val c = in.read()
      if (c >= 0) {
        tape(writing) = c.toChar
        this.writing = writing + 1
      }
      c
    }

  override def readChar(): Char =
    if (state < 0) in.readChar()
    else if (state > 0) {
      var reading = this.reading
      val c       = tape(reading)
      reading += 1
      this.reading = reading
      if (reading == writing) state = -1 // chatch up, stop replaying
      c
    } else {
      val writing = this.writing
      if (writing == tape.length) tape = Arrays.copyOf(tape, writing << 1)
      val c = in.readChar()
      tape(writing) = c
      this.writing = writing + 1
      c
    }

  def rewind(): Unit =
    if (state == 0) state = 1 // start replaying
    else throw new RewindTwice

  def retract(): Unit =
    if (state > 0) reading -= 1
    else {
      in match {
        case rr: RetractReader =>
          rr.retract()
          if (state == 0) writing -= 1 // factor in retracted delegate
        case _ =>
          throw new UnsupportedOperationException("underlying reader does not support retract")
      }
    }

  def offset(): Int =
    if (state > 0) reading
    else writing

  def history(idx: Int): Char = tape(idx)
}
