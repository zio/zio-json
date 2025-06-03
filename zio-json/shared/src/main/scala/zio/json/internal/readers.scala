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
