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
  def length: Int          = s.length
  def charAt(i: Int): Char = s(i)
  def subSequence(start: Int, end: Int): CharSequence =
    new FastCharSequence(Arrays.copyOfRange(s, start, end))
}

// java.io.StringReader uses a lock, which reduces perf by x2, this also allows
// fast retraction and access to raw char arrays (which are faster than Strings)
private[zio] final class FastStringReader(s: CharSequence) extends RetractReader with PlaybackReader {
  private[this] var i: Int = 0
  def offset(): Int        = i
  private val len: Int     = s.length
  def close(): Unit        = ()
  override def read(): Int = {
    i += 1
    if (i > len) -1
    else s.charAt(i - 1).toInt // -1 is faster than assigning a temp value
  }
  override def readChar(): Char = {
    i += 1
    if (i > len) throw new UnexpectedEnd
    s.charAt(i - 1)
  }
  override def nextNonWhitespace(): Char = {
    while ({
      {
        i += 1
        if (i > len) throw new UnexpectedEnd
      }; isWhitespace(s.charAt(i - 1))
    }) ()
    s.charAt(i - 1)
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
  def apply(in: OneCharReader): RecordingReader =
    new WithRecordingReader(in, 64)
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
  private[this] var tape: Array[Char] = new Array(Math.max(initial, 1))
  private[this] var eob: Int          = -1
  private[this] var writing: Int      = 0
  private[this] var reading: Int      = -1

  def close(): Unit = in.close()

  override def read(): Int =
    try readChar().toInt
    catch {
      case _: UnexpectedEnd =>
        eob = reading
        -1
    }
  override def readChar(): Char =
    if (reading != -1) {
      if (reading == eob) throw new UnexpectedEnd
      val v = tape(reading)
      reading += 1
      if (reading >= writing) {
        reading = -1 // caught up
        writing = -1 // stop recording
      }
      v
    } else {
      val v = in.readChar()
      if (writing != -1) {
        tape(writing) = v
        writing += 1
        if (writing == tape.length)
          tape = Arrays.copyOf(tape, tape.length << 1)
      }
      v
    }

  def rewind(): Unit =
    if (writing != -1)
      reading = 0
    else throw new RewindTwice

  def retract(): Unit =
    if (reading == -1) {
      in match {
        case rr: RetractReader =>
          rr.retract()
          if (writing != -1) {
            writing -= 1 // factor in retracted delegate
          }

        case _ =>
          reading = writing - 1
      }
    } else
      reading -= 1

  def offset(): Int =
    if (reading == -1)
      writing
    else
      reading

  def history(idx: Int): Char = tape(idx)
}
