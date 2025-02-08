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

// Implementations of java.io.Writer that are faster (2x) because they do not
// synchronize on a lock

import java.nio.CharBuffer
import java.util.Arrays

// a minimal subset of java.io.Writer that can be optimised
trait Write {
  def write(c: Char): Unit
  def write(s: String): Unit
  def write(cs: Array[Char], from: Int, to: Int): Unit = {
    var i = from
    while (i < to) {
      write(cs(i))
      i += 1
    }
  }
  @inline def write(c1: Char, c2: Char): Unit = {
    write(c1)
    write(c2)
  }
  @inline def write(c1: Char, c2: Char, c3: Char): Unit = {
    write(c1)
    write(c2)
    write(c3)
  }
  @inline def write(c1: Char, c2: Char, c3: Char, c4: Char): Unit = {
    write(c1)
    write(c2)
    write(c3)
    write(c4)
  }
  @inline def write(c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): Unit = {
    write(c1)
    write(c2)
    write(c3)
    write(c4)
    write(c5)
  }
  @inline def write(s: Short): Unit = {
    write((s & 0xff).toChar)
    write((s >> 8).toChar)
  }
  @inline def write(s1: Short, s2: Short): Unit = {
    write((s1 & 0xff).toChar)
    write((s1 >> 8).toChar)
    write((s2 & 0xff).toChar)
    write((s2 >> 8).toChar)
  }
  @inline def write(s1: Short, s2: Short, s3: Short): Unit = {
    write((s1 & 0xff).toChar)
    write((s1 >> 8).toChar)
    write((s2 & 0xff).toChar)
    write((s2 >> 8).toChar)
    write((s3 & 0xff).toChar)
    write((s3 >> 8).toChar)
  }
  @inline def write(s1: Short, s2: Short, s3: Short, s4: Short): Unit = {
    write((s1 & 0xff).toChar)
    write((s1 >> 8).toChar)
    write((s2 & 0xff).toChar)
    write((s2 >> 8).toChar)
    write((s3 & 0xff).toChar)
    write((s3 >> 8).toChar)
    write((s4 & 0xff).toChar)
    write((s4 >> 8).toChar)
  }
}

// wrapper to implement the legacy Java API
final class WriteWriter(out: java.io.Writer) extends Write {
  def write(s: String): Unit = out.write(s)
  def write(c: Char): Unit   = out.write(c.toInt)
}

// FIXME: remove in the next major version
private[zio] final class FastStringBuilder(initial: Int) {
  private[this] var chars: Array[Char] = new Array[Char](initial)
  private[this] var i: Int             = 0

  @inline
  def append(c: Char): Unit = {
    if (i == chars.length) chars = Arrays.copyOf(chars, chars.length << 1)
    chars(i) = c
    i += 1
  }

  def buffer: CharSequence = CharBuffer.wrap(chars, 0, i)
}
