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
}

// wrapper to implement the legacy Java API
final class WriteWriter(out: java.io.Writer) extends Write {
  def write(s: String): Unit = out.write(s)
  def write(c: Char): Unit   = out.write(c.toInt)
}

final class FastStringWrite(initial: Int) extends Write {
  private[this] val sb: java.lang.StringBuilder = new java.lang.StringBuilder(initial)

  def write(s: String): Unit = sb.append(s): Unit

  def write(c: Char): Unit = sb.append(c): Unit

  def buffer: CharSequence = sb
}

// like StringBuilder but doesn't have any encoding or range checks
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
