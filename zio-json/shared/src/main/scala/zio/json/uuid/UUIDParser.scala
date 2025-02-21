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
package zio.json.uuid

import java.util.UUID
import scala.util.control.NoStackTrace

private[json] object UUIDParser {
  private[this] val hexDigits: Array[Byte] = {
    val ns = new Array[Byte](256)
    java.util.Arrays.fill(ns, -1: Byte)
    ns('0') = 0
    ns('1') = 1
    ns('2') = 2
    ns('3') = 3
    ns('4') = 4
    ns('5') = 5
    ns('6') = 6
    ns('7') = 7
    ns('8') = 8
    ns('9') = 9
    ns('A') = 10
    ns('B') = 11
    ns('C') = 12
    ns('D') = 13
    ns('E') = 14
    ns('F') = 15
    ns('a') = 10
    ns('b') = 11
    ns('c') = 12
    ns('d') = 13
    ns('e') = 14
    ns('f') = 15
    ns
  }

  def unsafeParse(input: String): java.util.UUID = {
    if (
      input.length == 36 && {
        val ch1 = input.charAt(8)
        val ch2 = input.charAt(13)
        val ch3 = input.charAt(18)
        val ch4 = input.charAt(23)
        ch1 == '-' && ch2 == '-' && ch3 == '-' && ch4 == '-'
      }
    ) {
      val ds   = hexDigits
      val msb1 = uuidNibble(ds, input, 0)
      val msb2 = uuidNibble(ds, input, 4)
      val msb3 = uuidNibble(ds, input, 9)
      val msb4 = uuidNibble(ds, input, 14)
      val lsb1 = uuidNibble(ds, input, 19)
      val lsb2 = uuidNibble(ds, input, 24)
      val lsb3 = uuidNibble(ds, input, 28)
      val lsb4 = uuidNibble(ds, input, 32)
      if ((msb1 | msb2 | msb3 | msb4 | lsb1 | lsb2 | lsb3 | lsb4) >= 0) {
        return new UUID(
          msb1.toLong << 48 | msb2.toLong << 32 | msb3.toLong << 16 | msb4,
          lsb1.toLong << 48 | lsb2.toLong << 32 | lsb3.toLong << 16 | lsb4
        )
      }
    } else if (input.length <= 36) {
      return uuidExtended(input)
    }
    uuidError()
  }

  private[this] def uuidNibble(ds: Array[Byte], input: String, offset: Int): Int = {
    val ch1 = input.charAt(offset).toInt
    val ch2 = input.charAt(offset + 1).toInt
    val ch3 = input.charAt(offset + 2).toInt
    val ch4 = input.charAt(offset + 3).toInt
    if ((ch1 | ch2 | ch3 | ch4) > 0xff) -1
    else ds(ch1) << 12 | ds(ch2) << 8 | ds(ch3) << 4 | ds(ch4)
  }

  private[this] def uuidExtended(input: String): UUID = {
    val dash1 = input.indexOf('-', 1)
    val dash2 = input.indexOf('-', dash1 + 2)
    val dash3 = input.indexOf('-', dash2 + 2)
    val dash4 = input.indexOf('-', dash3 + 2)
    if (dash4 >= 0) {
      val ds       = hexDigits
      val section1 = uuidSection(ds, input, 0, dash1, 0xffffffff00000000L)
      val section2 = uuidSection(ds, input, dash1 + 1, dash2, 0xffffffffffff0000L)
      val section3 = uuidSection(ds, input, dash2 + 1, dash3, 0xffffffffffff0000L)
      val section4 = uuidSection(ds, input, dash3 + 1, dash4, 0xffffffffffff0000L)
      val section5 = uuidSection(ds, input, dash4 + 1, input.length, 0xffff000000000000L)
      return new UUID((section1 << 32) | (section2 << 16) | section3, (section4 << 48) | section5)
    }
    uuidError()
  }

  private[this] def uuidSection(ds: Array[Byte], input: String, from: Int, to: Int, mask: Long): Long = {
    if (from < to && from + 16 >= to) {
      var result = 0L
      var i      = from
      while (i < to) {
        val c = input.charAt(i).toInt
        if (c > 0xff) uuidError()
        result = (result << 4) | ds(c)
        i += 1
      }
      if ((result & mask) == 0L) return result
    }
    uuidError()
  }

  @noinline private[this] def uuidError(): Nothing = throw new IllegalArgumentException with NoStackTrace
}
