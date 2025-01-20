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

import scala.annotation.nowarn
import scala.util.control.NoStackTrace

// A port of https://github.com/openjdk/jdk/commit/ebadfaeb2e1cc7b5ce5f101cd8a539bc5478cf5b with optimizations applied
private[json] object UUIDParser {
  // Converts characters to their numeric representation (for example 'E' or 'e' becomes 0XE)
  private[this] val CharToNumeric: Array[Byte] = {
    // by filling in -1's we prevent from trying to parse invalid characters
    val ns = Array.fill[Byte](256)(-1)

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

  def unsafeParse(input: String): java.util.UUID =
    if (
      input.length != 36 || {
        val ch1 = input.charAt(8)
        val ch2 = input.charAt(13)
        val ch3 = input.charAt(18)
        val ch4 = input.charAt(23)
        ch1 != '-' || ch2 != '-' || ch3 != '-' || ch4 != '-'
      }
    ) unsafeParseExtended(input)
    else {
      val ch2n = CharToNumeric
      val msb1 = parseNibbles(ch2n, input, 0)
      val msb2 = parseNibbles(ch2n, input, 4)
      val msb3 = parseNibbles(ch2n, input, 9)
      val msb4 = parseNibbles(ch2n, input, 14)
      val lsb1 = parseNibbles(ch2n, input, 19)
      val lsb2 = parseNibbles(ch2n, input, 24)
      val lsb3 = parseNibbles(ch2n, input, 28)
      val lsb4 = parseNibbles(ch2n, input, 32)
      if ((msb1 | msb2 | msb3 | msb4 | lsb1 | lsb2 | lsb3 | lsb4) < 0) invalidUUIDError(input)
      new java.util.UUID(msb1 << 48 | msb2 << 32 | msb3 << 16 | msb4, lsb1 << 48 | lsb2 << 32 | lsb3 << 16 | lsb4)
    }

  // A nibble is 4 bits
  @nowarn("msg=implicit numeric widening")
  private[this] def parseNibbles(ch2n: Array[Byte], input: String, position: Int): Long = {
    val ch1 = input.charAt(position)
    val ch2 = input.charAt(position + 1)
    val ch3 = input.charAt(position + 2)
    val ch4 = input.charAt(position + 3)
    if ((ch1 | ch2 | ch3 | ch4) > 0xff) -1L
    else ch2n(ch1) << 12 | ch2n(ch2) << 8 | ch2n(ch3) << 4 | ch2n(ch4)
  }

  private[this] def unsafeParseExtended(input: String): java.util.UUID = {
    val len = input.length
    if (len > 36) invalidUUIDError("UUID string too large")
    val dash1 = input.indexOf('-', 0)
    val dash2 = input.indexOf('-', dash1 + 1)
    val dash3 = input.indexOf('-', dash2 + 1)
    val dash4 = input.indexOf('-', dash3 + 1)
    val dash5 = input.indexOf('-', dash4 + 1)

    // For any valid input, dash1 through dash4 will be positive and dash5 will be negative,
    // but it's enough to check dash4 and dash5:
    // - if dash1 is -1, dash4 will be -1
    // - if dash1 is positive but dash2 is -1, dash4 will be -1
    // - if dash1 and dash2 is positive, dash3 will be -1, dash4 will be positive, but so will dash5
    if (dash4 < 0 || dash5 >= 0) invalidUUIDError(input)

    val ch2n     = CharToNumeric
    val section1 = parseSection(ch2n, input, 0, dash1, 0xfffffff00000000L)
    val section2 = parseSection(ch2n, input, dash1 + 1, dash2, 0xfffffffffff0000L)
    val section3 = parseSection(ch2n, input, dash2 + 1, dash3, 0xfffffffffff0000L)
    val section4 = parseSection(ch2n, input, dash3 + 1, dash4, 0xfffffffffff0000L)
    val section5 = parseSection(ch2n, input, dash4 + 1, len, 0xfff000000000000L)
    new java.util.UUID((section1 << 32) | (section2 << 16) | section3, (section4 << 48) | section5)
  }

  @nowarn("msg=implicit numeric widening")
  private[this] def parseSection(
    ch2n: Array[Byte],
    input: String,
    beginIndex: Int,
    endIndex: Int,
    zeroMask: Long
  ): Long = {
    if (beginIndex >= endIndex || beginIndex + 16 < endIndex) invalidUUIDError(input)
    var result = 0L
    var i      = beginIndex
    while (i < endIndex) {
      result = (result << 4) | ch2n(input.charAt(i))
      i += 1
    }
    if ((result & zeroMask) != 0) invalidUUIDError(input)
    result
  }

  @noinline
  private[this] def invalidUUIDError(input: String): Nothing =
    throw new IllegalArgumentException(input) with NoStackTrace
}
