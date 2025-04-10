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

import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import java.time._
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.annotation._

// tries to stick to the spec, but maybe a bit loose in places (e.g. numbers)
//
// https://www.json.org/json-en.html
object Lexer {
  // TODO need a variant that doesn't skip whitespace, so that attack vectors
  // consisting of an infinite stream of space can exit early.

  val NumberMaxBits: Int = 256

  @noinline def error(msg: String, trace: List[JsonError]): Nothing =
    throw UnsafeJson(JsonError.Message(msg) :: trace)

  @noinline private[json] def error(expected: String, got: Char, trace: List[JsonError]): Nothing =
    error(s"expected $expected got '$got'", trace)

  @noinline private[json] def error(c: Char, trace: List[JsonError]): Nothing =
    error(s"invalid '\\$c' in string", trace)

  // FIXME: remove trace paramenter in the next major version
  // True if we got anything besides a }, False for }
  @inline def firstField(trace: List[JsonError], in: RetractReader): Boolean =
    if (in.nextNonWhitespace() != '}') {
      in.retract()
      true
    } else false

  // True if we got a comma, and False for }
  @inline def nextField(trace: List[JsonError], in: OneCharReader): Boolean = {
    val c = in.nextNonWhitespace()
    if (c == ',') true
    else if (c == '}') false
    else error("',' or '}'", c, trace)
  }

  // True if we got anything besides a ], False for ]
  @inline def firstArrayElement(in: RetractReader): Boolean =
    if (in.nextNonWhitespace() != ']') {
      in.retract()
      true
    } else false

  @inline def nextArrayElement(trace: List[JsonError], in: OneCharReader): Boolean = {
    val c = in.nextNonWhitespace()
    if (c == ',') true
    else if (c == ']') false
    else error("',' or ']'", c, trace)
  }

  @inline def field(trace: List[JsonError], in: OneCharReader, matrix: StringMatrix): Int = {
    val f = enumeration(trace, in, matrix)
    val c = in.nextNonWhitespace()
    if (c == ':') return f
    error("':'", c, trace)
  }

  def enumeration(trace: List[JsonError], in: OneCharReader, matrix: StringMatrix): Int = {
    var c = in.nextNonWhitespace()
    if (c != '"') error("'\"'", c, trace)
    var bs = matrix.initial
    var i  = 0
    while ({
      c = in.readChar()
      c != '"'
    }) {
      if (c == '\\') c = nextEscaped(trace, in)
      else if (c < ' ') error("invalid control in string", trace)
      bs = matrix.update(bs, i, c)
      i += 1
    }
    matrix.first(matrix.exact(bs, i))
  }

  def enumeration128(trace: List[JsonError], in: OneCharReader, matrix1: StringMatrix, matrix2: StringMatrix): Int = {
    var c = in.nextNonWhitespace()
    if (c != '"') error("'\"'", c, trace)
    var bs1 = matrix1.initial
    var bs2 = matrix2.initial
    var i   = 0
    while ({
      c = in.readChar()
      c != '"'
    }) {
      if (c == '\\') c = nextEscaped(trace, in)
      else if (c < ' ') error("invalid control in string", trace)
      bs1 = matrix1.update(bs1, i, c)
      bs2 = matrix2.update(bs2, i, c)
      i += 1
    }
    var idx = matrix1.first(matrix1.exact(bs1, i))
    if (idx < 0) idx = matrix2.first(matrix2.exact(bs2, i)) + matrix1.namesLen
    idx
  }

  @inline def field128(trace: List[JsonError], in: OneCharReader, matrix1: StringMatrix, matrix2: StringMatrix): Int = {
    val f = enumeration128(trace, in, matrix1, matrix2)
    val c = in.nextNonWhitespace()
    if (c == ':') return f
    error("':'", c, trace)
  }

  @noinline def skipValue(trace: List[JsonError], in: RetractReader): Unit =
    (in.nextNonWhitespace(): @switch) match {
      case 'n' | 't' => skipFixedChars(in, 3)
      case 'f'       => skipFixedChars(in, 4)
      case '{'       => skipObject(in, 0)
      case '['       => skipArray(in, 0)
      case '"' =>
        skipString(in, evenBackSlashes = true)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        skipNumber(in)
      case c => error(s"unexpected '$c'", trace)
    }

  def skipNumber(in: RetractReader): Unit = {
    while (isNumber(in.readChar())) ()
    in.retract()
  }

  // FIXME: remove in the next major version
  def skipString(trace: List[JsonError], in: OneCharReader): Unit =
    skipString(in, evenBackSlashes = true)

  @tailrec private def skipFixedChars(in: OneCharReader, n: Int): Unit =
    if (n > 0) {
      in.readChar()
      skipFixedChars(in, n - 1)
    }

  @tailrec private def skipString(in: OneCharReader, evenBackSlashes: Boolean): Unit = {
    val ch = in.readChar()
    if (evenBackSlashes) {
      if (ch != '"') skipString(in, ch != '\\')
    } else skipString(in, evenBackSlashes = true)
  }

  @tailrec private def skipObject(in: OneCharReader, level: Int): Unit = {
    val ch = in.readChar()
    if (ch == '"') {
      skipString(in, evenBackSlashes = true)
      skipObject(in, level)
    } else if (ch == '{') skipObject(in, level + 1)
    else if (ch != '}') skipObject(in, level)
    else if (level != 0) skipObject(in, level - 1)
  }

  @tailrec private def skipArray(in: OneCharReader, level: Int): Unit = {
    val b = in.readChar()
    if (b == '"') {
      skipString(in, evenBackSlashes = true)
      skipArray(in, level)
    } else if (b == '[') skipArray(in, level + 1)
    else if (b != ']') skipArray(in, level)
    else if (level != 0) skipArray(in, level - 1)
  }

  def boolean(trace: List[JsonError], in: OneCharReader): Boolean = {
    val c = in.nextNonWhitespace()
    if (c == 't' && in.readChar() == 'r' && in.readChar() == 'u' && in.readChar() == 'e') true
    else if (c == 'f' && in.readChar() == 'a' && in.readChar() == 'l' && in.readChar() == 's' && in.readChar() == 'e')
      false
    else error("expected a Boolean", trace)
  }

  def byte(trace: List[JsonError], in: RetractReader): Byte =
    try {
      val i = UnsafeNumbers.byte_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Byte", trace)
    }

  def short(trace: List[JsonError], in: RetractReader): Short =
    try {
      val i = UnsafeNumbers.short_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Short", trace)
    }

  def int(trace: List[JsonError], in: RetractReader): Int =
    try {
      val i = UnsafeNumbers.int_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected an Int", trace)
    }

  def long(trace: List[JsonError], in: RetractReader): Long =
    try {
      val i = UnsafeNumbers.long_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Long", trace)
    }

  def bigInteger(trace: List[JsonError], in: RetractReader): java.math.BigInteger =
    try {
      val i = UnsafeNumbers.bigInteger_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error(s"expected a $NumberMaxBits-bit BigInteger", trace)
    }

  def bigInt(trace: List[JsonError], in: RetractReader): BigInt =
    try {
      val i = UnsafeNumbers.bigInt_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error(s"expected a $NumberMaxBits-bit BigInt", trace)
    }

  def float(trace: List[JsonError], in: RetractReader): Float =
    try {
      val i = UnsafeNumbers.float_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Float", trace)
    }

  def double(trace: List[JsonError], in: RetractReader): Double =
    try {
      val i = UnsafeNumbers.double_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Double", trace)
    }

  def bigDecimal(trace: List[JsonError], in: RetractReader): java.math.BigDecimal =
    try {
      val i = UnsafeNumbers.bigDecimal_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error(s"expected a BigDecimal with $NumberMaxBits-bit mantissa", trace)
    }

  // FIXME: remove in the next major version
  def streamingString(trace: List[JsonError], in: OneCharReader): java.io.Reader = {
    char(trace, in, '"')
    new OneCharReader {
      def close(): Unit = in.close()

      private[this] var escaped = false

      @tailrec override def read(): Int = {
        val c = in.readChar()
        if (escaped) {
          escaped = false
          ((c: @switch) match {
            case '"' | '\\' | '/' => c
            case 'b'              => '\b'
            case 'f'              => '\f'
            case 'n'              => '\n'
            case 'r'              => '\r'
            case 't'              => '\t'
            case 'u'              => nextHex4(trace, in)
            case c                => error(c, trace)
          }).toInt
        } else if (c == '\\') {
          escaped = true
          read()
        } else if (c == '"') -1 // this is the EOS for the caller
        else if (c < ' ') error("invalid control in string", trace)
        else c.toInt
      }

      // callers expect to get an EOB so this is rare
      def readChar(): Char = {
        val v = read()
        if (v == -1) throw new UnexpectedEnd
        v.toChar
      }
    }
  }

  def string(trace: List[JsonError], in: OneCharReader): CharSequence = {
    var c = in.nextNonWhitespace()
    if (c == '"') {
      var cs = charArrays.get
      var i  = 0
      while ({
        c = in.readChar()
        c != '"'
      }) {
        if (c == '\\') c = nextEscaped(trace, in)
        else if (c < ' ') error("invalid control in string", trace)
        if (i == cs.length) cs = java.util.Arrays.copyOf(cs, i << 1)
        cs(i) = c
        i += 1
      }
      return new String(cs, 0, i)
    }
    error("expected string", trace)
  }

  def uuid(trace: List[JsonError], in: OneCharReader): UUID = {
    var c = in.nextNonWhitespace()
    if (c == '"') {
      val cs   = charArrays.get
      var i, m = 0
      while ({
        c = in.readChar()
        i < 1024 && c != '"'
      }) {
        if (c == '\\') c = nextEscaped(trace, in)
        cs(i) = c
        m |= c
        i += 1
      }
      if (m <= 0xff) {
        if (
          i == 36 && {
            val c1 = cs(8)
            val c2 = cs(13)
            val c3 = cs(18)
            val c4 = cs(23)
            c1 == '-' && c2 == '-' && c3 == '-' && c4 == '-'
          }
        ) {
          val ds = hexDigits
          val msb1 =
            ds(cs(0)).toLong << 28 |
              (ds(cs(1)) << 24 |
                ds(cs(2)) << 20 |
                ds(cs(3)) << 16 |
                ds(cs(4)) << 12 |
                ds(cs(5)) << 8 |
                ds(cs(6)) << 4 |
                ds(cs(7)))
          val msb2 =
            (ds(cs(9)) << 12 |
              ds(cs(10)) << 8 |
              ds(cs(11)) << 4 |
              ds(cs(12))).toLong
          val msb3 =
            (ds(cs(14)) << 12 |
              ds(cs(15)) << 8 |
              ds(cs(16)) << 4 |
              ds(cs(17))).toLong
          val lsb1 =
            (ds(cs(19)) << 12 |
              ds(cs(20)) << 8 |
              ds(cs(21)) << 4 |
              ds(cs(22))).toLong
          val lsb2 =
            (ds(cs(24)) << 16 |
              ds(cs(25)) << 12 |
              ds(cs(26)) << 8 |
              ds(cs(27)) << 4 |
              ds(cs(28))).toLong << 28 |
              (ds(cs(29)) << 24 |
                ds(cs(30)) << 20 |
                ds(cs(31)) << 16 |
                ds(cs(32)) << 12 |
                ds(cs(33)) << 8 |
                ds(cs(34)) << 4 |
                ds(cs(35)))
          if ((msb1 | msb2 | msb3 | lsb1 | lsb2) >= 0L) {
            return new UUID(msb1 << 32 | msb2 << 16 | msb3, lsb1 << 48 | lsb2)
          }
        } else if (i <= 36) {
          return uuidExtended(trace, cs, i)
        }
      }
    }
    uuidError(trace)
  }

  private[this] def uuidExtended(trace: List[JsonError], cs: Array[Char], len: Int): UUID = {
    val dash1 = indexOfDash(cs, 1, len)
    val dash2 = indexOfDash(cs, dash1 + 2, len)
    val dash3 = indexOfDash(cs, dash2 + 2, len)
    val dash4 = indexOfDash(cs, dash3 + 2, len)
    if (dash4 >= 0) {
      val ds       = hexDigits
      val section1 = uuidSection(trace, ds, cs, 0, dash1, 0xffffffff00000000L)
      val section2 = uuidSection(trace, ds, cs, dash1 + 1, dash2, 0xffffffffffff0000L)
      val section3 = uuidSection(trace, ds, cs, dash2 + 1, dash3, 0xffffffffffff0000L)
      val section4 = uuidSection(trace, ds, cs, dash3 + 1, dash4, 0xffffffffffff0000L)
      val section5 = uuidSection(trace, ds, cs, dash4 + 1, len, 0xffff000000000000L)
      return new UUID((section1 << 32) | (section2 << 16) | section3, (section4 << 48) | section5)
    }
    uuidError(trace)
  }

  private[this] def indexOfDash(cs: Array[Char], from: Int, to: Int): Int = {
    var i = from
    while (i < to) {
      if (cs(i) == '-') return i
      i += 1
    }
    -1
  }

  private[this] def uuidSection(
    trace: List[JsonError],
    ds: Array[Byte],
    cs: Array[Char],
    from: Int,
    to: Int,
    mask: Long
  ): Long = {
    if (from < to && from + 16 >= to) {
      var result = 0L
      var i      = from
      while (i < to) {
        result = (result << 4) | ds(cs(i))
        i += 1
      }
      if ((result & mask) == 0L) return result
    }
    uuidError(trace)
  }

  def duration(trace: List[JsonError], in: OneCharReader): Duration = {
    if (in.nextNonWhitespace() == '"') {
      val cs                = charArrays.get
      val i                 = readChars(trace, in, cs)
      var seconds           = 0L
      var nanos, pos, state = 0
      if (pos >= i) durationError(trace)
      var ch = cs(pos)
      pos += 1
      val isNeg = ch == '-'
      if (isNeg) {
        if (pos >= i) durationError(trace)
        ch = cs(pos)
        pos += 1
      }
      if (ch != 'P' || pos >= i) durationError(trace)
      ch = cs(pos)
      pos += 1
      while ({
        if (state == 0) {
          if (ch == 'T') {
            if (pos >= i) durationError(trace)
            ch = cs(pos)
            pos += 1
            state = 1
          }
        } else if (state == 1) {
          if (ch != 'T' || pos >= i) durationError(trace)
          ch = cs(pos)
          pos += 1
        } else if (state == 4 && pos >= i) durationError(trace)
        val isNegX = ch == '-'
        if (isNegX) {
          if (pos >= i) durationError(trace)
          ch = cs(pos)
          pos += 1
        }
        if (ch < '0' || ch > '9') durationError(trace)
        var x: Long = ('0' - ch).toLong
        while (
          (pos < i) && {
            ch = cs(pos)
            ch >= '0' && ch <= '9'
          }
        ) {
          if (
            x < -922337203685477580L || {
              x = x * 10 + ('0' - ch)
              x > 0
            }
          ) durationError(trace)
          pos += 1
        }
        if (!(isNeg ^ isNegX)) {
          if (x == -9223372036854775808L) durationError(trace)
          x = -x
        }
        if (ch == 'D' && state <= 0) {
          if (x < -106751991167300L || x > 106751991167300L) durationError(trace)
          seconds = x * 86400
          state = 1
        } else if (ch == 'H' && state <= 1) {
          if (x < -2562047788015215L || x > 2562047788015215L) durationError(trace)
          seconds = sumSeconds(x * 3600, seconds, trace)
          state = 2
        } else if (ch == 'M' && state <= 2) {
          if (x < -153722867280912930L || x > 153722867280912930L) durationError(trace)
          seconds = sumSeconds(x * 60, seconds, trace)
          state = 3
        } else if (ch == '.') {
          pos += 1
          seconds = sumSeconds(x, seconds, trace)
          var nanoDigitWeight = 100000000
          while (
            (pos < i) && {
              ch = cs(pos)
              ch >= '0' && ch <= '9' && nanoDigitWeight != 0
            }
          ) {
            nanos += (ch - '0') * nanoDigitWeight
            nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
            pos += 1
          }
          if (ch != 'S') durationError(trace)
          if (isNeg ^ isNegX) nanos = -nanos
          state = 4
        } else if (ch == 'S') {
          seconds = sumSeconds(x, seconds, trace)
          state = 4
        } else durationError(trace)
        pos += 1
        (pos < i) && {
          ch = cs(pos)
          pos += 1
          true
        }
      }) ()
      return Duration.ofSeconds(seconds, nanos.toLong)
    }
    durationError(trace)
  }

  def instant(trace: List[JsonError], in: OneCharReader): Instant = {
    if (in.nextNonWhitespace() == '"') {
      val cs                    = charArrays.get
      val i                     = readChars(trace, in, cs)
      var pos, year, month, day = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              ch4 != '-'
            } else {
              year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
              val yearNeg = ch0 == '-' || (ch0 != '+' && instantError(trace))
              ch4 < '0' || ch4 > '9' || {
                var yearDigits = 4
                var ch         = '0'
                while (
                  pos < i && {
                    ch = cs(pos)
                    pos += 1
                    ch >= '0' && ch <= '9' && yearDigits < 10
                  }
                ) {
                  year =
                    if (year > 100000000) 2147483647
                    else year * 10 + (ch - '0')
                  yearDigits += 1
                }
                yearDigits == 10 && year > 1000000000 || yearNeg && {
                  year = -year
                  year == 0
                } || ch != '-'
              }
            }
          }
        } || pos + 5 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          val ch5 = cs(pos + 5)
          pos += 6
          month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
          ch4 < '0' || ch4 > '9' || ch5 != 'T' || month < 1 || month > 12 || day == 0 ||
          (day > 28 && day > maxDayForYearMonth(year, month))
        }
      ) instantError(trace)
      val epochDay =
        epochDayForYear(year) + (dayOfYearForYearMonth(year, month) + day - 719529) // 719528 == days 0000 to 1970
      var epochSecond = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          epochSecond = hour * 3600 + (ch3 * 10 + ch4 - 528) * 60 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
          ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
        }
      ) instantError(trace)
      var nano = 0
      var ch   = '0'
      if (pos < i) {
        ch = cs(pos)
        pos += 1
        if (ch == ':') {
          if (
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              epochSecond += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) instantError(trace)
          if (pos < i) {
            ch = cs(pos)
            pos += 1
            if (ch == '.') {
              var nanoDigitWeight = 100000000
              while (
                pos < i && {
                  ch = cs(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && nanoDigitWeight != 0
                }
              ) {
                nano += (ch - '0') * nanoDigitWeight
                nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
              }
            }
          }
        }
      }
      var offsetTotal = 0
      if (ch != 'Z') {
        val offsetNeg = ch == '-' || (ch != '+' && instantError(trace))
        if (
          pos + 1 >= i || {
            val ch0 = cs(pos)
            val ch1 = cs(pos + 1)
            pos += 2
            offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
          }
        ) instantError(trace)
        if (
          pos < i && {
            ch = cs(pos)
            pos += 1
            ch == ':'
          }
        ) {
          if (
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) instantError(trace)
          if (
            pos < i && {
              ch = cs(pos)
              pos += 1
              ch == ':'
            }
          ) {
            if (
              pos + 1 >= i || {
                val ch0 = cs(pos)
                val ch1 = cs(pos + 1)
                pos += 2
                offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) instantError(trace)
          }
        }
        if (offsetTotal > 64800) instantError(trace) // 64800 == 18 * 60 * 60
        if (offsetNeg) offsetTotal = -offsetTotal
      }
      if (pos == i) return Instant.ofEpochSecond(epochDay * 86400 + (epochSecond - offsetTotal), nano.toLong)
    }
    instantError(trace)
  }

  def localDate(trace: List[JsonError], in: OneCharReader): LocalDate = {
    var year, month, day = 0
    if (
      in.nextNonWhitespace() != '"' || {
        val cs  = charArrays.get
        val i   = readChars(trace, in, cs)
        var pos = 0
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              ch4 != '-'
            } else {
              year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
              val yearNeg = ch0 == '-' || (ch0 != '+' && localDateError(trace))
              ch4 < '0' || ch4 > '9' || {
                var yearDigits = 4
                var ch         = '0'
                while (
                  pos < i && {
                    ch = cs(pos)
                    pos += 1
                    ch >= '0' && ch <= '9' && yearDigits < 9
                  }
                ) {
                  year = year * 10 + (ch - '0')
                  yearDigits += 1
                }
                yearNeg && {
                  year = -year
                  year == 0
                } || ch != '-'
              }
            }
          }
        } || pos + 5 != i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
          ch4 < '0' || ch4 > '9' || month < 1 || month > 12 || day == 0 ||
          (day > 28 && day > maxDayForYearMonth(year, month))
        }
      }
    ) localDateError(trace)
    LocalDate.of(year, month, day)
  }

  def localDateTime(trace: List[JsonError], in: OneCharReader): LocalDateTime = {
    if (in.nextNonWhitespace() == '"') {
      val cs                    = charArrays.get
      val i                     = readChars(trace, in, cs)
      var pos, year, month, day = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              ch4 != '-'
            } else {
              year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
              val yearNeg = ch0 == '-' || (ch0 != '+' && localDateTimeError(trace))
              ch4 < '0' || ch4 > '9' || {
                var yearDigits = 4
                var ch         = '0'
                while (
                  pos < i && {
                    ch = cs(pos)
                    pos += 1
                    ch >= '0' && ch <= '9' && yearDigits < 9
                  }
                ) {
                  year = year * 10 + (ch - '0')
                  yearDigits += 1
                }
                yearNeg && {
                  year = -year
                  year == 0
                } || ch != '-'
              }
            }
          }
        } || pos + 5 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          val ch5 = cs(pos + 5)
          pos += 6
          month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
          ch4 < '0' || ch4 > '9' || ch5 != 'T' || day == 0 || month < 1 || month > 12 ||
          (day > 28 && day > maxDayForYearMonth(year, month))
        }
      ) localDateTimeError(trace)
      var hour, minute = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
          minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
          ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
        }
      ) localDateTimeError(trace)
      var second, nano = 0
      if (pos < i) {
        if (
          cs(pos) != ':' || {
            pos += 1
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          }
        ) localDateTimeError(trace)
        if (pos < i) {
          if (
            cs(pos) != '.' || {
              pos += 1
              var nanoDigitWeight = 100000000
              var ch              = '0'
              while (
                pos < i && {
                  ch = cs(pos)
                  ch >= '0' && ch <= '9' && nanoDigitWeight != 0
                }
              ) {
                nano += (ch - '0') * nanoDigitWeight
                nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
                pos += 1
              }
              pos != i
            }
          ) localDateTimeError(trace)
        }
      }
      return LocalDateTime.of(year, month, day, hour, minute, second, nano)
    }
    localDateTimeError(trace)
  }

  def localTime(trace: List[JsonError], in: OneCharReader): LocalTime = {
    if (in.nextNonWhitespace() == '"') {
      val cs                = charArrays.get
      val i                 = readChars(trace, in, cs)
      var pos, hour, minute = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
          minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
          ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
        }
      ) localTimeError(trace)
      var second, nano = 0
      if (pos < i) {
        if (
          cs(pos) != ':' || {
            pos += 1
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          }
        ) localTimeError(trace)
        if (pos < i) {
          if (
            cs(pos) != '.' || {
              pos += 1
              var nanoDigitWeight = 100000000
              var ch              = '0'
              while (
                pos < i && {
                  ch = cs(pos)
                  ch >= '0' && ch <= '9' && nanoDigitWeight != 0
                }
              ) {
                nano += (ch - '0') * nanoDigitWeight
                nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
                pos += 1
              }
              pos != i
            }
          ) localTimeError(trace)
        }
      }
      return LocalTime.of(hour, minute, second, nano)
    }
    localTimeError(trace)
  }

  def monthDay(trace: List[JsonError], in: OneCharReader): MonthDay = {
    var month, day = 0
    if (
      in.nextNonWhitespace() != '"' || {
        val cs = charArrays.get
        val i  = readChars(trace, in, cs)
        i != 7 || {
          val ch0 = cs(0)
          val ch1 = cs(1)
          val ch2 = cs(2)
          val ch3 = cs(3)
          val ch4 = cs(4)
          val ch5 = cs(5)
          val ch6 = cs(6)
          month = ch2 * 10 + ch3 - 528 // 528 == '0' * 11
          day = ch5 * 10 + ch6 - 528   // 528 == '0' * 11
          ch0 != '-' || ch1 != '-' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || ch4 != '-' ||
          ch5 < '0' || ch5 > '9' || ch6 < '0' || ch6 > '9' || month < 1 || month > 12 || day == 0 ||
          (day > 28 && day > maxDayForMonth(month))
        }
      }
    ) monthDayError(trace)
    MonthDay.of(month, day)
  }

  def offsetDateTime(trace: List[JsonError], in: OneCharReader): OffsetDateTime = {
    if (in.nextNonWhitespace() == '"') {
      val cs                    = charArrays.get
      val i                     = readChars(trace, in, cs)
      var pos, year, month, day = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              ch4 != '-'
            } else {
              year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
              val yearNeg = ch0 == '-' || (ch0 != '+' && offsetDateTimeError(trace))
              ch4 < '0' || ch4 > '9' || {
                var yearDigits = 4
                var ch         = '0'
                while (
                  pos < i && {
                    ch = cs(pos)
                    pos += 1
                    ch >= '0' && ch <= '9' && yearDigits < 9
                  }
                ) {
                  year = year * 10 + (ch - '0')
                  yearDigits += 1
                }
                yearNeg && {
                  year = -year
                  year == 0
                } || ch != '-'
              }
            }
          }
        } || pos + 5 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          val ch5 = cs(pos + 5)
          pos += 6
          month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
          ch4 < '0' || ch4 > '9' || ch5 != 'T' || month < 1 || month > 12 || day == 0 ||
          (day > 28 && day > maxDayForYearMonth(year, month))
        }
      ) offsetDateTimeError(trace)
      var hour, minute = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
          minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
          ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
        } || pos >= i
      ) offsetDateTimeError(trace)
      var second, nano = 0
      var ch           = cs(pos)
      pos += 1
      if (ch == ':') {
        if (
          pos + 1 >= i || {
            val ch0 = cs(pos)
            val ch1 = cs(pos + 1)
            pos += 2
            second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          } || pos >= i
        ) offsetDateTimeError(trace)
        ch = cs(pos)
        pos += 1
        if (ch == '.') {
          var nanoDigitWeight = 100000000
          while ({
            if (pos >= i) offsetDateTimeError(trace)
            ch = cs(pos)
            pos += 1
            ch >= '0' && ch <= '9' && nanoDigitWeight != 0
          }) {
            nano += (ch - '0') * nanoDigitWeight
            nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
          }
        }
      }
      val zoneOffset =
        if (ch == 'Z') ZoneOffset.UTC
        else {
          val offsetNeg   = ch == '-' || (ch != '+' && offsetDateTimeError(trace))
          var offsetTotal = 0
          if (
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
            }
          ) offsetDateTimeError(trace)
          if (pos < i) {
            if (
              cs(pos) != ':' || {
                pos += 1
                pos + 1 >= i
              } || {
                val ch0 = cs(pos)
                val ch1 = cs(pos + 1)
                pos += 2
                offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) offsetDateTimeError(trace)
            if (pos < i) {
              if (
                cs(pos) != ':' || {
                  pos += 1
                  pos + 1 >= i
                } || {
                  val ch0 = cs(pos)
                  val ch1 = cs(pos + 1)
                  pos += 2
                  offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                  ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
                }
              ) offsetDateTimeError(trace)
            }
          }
          if (offsetTotal > 64800) offsetDateTimeError(trace)
          toZoneOffset(offsetNeg, offsetTotal)
        }
      if (pos == i) return OffsetDateTime.of(year, month, day, hour, minute, second, nano, zoneOffset)
    }
    offsetDateTimeError(trace)
  }

  def offsetTime(trace: List[JsonError], in: OneCharReader): OffsetTime = {
    if (in.nextNonWhitespace() == '"') {
      val cs                = charArrays.get
      val i                 = readChars(trace, in, cs)
      var pos, hour, minute = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
          minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
          ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
        } || pos >= i
      ) offsetTimeError(trace)
      var second, nano = 0
      var ch           = cs(pos)
      pos += 1
      if (ch == ':') {
        if (
          pos + 1 >= i || {
            val ch0 = cs(pos)
            val ch1 = cs(pos + 1)
            pos += 2
            second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          } || pos >= i
        ) offsetTimeError(trace)
        ch = cs(pos)
        pos += 1
        if (ch == '.') {
          var nanoDigitWeight = 100000000
          while ({
            if (pos >= i) offsetTimeError(trace)
            ch = cs(pos)
            pos += 1
            ch >= '0' && ch <= '9' && nanoDigitWeight != 0
          }) {
            nano += (ch - '0') * nanoDigitWeight
            nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
          }
        }
      }
      val zoneOffset =
        if (ch == 'Z') ZoneOffset.UTC
        else {
          val offsetNeg   = ch == '-' || (ch != '+' && offsetTimeError(trace))
          var offsetTotal = 0
          if (
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
            }
          ) offsetTimeError(trace)
          if (pos < i) {
            if (
              cs(pos) != ':' || {
                pos += 1
                pos + 1 >= i
              } || {
                val ch0 = cs(pos)
                val ch1 = cs(pos + 1)
                pos += 2
                offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) offsetTimeError(trace)
            if (pos < i) {
              if (
                cs(pos) != ':' || {
                  pos += 1
                  pos + 1 >= i
                } || {
                  val ch0 = cs(pos)
                  val ch1 = cs(pos + 1)
                  pos += 2
                  offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                  ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
                }
              ) offsetTimeError(trace)
            }
          }
          if (offsetTotal > 64800) offsetTimeError(trace)
          toZoneOffset(offsetNeg, offsetTotal)
        }
      if (pos == i) return OffsetTime.of(hour, minute, second, nano, zoneOffset)
    }
    offsetTimeError(trace)
  }

  def period(trace: List[JsonError], in: OneCharReader): Period = {
    if (in.nextNonWhitespace() == '"') {
      val cs                              = charArrays.get
      val i                               = readChars(trace, in, cs)
      var pos, state, years, months, days = 0
      if (pos >= i) periodError(trace)
      var ch = cs(pos)
      pos += 1
      val isNeg = ch == '-'
      if (isNeg) {
        if (pos >= i) periodError(trace)
        ch = cs(pos)
        pos += 1
      }
      if (ch != 'P' || pos >= i) periodError(trace)
      ch = cs(pos)
      pos += 1
      while ({
        if (state == 4 && pos >= i) periodError(trace)
        val isNegX = ch == '-'
        if (isNegX) {
          if (pos >= i) periodError(trace)
          ch = cs(pos)
          pos += 1
        }
        if (ch < '0' || ch > '9') periodError(trace)
        var x: Int = '0' - ch
        while (
          (pos < i) && {
            ch = cs(pos)
            ch >= '0' && ch <= '9'
          }
        ) {
          if (
            x < -214748364 || {
              x = x * 10 + ('0' - ch)
              x > 0
            }
          ) periodError(trace)
          pos += 1
        }
        if (!(isNeg ^ isNegX)) {
          if (x == -2147483648) periodError(trace)
          x = -x
        }
        if (ch == 'Y' && state <= 0) {
          years = x
          state = 1
        } else if (ch == 'M' && state <= 1) {
          months = x
          state = 2
        } else if (ch == 'W' && state <= 2) {
          if (x < -306783378 || x > 306783378) periodError(trace)
          days = x * 7
          state = 3
        } else if (ch == 'D') {
          val ds = x.toLong + days
          if (ds != ds.toInt) periodError(trace)
          days = ds.toInt
          state = 4
        } else periodError(trace)
        pos += 1
        (pos < i) && {
          ch = cs(pos)
          pos += 1
          true
        }
      }) ()
      return Period.of(years, months, days)
    }
    periodError(trace)
  }

  def year(trace: List[JsonError], in: OneCharReader): Year = {
    if (in.nextNonWhitespace() == '"') {
      val cs        = charArrays.get
      val i         = readChars(trace, in, cs)
      var pos, year = 0
      if (
        pos + 3 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          pos += 4
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              pos != i
            } else {
              val yearNeg = ch0 == '-' || (ch0 != '+' && yearError(trace))
              year = ch1 * 100 + ch2 * 10 + ch3 - 5328 // 53328 == '0' * 111
              var yearDigits = 3
              var ch         = '0'
              while (
                pos < i && {
                  ch = cs(pos)
                  ch >= '0' && ch <= '9' && yearDigits < 9
                }
              ) {
                year = year * 10 + (ch - '0')
                yearDigits += 1
                pos += 1
              }
              yearNeg && {
                year = -year
                year == 0
              } || pos != i
            }
          }
        }
      ) yearError(trace)
      return Year.of(year)
    }
    yearError(trace)
  }

  def yearMonth(trace: List[JsonError], in: OneCharReader): YearMonth = {
    if (in.nextNonWhitespace() == '"') {
      val cs               = charArrays.get
      val i                = readChars(trace, in, cs)
      var pos, year, month = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              ch4 != '-'
            } else {
              val yearNeg = ch0 == '-' || (ch0 != '+' && yearMonthError(trace))
              year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
              ch4 < '0' || ch4 > '9' || {
                var yearDigits = 4
                var ch         = '0'
                while ({
                  if (pos >= i) yearMonthError(trace)
                  ch = cs(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && yearDigits < 9
                }) {
                  year = year * 10 + (ch - '0')
                  yearDigits += 1
                }
                yearNeg && {
                  year = -year
                  year == 0
                } || ch != '-'
              }
            }
          }
        } || pos + 2 != i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          pos += 2
          month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || month < 1 || month > 12
        }
      ) yearMonthError(trace)
      return YearMonth.of(year, month)
    }
    yearMonthError(trace)
  }

  def zonedDateTime(trace: List[JsonError], in: OneCharReader): ZonedDateTime = {
    if (in.nextNonWhitespace() == '"') {
      val cs                                  = charArrays.get
      val i                                   = readChars(trace, in, cs)
      var pos, year, month, day, hour, minute = 0
      if (
        pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
            if (ch0 >= '0' && ch0 <= '9') {
              year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
              ch4 != '-'
            } else {
              val yearNeg = ch0 == '-' || (ch0 != '+' && zonedDateTimeError(trace))
              year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
              ch4 < '0' || ch4 > '9' || {
                var yearDigits = 4
                var ch         = '0'
                while ({
                  if (pos >= i) zonedDateTimeError(trace)
                  ch = cs(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && yearDigits < 9
                }) {
                  year = year * 10 + (ch - '0')
                  yearDigits += 1
                }
                yearNeg && {
                  year = -year
                  year == 0
                } || ch != '-'
              }
            }
          }
        } || pos + 5 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          val ch5 = cs(pos + 5)
          pos += 6
          month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
          ch4 < '0' || ch4 > '9' || ch5 != 'T' || month < 1 || month > 12 || day == 0 ||
          (day > 28 && day > maxDayForYearMonth(year, month))
        } || pos + 4 >= i || {
          val ch0 = cs(pos)
          val ch1 = cs(pos + 1)
          val ch2 = cs(pos + 2)
          val ch3 = cs(pos + 3)
          val ch4 = cs(pos + 4)
          pos += 5
          hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
          minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
          ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
        } || pos >= i
      ) zonedDateTimeError(trace)
      var second, nano = 0
      var ch           = cs(pos)
      pos += 1
      if (ch == ':') {
        if (
          pos + 1 >= i || {
            val ch0 = cs(pos)
            val ch1 = cs(pos + 1)
            pos += 2
            second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          } || pos >= i
        ) zonedDateTimeError(trace)
        ch = cs(pos)
        pos += 1
        if (ch == '.') {
          var nanoDigitWeight = 100000000
          while ({
            if (pos >= i) zonedDateTimeError(trace)
            ch = cs(pos)
            pos += 1
            ch >= '0' && ch <= '9' && nanoDigitWeight != 0
          }) {
            nano += (ch - '0') * nanoDigitWeight
            nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
          }
        }
      }
      val localDateTime = LocalDateTime.of(year, month, day, hour, minute, second, nano)
      val zoneOffset =
        if (ch == 'Z') {
          if (pos < i) {
            ch = cs(pos)
            if (ch != '[') zonedDateTimeError(trace)
            pos += 1
          }
          ZoneOffset.UTC
        } else {
          val offsetNeg   = ch == '-' || (ch != '+' && zonedDateTimeError(trace))
          var offsetTotal = 0
          if (
            pos + 1 >= i || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
            }
          ) zonedDateTimeError(trace)
          if (
            pos < i && {
              ch = cs(pos)
              pos += 1
              ch == ':' || ch != '[' && zonedDateTimeError(trace)
            }
          ) {
            if (
              pos + 1 >= i || {
                val ch0 = cs(pos)
                val ch1 = cs(pos + 1)
                pos += 2
                offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) zonedDateTimeError(trace)
            if (
              pos < i && {
                ch = cs(pos)
                pos += 1
                ch == ':' || ch != '[' && zonedDateTimeError(trace)
              }
            ) {
              if (
                pos + 1 >= i || {
                  val ch0 = cs(pos)
                  val ch1 = cs(pos + 1)
                  pos += 2
                  offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                  ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
                }
              ) zonedDateTimeError(trace)
              if (pos < i) {
                ch = cs(pos)
                if (ch != '[') zonedDateTimeError(trace)
                pos += 1
              }
            }
          }
          if (offsetTotal > 64800) zonedDateTimeError(trace)
          toZoneOffset(offsetNeg, offsetTotal)
        }
      if (ch == '[') {
        var zoneId: ZoneId = null
        val from           = pos
        while ({
          if (pos >= i) zonedDateTimeError(trace)
          ch = cs(pos)
          ch != ']'
        }) pos += 1
        val key = new String(cs, from, pos - from)
        zoneId = zoneIds.get(key)
        if (
          (zoneId eq null) && {
            try zoneId = ZoneId.of(key)
            catch {
              case _: DateTimeException => zonedDateTimeError(trace)
            }
            !zoneId.isInstanceOf[ZoneOffset] || zoneId.asInstanceOf[ZoneOffset].getTotalSeconds % 900 == 0
          }
        ) zoneIds.put(key, zoneId)
        if (pos + 1 == i) return ZonedDateTime.ofInstant(localDateTime, zoneOffset, zoneId)
      } else {
        if (pos == i) return ZonedDateTime.ofLocal(localDateTime, zoneOffset, null)
      }
    }
    zonedDateTimeError(trace)
  }

  def zoneOffset(trace: List[JsonError], in: OneCharReader): ZoneOffset = {
    if (in.nextNonWhitespace() == '"') {
      val cs  = charArrays.get
      val i   = readChars(trace, in, cs)
      var pos = 0
      if (pos >= i) zoneOffsetError(trace)
      val ch = cs(pos)
      pos += 1
      if (ch == 'Z') {
        if (pos == i) return ZoneOffset.UTC
      } else {
        val offsetNeg   = ch == '-' || (ch != '+' && zoneOffsetError(trace))
        var offsetTotal = 0
        if (
          pos + 1 >= i || {
            val ch0 = cs(pos)
            val ch1 = cs(pos + 1)
            offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
            pos += 2
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
          }
        ) zoneOffsetError(trace)
        if (pos < i) {
          if (
            cs(pos) != ':' || {
              pos += 1
              pos + 1 >= i
            } || {
              val ch0 = cs(pos)
              val ch1 = cs(pos + 1)
              pos += 2
              offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) zoneOffsetError(trace)
          if (pos < i) {
            if (
              cs(pos) != ':' || {
                pos += 1
                pos + 1 >= i
              } || {
                val ch0 = cs(pos)
                val ch1 = cs(pos + 1)
                pos += 2
                offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) zoneOffsetError(trace)
          }
        }
        if (offsetTotal <= 64800 && pos == i) return toZoneOffset(offsetNeg, offsetTotal)
      }
    }
    zoneOffsetError(trace)
  }

  private[this] def readChars(trace: List[JsonError], in: OneCharReader, cs: Array[Char]): Int = {
    val len = cs.length
    var c   = '0'
    var i   = 0
    while ({
      c = in.readChar()
      i < len && c != '"'
    }) {
      if (c == '\\') c = nextEscaped(trace, in)
      cs(i) = c
      i += 1
    }
    i
  }

  private[this] def toZoneOffset(offsetNeg: Boolean, offsetTotal: Int): ZoneOffset = {
    var qp = offsetTotal * 37283
    if ((qp & 0x1ff8000) == 0) { // check if offsetTotal divisible by 900
      qp >>>= 25                 // divide offsetTotal by 900
      if (offsetNeg) qp = -qp
      var zoneOffset = zoneOffsets(qp + 72)
      if (zoneOffset ne null) zoneOffset
      else {
        zoneOffset = ZoneOffset.ofTotalSeconds(if (offsetNeg) -offsetTotal else offsetTotal)
        zoneOffsets(qp + 72) = zoneOffset
        zoneOffset
      }
    } else ZoneOffset.ofTotalSeconds(if (offsetNeg) -offsetTotal else offsetTotal)
  }

  private[this] def sumSeconds(s1: Long, s2: Long, trace: List[JsonError]): Long = {
    val s = s1 + s2
    if (((s1 ^ s) & (s2 ^ s)) < 0) durationError(trace)
    s
  }

  private[this] def epochDayForYear(year: Int): Long =
    year * 365L + ((year + 3 >> 2) - {
      val cp = year * 1374389535L
      if (year < 0) (cp >> 37) - (cp >> 39)                        // year / 100 - year / 400
      else (cp + 136064563965L >> 37) - (cp + 548381424465L >> 39) // (year + 99) / 100 - (year + 399) / 400
    }.toInt)

  private[this] def dayOfYearForYearMonth(year: Int, month: Int): Int =
    (month * 1002277 - 988622 >> 15) - // (month * 367 - 362) / 12
      (if (month <= 2) 0
       else if (isLeap(year)) 1
       else 2)

  private[this] def maxDayForMonth(month: Int): Int =
    if (month != 2) ((month >> 3) ^ (month & 0x1)) + 30
    else 29

  private[this] def maxDayForYearMonth(year: Int, month: Int): Int =
    if (month != 2) ((month >> 3) ^ (month & 0x1)) + 30
    else if (isLeap(year)) 29
    else 28

  private[this] def isLeap(year: Int): Boolean = (year & 0x3) == 0 && { // (year % 100 != 0 || year % 400 == 0)
    val cp = year * 1374389535L
    val cc = year >> 31
    ((cp ^ cc) & 0x1fc0000000L) != 0 || (((cp >> 37).toInt - cc) & 0x3) == 0
  }

  @noinline private[this] def uuidError(trace: List[JsonError]): Nothing = error("expected a UUID", trace)

  @noinline private[this] def durationError(trace: List[JsonError]): Nothing = error("expected a Duration", trace)

  @noinline private[this] def instantError(trace: List[JsonError]): Nothing = error("expected an Instant", trace)

  @noinline private[this] def localDateError(trace: List[JsonError]): Nothing = error("expected a LocalDate", trace)

  @noinline private[this] def localDateTimeError(trace: List[JsonError]): Nothing =
    error("expected a LocalDateTime", trace)

  @noinline private[this] def localTimeError(trace: List[JsonError]): Nothing = error("expected a LocalTime", trace)

  @noinline private[this] def monthDayError(trace: List[JsonError]): Nothing = error("expected a MonthDay", trace)

  @noinline private[this] def offsetDateTimeError(trace: List[JsonError]): Nothing =
    error("expected an OffsetDateTime", trace)

  @noinline private[this] def offsetTimeError(trace: List[JsonError]): Nothing = error("expected an OffsetTime", trace)

  @noinline private[this] def periodError(trace: List[JsonError]): Nothing = error("expected a Period", trace)

  @noinline private[this] def yearError(trace: List[JsonError]): Nothing = error("expected a Year", trace)

  @noinline private[this] def yearMonthError(trace: List[JsonError]): Nothing = error("expected a YearMonth", trace)

  @noinline private[this] def zonedDateTimeError(trace: List[JsonError]): Nothing =
    error("expected a ZonedDateTime", trace)

  @noinline private[this] def zoneIdError(trace: List[JsonError]): Nothing = error("expected a ZoneId", trace)

  @noinline private[this] def zoneOffsetError(trace: List[JsonError]): Nothing = error("expected a ZoneOffset", trace)

  private[this] val charArrays = new ThreadLocal[Array[Char]] {
    override def initialValue(): Array[Char] = new Array[Char](1024)
  }

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

  private[this] final val zoneOffsets: Array[ZoneOffset] = new Array(145)

  private[this] final val zoneIds: ConcurrentHashMap[String, ZoneId] = new ConcurrentHashMap(256)

  def char(trace: List[JsonError], in: OneCharReader): Char = {
    var c = in.nextNonWhitespace()
    if (c != '"') error("'\"'", c, trace)
    c = in.readChar()
    if (
      c == '"' || {
        if (c == '\\') c = nextEscaped(trace, in)
        else if (c < ' ') error("invalid control in string", trace)
        in.readChar() != '"'
      }
    ) error("expected single character string", trace)
    c
  }

  @noinline private[this] def nextEscaped(trace: List[JsonError], in: OneCharReader): Char =
    (in.readChar(): @switch) match {
      case '"'  => '"'
      case '\\' => '\\'
      case '/'  => '/'
      case 'b'  => '\b'
      case 'f'  => '\f'
      case 'n'  => '\n'
      case 'r'  => '\r'
      case 't'  => '\t'
      case 'u'  => nextHex4(trace, in)
      case c    => error(c, trace)
    }

  private[this] def nextHex4(trace: List[JsonError], in: OneCharReader): Char = {
    var i, accum = 0
    while (i < 4) {
      val c = in.readChar() | 0x20
      accum = (accum << 4) + c
      i += 1
      if ('0' <= c && c <= '9') accum -= 48
      else if ('a' <= c && c <= 'f') accum -= 87
      else error("invalid charcode in string", trace)
    }
    accum.toChar
  }

  def dayOfWeek(trace: List[JsonError], in: OneCharReader): DayOfWeek = {
    var c = in.nextNonWhitespace()
    if (c == '"') {
      var bs = dayOfWeekMatrix.initial
      var i  = 0
      while ({
        c = in.readChar()
        c != '"'
      }) {
        if (c == '\\') c = nextEscaped(trace, in)
        bs = dayOfWeekMatrix.update(bs, i, (c & 0xffdf).toChar)
        i += 1
      }
      val dayOfWeek = dayOfWeekMatrix.first(dayOfWeekMatrix.exact(bs, i)) + 1
      if (dayOfWeek > 0) return DayOfWeek.of(dayOfWeek)
    }
    error("expected a DayOfWeek", trace)
  }

  private[this] val dayOfWeekMatrix = new StringMatrix(DayOfWeek.values.map(_.toString))

  def month(trace: List[JsonError], in: OneCharReader): Month = {
    var c = in.nextNonWhitespace()
    if (c == '"') {
      var bs = monthMatrix.initial
      var i  = 0
      while ({
        c = in.readChar()
        c != '"'
      }) {
        if (c == '\\') c = nextEscaped(trace, in)
        bs = monthMatrix.update(bs, i, (c & 0xffdf).toChar)
        i += 1
      }
      val month = monthMatrix.first(monthMatrix.exact(bs, i)) + 1
      if (month > 0) return Month.of(month)
    }
    error("expected a Month", trace)
  }

  private[this] val monthMatrix = new StringMatrix(Month.values.map(_.toString))

  @inline def char(trace: List[JsonError], in: OneCharReader, c: Char): Unit = {
    val got = in.nextNonWhitespace()
    if (got != c) error(s"'$c'", got, trace)
  }

  // FIXME: remove on next major version release
  @inline def charOnly(trace: List[JsonError], in: OneCharReader, c: Char): Unit = {
    val got = in.readChar()
    if (got != c) error(s"'$c'", got, trace)
  }

  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match {
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' => true
      case _                                                                                       => false
    }

  def readChars(trace: List[JsonError], in: OneCharReader, expect: Array[Char], errMsg: String): Unit = {
    var i: Int = 0
    while (i < expect.length) {
      if (in.readChar() != expect(i)) error(s"expected '$errMsg'", trace)
      i += 1
    }
  }
}

// A data structure encoding a simple algorithm for Trie pruning: Given a list
// of strings, and a sequence of incoming characters, find the strings that
// match, by manually maintaining a bitset. Empty strings are not allowed.
final class StringMatrix(names: Array[String], aliases: Array[(String, Int)] = Array.empty) {
  val namesLen: Int            = names.length
  private[this] val width: Int = namesLen + aliases.length
  val initial: Long            = -1L >>> (64 - width)
  private[this] val lengths: Array[Int] = {
    require(namesLen > 0 && width <= 64)
    val ls     = new Array[Int](width)
    var string = 0
    while (string < namesLen) {
      val l = names(string).length
      if (l == 0) require(false)
      ls(string) = l
      string += 1
    }
    while (string < ls.length) {
      val l = aliases(string - namesLen)._1.length
      if (l == 0) require(false)
      ls(string) = l
      string += 1
    }
    ls
  }
  private[this] val height: Int = lengths.max
  private[this] val matrix: Array[Char] = {
    val w      = width
    val m      = new Array[Char](height * w)
    var string = 0
    while (string < w) {
      val s =
        if (string < namesLen) names(string)
        else aliases(string - namesLen)._1
      val len        = s.length
      var char, base = 0
      while (char < len) {
        m(base + string) = s.charAt(char)
        base += w
        char += 1
      }
      string += 1
    }
    m
  }
  private[this] val resolvers: Array[Byte] = {
    val rs     = new Array[Byte](width)
    var string = 0
    while (string < namesLen) {
      rs(string) = string.toByte
      string += 1
    }
    while (string < rs.length) {
      val x = aliases(string - namesLen)._2
      if (x < 0 || x > namesLen) require(false)
      rs(string) = x.toByte
      string += 1
    }
    rs
  }

  // must be called with increasing `char` (starting with bitset obtained from a
  // call to 'initial', char = 0)
  def update(bitset: Long, char: Int, c: Char): Long =
    if (char < height) {
      val w      = width
      val m      = matrix
      val base   = char * w
      var latest = bitset
      if (initial == bitset) { // special case when it is dense since it is simple
        var string = 0
        while (string < w) {
          if (m(base + string) != c) latest ^= 1L << string
          string += 1
        }
      } else {
        var remaining = bitset
        while (remaining != 0L) {
          val string = java.lang.Long.numberOfTrailingZeros(remaining)
          val bit    = 1L << string
          remaining ^= bit
          if (m(base + string) != c) latest ^= bit
        }
      }
      latest
    } else 0L // too long

  // excludes entries that are not the given exact length
  def exact(bitset: Long, length: Int): Long =
    if (length <= height) {
      var remaining, latest = bitset
      val ls                = lengths
      while (remaining != 0L) {
        val string = java.lang.Long.numberOfTrailingZeros(remaining)
        val bit    = 1L << string
        remaining ^= bit
        if (ls(string) != length) latest ^= bit
      }
      latest
    } else 0L // too long

  def first(bitset: Long): Int =
    if (bitset != 0L) resolvers(java.lang.Long.numberOfTrailingZeros(bitset)).toInt // never returns 64
    else -1
}
