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

import java.util.UUID
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
    in.nextNonWhitespace() != '}' && {
      in.retract()
      true
    }

  // True if we got a comma, and False for }
  @inline def nextField(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case '}' => false
      case c   => error("',' or '}'", c, trace)
    }

  // True if we got anything besides a ], False for ]
  @inline def firstArrayElement(in: RetractReader): Boolean =
    in.nextNonWhitespace() != ']' && {
      in.retract()
      true
    }

  @inline def nextArrayElement(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case ']' => false
      case c   => error("',' or ']'", c, trace)
    }

  @inline def field(trace: List[JsonError], in: OneCharReader, matrix: StringMatrix): Int = {
    val f = enumeration(trace, in, matrix)
    val c = in.nextNonWhitespace()
    if (c != ':') error("':'", c, trace)
    f
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
    bs = matrix.exact(bs, i)
    matrix.first(bs)
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
    if (c != '"') error("'\"'", c, trace)
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
    new String(cs, 0, i)
  }

  def uuid(trace: List[JsonError], in: OneCharReader): UUID = {
    var c = in.nextNonWhitespace()
    if (c != '"') error("'\"'", c, trace)
    var cs = charArrays.get
    var i  = 0
    while ({
      c = in.readChar()
      c != '"'
    }) {
      if (c == '\\') c = nextEscaped(trace, in)
      if (c > 0xff) uuidError(trace)
      if (i == cs.length) cs = java.util.Arrays.copyOf(cs, i << 1)
      cs(i) = c
      i += 1
    }
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
        ds(cs(0).toInt).toLong << 28 |
          (ds(cs(1).toInt) << 24 |
            ds(cs(2).toInt) << 20 |
            ds(cs(3).toInt) << 16 |
            ds(cs(4).toInt) << 12 |
            ds(cs(5).toInt) << 8 |
            ds(cs(6).toInt) << 4 |
            ds(cs(7).toInt))
      val msb2 =
        ds(cs(9).toInt) << 12 |
          ds(cs(10).toInt) << 8 |
          ds(cs(11).toInt) << 4 |
          ds(cs(12).toInt)
      val msb3 =
        ds(cs(14).toInt) << 12 |
          ds(cs(15).toInt) << 8 |
          ds(cs(16).toInt) << 4 |
          ds(cs(17).toInt)
      val lsb1 =
        ds(cs(19).toInt) << 12 |
          ds(cs(20).toInt) << 8 |
          ds(cs(21).toInt) << 4 |
          ds(cs(22).toInt)
      val lsb2 =
        (ds(cs(24).toInt) << 16 |
          ds(cs(25).toInt) << 12 |
          ds(cs(26).toInt) << 8 |
          ds(cs(27).toInt) << 4 |
          ds(cs(28).toInt)).toLong << 28 |
          (ds(cs(29).toInt) << 24 |
            ds(cs(30).toInt) << 20 |
            ds(cs(31).toInt) << 16 |
            ds(cs(32).toInt) << 12 |
            ds(cs(33).toInt) << 8 |
            ds(cs(34).toInt) << 4 |
            ds(cs(35).toInt))
      if ((msb1 | msb2 | msb3 | lsb1 | lsb2) >= 0L) {
        return new UUID(msb1 << 32 | msb2.toLong << 16 | msb3, lsb1.toLong << 48 | lsb2)
      }
    } else if (i <= 36) {
      return uuidExtended(trace, cs, i)
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
        result = (result << 4) | ds(cs(i).toInt)
        i += 1
      }
      if ((result & mask) == 0L) return result
    }
    uuidError(trace)
  }

  @noinline private[this] def uuidError(trace: List[JsonError]): Nothing = error("expected UUID string", trace)

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

  def boolean(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case 't' =>
        if (in.readChar() != 'r' || in.readChar() != 'u' || in.readChar() != 'e') {
          error("expected 'true'", trace)
        }
        true
      case 'f' =>
        if (in.readChar() != 'a' || in.readChar() != 'l' || in.readChar() != 's' || in.readChar() != 'e') {
          error("expected 'false'", trace)
        }
        false
      case c =>
        error("'true' or 'false'", c, trace)
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
final class StringMatrix(xs: Array[String], aliases: Array[(String, Int)] = Array.empty) {
  require(xs.nonEmpty)

  private[this] val width: Int = xs.length + aliases.length

  require(width <= 64)

  val initial: Long = -1L >>> (64 - width)

  private[this] val lengths: Array[Int] = {
    val ls     = new Array[Int](width)
    val xsLen  = xs.length
    var string = 0
    while (string < xsLen) {
      val l = xs(string).length
      if (l == 0) require(false)
      ls(string) = l
      string += 1
    }
    while (string < ls.length) {
      val l = aliases(string - xsLen)._1.length
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
    val xsLen  = xs.length
    var string = 0
    while (string < w) {
      val s =
        if (string < xsLen) xs(string)
        else aliases(string - xsLen)._1
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
    val xsLen  = xs.length
    var string = 0
    while (string < xsLen) {
      rs(string) = string.toByte
      string += 1
    }
    while (string < rs.length) {
      val x = aliases(string - xsLen)._2
      if (x < 0 || x > xsLen) require(false)
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
