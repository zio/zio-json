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

import scala.annotation._

// tries to stick to the spec, but maybe a bit loose in places (e.g. numbers)
//
// https://www.json.org/json-en.html
object Lexer {
  // TODO need a variant that doesn't skip whitespace, so that attack vectors
  // consisting of an infinite stream of space can exit early.

  val NumberMaxBits: Int = 256

  @noinline
  def error(msg: String, trace: List[JsonError]): Nothing =
    throw UnsafeJson(JsonError.Message(msg) :: trace)

  @noinline
  private[json] def error(expected: String, got: Char, trace: List[JsonError]): Nothing =
    throw UnsafeJson(JsonError.Message(s"expected $expected got '$got'") :: trace)

  @noinline
  private[json] def error(c: Char, trace: List[JsonError]): Nothing =
    error(s"invalid '\\$c' in string", trace)

  // True if we got a string (implies a retraction), False for }
  def firstField(trace: List[JsonError], in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case '"' =>
        in.retract()
        true
      case '}' => false
      case c   => error("string or '}'", c, trace)
    }

  // True if we got a comma, and False for }
  def nextField(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case '}' => false
      case c   => error("',' or '}'", c, trace)
    }

  // True if we got anything besides a ], False for ]
  def firstArrayElement(in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ']' => false
      case _ =>
        in.retract()
        true
    }

  def nextArrayElement(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case ']' => false
      case c   => error("',' or ']'", c, trace)
    }

  // avoids allocating lots of strings (they are often the bulk of incoming
  // messages) by only checking for what we expect to see (Jon Pretty's idea).
  //
  // returns the index of the matched field, or -1
  def field(
    trace: List[JsonError],
    in: OneCharReader,
    matrix: StringMatrix
  ): Int = {
    val f = enumeration(trace, in, matrix)
    char(trace, in, ':')
    f
  }

  def enumeration(
    trace: List[JsonError],
    in: OneCharReader,
    matrix: StringMatrix
  ): Int = {
    var c = in.nextNonWhitespace()
    if (c != '"') error("'\"'", c, trace)
    var bs = matrix.initial
    var i  = 0
    while ({
      c = in.readChar()
      c != '"'
    }) {
      if (c == '\\') {
        (in.readChar(): @switch) match {
          case '"'  => c = '"'
          case '\\' => c = '\\'
          case '/'  => c = '/'
          case 'b'  => c = '\b'
          case 'f'  => c = '\f'
          case 'n'  => c = '\n'
          case 'r'  => c = '\r'
          case 't'  => c = '\t'
          case 'u'  => c = nextHex4(trace, in)
          case _    => error(c, trace)
        }
      } else if (c < ' ') error("invalid control in string", trace)
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    matrix.first(bs)
  }

  def skipValue(trace: List[JsonError], in: RetractReader): Unit =
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
    while (isNumber(in.readChar())) {}
    in.retract()
  }

  def skipString(trace: List[JsonError], in: OneCharReader): Unit =
    skipString(in, evenBackSlashes = true)

  @tailrec
  private def skipFixedChars(in: OneCharReader, n: Int): Unit =
    if (n > 0) {
      in.readChar()
      skipFixedChars(in, n - 1)
    }

  @tailrec
  private def skipString(in: OneCharReader, evenBackSlashes: Boolean): Unit =
    if (evenBackSlashes) {
      val ch = in.readChar()
      if (ch != '"') skipString(in, ch != '\\')
    } else skipString(in, evenBackSlashes = true)

  @tailrec
  private def skipObject(in: OneCharReader, level: Int): Unit = {
    val ch = in.readChar()
    if (ch == '"') {
      skipString(in, evenBackSlashes = true)
      skipObject(in, level)
    } else if (ch == '{') skipObject(in, level + 1)
    else if (ch != '}') skipObject(in, level)
    else if (level != 0) skipObject(in, level - 1)
  }

  @tailrec
  private def skipArray(in: OneCharReader, level: Int): Unit = {
    val b = in.readChar()
    if (b == '"') {
      skipString(in, evenBackSlashes = true)
      skipArray(in, level)
    } else if (b == '[') skipArray(in, level + 1)
    else if (b != ']') skipArray(in, level)
    else if (level != 0) skipArray(in, level - 1)
  }

  // useful for embedded documents, e.g. CSV contained inside JSON
  def streamingString(
    trace: List[JsonError],
    in: OneCharReader
  ): java.io.Reader = {
    char(trace, in, '"')
    new OneCharReader {
      def close(): Unit = in.close()

      private[this] var escaped = false

      @tailrec
      override def read(): Int = {
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
            case 'u'              => Lexer.nextHex4(trace, in)
            case _                => Lexer.error(c, trace)
          }).toInt
        } else if (c == '\\') {
          escaped = true
          read()
        } else if (c == '"') -1 // this is the EOS for the caller
        else if (c < ' ') Lexer.error("invalid control in string", trace)
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
    val sb = new FastStringBuilder(64)
    while ({
      c = in.readChar()
      c != '"'
    }) {
      if (c == '\\') {
        (in.readChar(): @switch) match {
          case '"'  => c = '"'
          case '\\' => c = '\\'
          case '/'  => c = '/'
          case 'b'  => c = '\b'
          case 'f'  => c = '\f'
          case 'n'  => c = '\n'
          case 'r'  => c = '\r'
          case 't'  => c = '\t'
          case 'u'  => c = nextHex4(trace, in)
          case _    => error(c, trace)
        }
      } else if (c < ' ') error("invalid control in string", trace)
      sb.append(c)
    }
    sb.buffer
  }

  // consumes 4 hex characters after current
  @noinline
  def nextHex4(trace: List[JsonError], in: OneCharReader): Char = {
    var i, accum = 0
    while (i < 4) {
      val c = in.readChar()
      accum <<= 4
      accum += {
        if ('0' <= c && c <= '9') c - '0'
        else if ('A' <= c && c <= 'F') c - 'A' + 10
        else if ('a' <= c && c <= 'f') c - 'a' + 10
        else error("invalid charcode in string", trace)
      }
      i += 1
    }
    accum.toChar
  }

  def boolean(trace: List[JsonError], in: OneCharReader): Boolean = {
    val c1 = in.nextNonWhitespace()
    val c2 = in.readChar()
    val c3 = in.readChar()
    val c4 = in.readChar()
    (c1: @switch) match {
      case 't' =>
        if (c2 != 'r' || c3 != 'u' || c4 != 'e') error(s"expected 'true'", trace)
        true
      case 'f' =>
        if (in.readChar() != 'e' || c2 != 'a' || c3 != 'l' || c4 != 's') error(s"expected 'false'", trace)
        false
      case c =>
        error("'true' or 'false'", c, trace)
    }
  }

  def byte(trace: List[JsonError], in: RetractReader): Byte = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.byte_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Byte", trace)
    }
  }

  def short(trace: List[JsonError], in: RetractReader): Short = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.short_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Short", trace)
    }
  }

  def int(trace: List[JsonError], in: RetractReader): Int = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.int_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected an Int", trace)
    }
  }

  def long(trace: List[JsonError], in: RetractReader): Long = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.long_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Long", trace)
    }
  }

  def bigInteger(
    trace: List[JsonError],
    in: RetractReader
  ): java.math.BigInteger = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.bigInteger_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error(s"expected a $NumberMaxBits bit BigInteger", trace)
    }
  }

  def float(trace: List[JsonError], in: RetractReader): Float = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.float_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Float", trace)
    }
  }

  def double(trace: List[JsonError], in: RetractReader): Double = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.double_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error("expected a Double", trace)
    }
  }

  def bigDecimal(
    trace: List[JsonError],
    in: RetractReader
  ): java.math.BigDecimal = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.bigDecimal_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => error(s"expected a $NumberMaxBits BigDecimal", trace)
    }
  }

  // really just a way to consume the whitespace
  private def checkNumber(trace: List[JsonError], in: RetractReader): Unit = {
    (in.nextNonWhitespace(): @switch) match {
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => ()
      case c                                                               => error("a number,", c, trace)
    }
    in.retract()
  }

  // optional whitespace and then an expected character
  @inline def char(trace: List[JsonError], in: OneCharReader, c: Char): Unit = {
    val got = in.nextNonWhitespace()
    if (got != c) error(s"'$c'", got, trace)
  }

  @inline def charOnly(
    trace: List[JsonError],
    in: OneCharReader,
    c: Char
  ): Unit = {
    val got = in.readChar()
    if (got != c) error(s"'$c'", got, trace)
  }

  // non-positional for performance
  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match {
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' =>
        true
      case _ => false
    }

  def readChars(
    trace: List[JsonError],
    in: OneCharReader,
    expect: Array[Char],
    errMsg: String
  ): Unit = {
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
