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

  // True if we got a string (implies a retraction), False for }
  def firstField(trace: List[JsonError], in: RetractReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case '"' =>
        in.retract()
        true
      case '}' => false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected string or '}' got '$c'") :: trace
        )
    }

  // True if we got a comma, and False for }
  def nextField(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case ',' => true
      case '}' => false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected ',' or '}' got '$c'") :: trace
        )
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
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected ',' or ']' got '$c'") :: trace
        )
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
    val stream = streamingString(trace, in)

    var i: Int   = 0
    var bs: Long = matrix.initial
    var c: Int   = -1
    while ({ c = stream.read(); c != -1 }) {
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    matrix.first(bs)
  }

  private[this] val ull: Array[Char]  = "ull".toCharArray
  private[this] val alse: Array[Char] = "alse".toCharArray
  private[this] val rue: Array[Char]  = "rue".toCharArray

  def skipValue(trace: List[JsonError], in: RetractReader): Unit =
    (in.nextNonWhitespace(): @switch) match {
      case 'n' => readChars(trace, in, ull, "null")
      case 'f' => readChars(trace, in, alse, "false")
      case 't' => readChars(trace, in, rue, "true")
      case '{' =>
        if (firstField(trace, in)) {
          while ({
            {
              char(trace, in, '"')
              skipString(trace, in)
              char(trace, in, ':')
              skipValue(trace, in)
            }; nextField(trace, in)
          }) ()
        }
      case '[' =>
        if (firstArrayElement(in)) {
          while ({ skipValue(trace, in); nextArrayElement(trace, in) }) ()
        }
      case '"' =>
        skipString(trace, in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        skipNumber(in)
      case c => throw UnsafeJson(JsonError.Message(s"unexpected '$c'") :: trace)
    }

  def skipNumber(in: RetractReader): Unit = {
    while (isNumber(in.readChar())) {}
    in.retract()
  }

  def skipString(trace: List[JsonError], in: OneCharReader): Unit = {
    val stream = new EscapedString(trace, in)
    var i: Int = 0
    while ({ i = stream.read(); i != -1 }) ()
  }

  // useful for embedded documents, e.g. CSV contained inside JSON
  def streamingString(
    trace: List[JsonError],
    in: OneCharReader
  ): java.io.Reader = {
    char(trace, in, '"')
    new EscapedString(trace, in)
  }

  def string(trace: List[JsonError], in: OneCharReader): CharSequence = {
    char(trace, in, '"')
    val stream = new EscapedString(trace, in)

    val sb = new FastStringBuilder(64)
    while (true) {
      val c = stream.read()
      if (c == -1)
        return sb.buffer // mutable thing escapes, but cannot be changed
      sb.append(c.toChar)
    }
    throw UnsafeJson(JsonError.Message("impossible string") :: trace)
  }

  def boolean(trace: List[JsonError], in: OneCharReader): Boolean =
    (in.nextNonWhitespace(): @switch) match {
      case 't' =>
        readChars(trace, in, rue, "true")
        true
      case 'f' =>
        readChars(trace, in, alse, "false")
        false
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected 'true' or 'false' got $c") :: trace
        )
    }

  def byte(trace: List[JsonError], in: RetractReader): Byte = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.byte_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Byte") :: trace)
    }
  }

  def short(trace: List[JsonError], in: RetractReader): Short = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.short_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Short") :: trace)
    }
  }

  def int(trace: List[JsonError], in: RetractReader): Int = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.int_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected an Int") :: trace)
    }
  }

  def long(trace: List[JsonError], in: RetractReader): Long = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.long_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Long") :: trace)
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
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message(s"expected a $NumberMaxBits bit BigInteger") :: trace)
    }
  }

  def float(trace: List[JsonError], in: RetractReader): Float = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.float_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Float") :: trace)
    }
  }

  def double(trace: List[JsonError], in: RetractReader): Double = {
    checkNumber(trace, in)
    try {
      val i = UnsafeNumbers.double_(in, false, NumberMaxBits)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message("expected a Double") :: trace)
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
      case UnsafeNumbers.UnsafeNumber =>
        throw UnsafeJson(JsonError.Message(s"expected a $NumberMaxBits BigDecimal") :: trace)
    }
  }

  // really just a way to consume the whitespace
  private def checkNumber(trace: List[JsonError], in: RetractReader): Unit = {
    (in.nextNonWhitespace(): @switch) match {
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => ()
      case c =>
        throw UnsafeJson(
          JsonError.Message(s"expected a number, got $c") :: trace
        )
    }
    in.retract()
  }

  // optional whitespace and then an expected character
  @inline def char(trace: List[JsonError], in: OneCharReader, c: Char): Unit = {
    val got = in.nextNonWhitespace()
    if (got != c)
      throw UnsafeJson(JsonError.Message(s"expected '$c' got '$got'") :: trace)
  }

  @inline def charOnly(
    trace: List[JsonError],
    in: OneCharReader,
    c: Char
  ): Unit = {
    val got = in.readChar()
    if (got != c)
      throw UnsafeJson(JsonError.Message(s"expected '$c' got '$got'") :: trace)
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
      if (in.readChar() != expect(i))
        throw UnsafeJson(JsonError.Message(s"expected '$errMsg'") :: trace)
      i += 1
    }
  }

}

// A Reader for the contents of a string, taking care of the escaping.
//
// `read` can throw extra exceptions on badly formed input.
private final class EscapedString(trace: List[JsonError], in: OneCharReader) extends java.io.Reader with OneCharReader {

  def close(): Unit = in.close()

  private[this] var escaped = false

  override def read(): Int = {
    val c = in.readChar()
    if (escaped) {
      escaped = false
      (c: @switch) match {
        case '"' | '\\' | '/' => c.toInt
        case 'b'              => '\b'.toInt
        case 'f'              => '\f'.toInt
        case 'n'              => '\n'.toInt
        case 'r'              => '\r'.toInt
        case 't'              => '\t'.toInt
        case 'u'              => nextHex4()
        case _ =>
          throw UnsafeJson(
            JsonError.Message(s"invalid '\\${c.toChar}' in string") :: trace
          )
      }
    } else if (c == '\\') {
      escaped = true
      read()
    } else if (c == '"') -1 // this is the EOS for the caller
    else if (c < ' ')
      throw UnsafeJson(JsonError.Message("invalid control in string") :: trace)
    else c.toInt
  }

  // callers expect to get an EOB so this is rare
  def readChar(): Char = {
    val v = read()
    if (v == -1) throw new UnexpectedEnd
    v.toChar
  }

  // consumes 4 hex characters after current
  def nextHex4(): Int = {
    var i: Int     = 0
    var accum: Int = 0
    while (i < 4) {
      var c: Int = in.read()
      if (c == -1)
        throw UnsafeJson(JsonError.Message("unexpected EOB in string") :: trace)
      c =
        if ('0' <= c && c <= '9') c - '0'
        else if ('A' <= c && c <= 'F') c - 'A' + 10
        else if ('a' <= c && c <= 'f') c - 'a' + 10
        else
          throw UnsafeJson(
            JsonError.Message("invalid charcode in string") :: trace
          )
      accum = accum * 16 + c
      i += 1
    }
    accum
  }

}

// A data structure encoding a simple algorithm for Trie pruning: Given a list
// of strings, and a sequence of incoming characters, find the strings that
// match, by manually maintaining a bitset. Empty strings are not allowed.
final class StringMatrix(val xs: Array[String], aliases: Array[(String, Int)] = Array.empty) {
  require(xs.forall(_.nonEmpty))
  require(xs.nonEmpty)
  require(aliases.forall(_._1.nonEmpty))
  require(aliases.forall(p => p._2 >= 0 && p._2 < xs.length))

  val width: Int = xs.length + aliases.length

  require(width <= 64)

  val lengths: Array[Int] = Array.tabulate[Int](width) { string =>
    if (string < xs.length) xs(string).length
    else aliases(string - xs.length)._1.length
  }
  val height: Int   = lengths.max
  val initial: Long = -1L >>> (64 - width)
  private val matrix: Array[Char] = {
    val m      = Array.fill[Char](width * height)(0xffff)
    var string = 0
    while (string < width) {
      val s =
        if (string < xs.length) xs(string)
        else aliases(string - xs.length)._1
      val len  = s.length
      var char = 0
      while (char < len) {
        m(width * char + string) = s.charAt(char)
        char += 1
      }
      string += 1
    }
    m
  }
  private val resolve: Array[Byte] = Array.tabulate[Byte](width) { string =>
    if (string < xs.length) string.toByte
    else aliases(string - xs.length)._2.toByte
  }

  // must be called with increasing `char` (starting with bitset obtained from a
  // call to 'initial', char = 0)
  def update(bitset: Long, char: Int, c: Int): Long =
    if (char >= height) 0L    // too long
    else if (bitset == 0L) 0L // everybody lost
    else {
      var latest = bitset
      val base   = width * char
      if (bitset == initial) { // special case when it is dense since it is simple
        var string = 0
        while (string < width) {
          if (matrix(base + string) != c) latest ^= 1L << string
          string += 1
        }
      } else {
        var remaining = bitset
        while (remaining != 0L) {
          val string = java.lang.Long.numberOfTrailingZeros(remaining)
          val bit    = 1L << string
          if (matrix(base + string) != c) latest ^= bit
          remaining ^= bit
        }
      }
      latest
    }

  // excludes entries that are not the given exact length
  def exact(bitset: Long, length: Int): Long =
    if (length > height) 0L // too long
    else {
      var latest    = bitset
      var remaining = bitset
      while (remaining != 0L) {
        val string = java.lang.Long.numberOfTrailingZeros(remaining)
        val bit    = 1L << string
        if (lengths(string) != length) latest ^= bit
        remaining ^= bit
      }
      latest
    }

  def first(bitset: Long): Int =
    if (bitset == 0L) -1
    else resolve(java.lang.Long.numberOfTrailingZeros(bitset)) // never returns 64
}
