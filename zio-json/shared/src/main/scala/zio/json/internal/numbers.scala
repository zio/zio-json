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

import scala.util.control.NoStackTrace

// specialised Options to avoid boxing. Prefer .isEmpty guarded access to .value
// for higher performance: pattern matching is slightly slower.

sealed abstract class ByteOption {
  def isEmpty: Boolean
  def value: Byte
}

case object ByteNone extends ByteOption {
  def isEmpty     = true
  def value: Byte = throw new java.util.NoSuchElementException
}

case class ByteSome(value: Byte) extends ByteOption {
  def isEmpty = false
}

sealed abstract class ShortOption {
  def isEmpty: Boolean
  def value: Short
}

case object ShortNone extends ShortOption {
  def isEmpty      = true
  def value: Short = throw new java.util.NoSuchElementException
}

case class ShortSome(value: Short) extends ShortOption {
  def isEmpty = false
}

sealed abstract class IntOption {
  def isEmpty: Boolean
  def value: Int
}

case object IntNone extends IntOption {
  def isEmpty    = true
  def value: Int = throw new java.util.NoSuchElementException
}

case class IntSome(value: Int) extends IntOption {
  def isEmpty = false
}

sealed abstract class LongOption {
  def isEmpty: Boolean
  def value: Long
}

case object LongNone extends LongOption {
  def isEmpty     = true
  def value: Long = throw new java.util.NoSuchElementException
}

case class LongSome(value: Long) extends LongOption {
  def isEmpty = false
}

sealed abstract class FloatOption {
  def isEmpty: Boolean
  def value: Float
}

case object FloatNone extends FloatOption {
  def isEmpty      = true
  def value: Float = throw new java.util.NoSuchElementException
}

case class FloatSome(value: Float) extends FloatOption {
  def isEmpty = false
}

sealed abstract class DoubleOption {
  def isEmpty: Boolean
  def value: Double
}

case object DoubleNone extends DoubleOption {
  def isEmpty       = true
  def value: Double = throw new java.util.NoSuchElementException
}

case class DoubleSome(value: Double) extends DoubleOption {
  def isEmpty = false
}

// The underlying implementation uses an exception that has no stack trace for
// the failure case, which is 20x faster than retaining stack traces. Therefore,
// we require no boxing of the results on the happy path. This slows down the
// unhappy path a little bit, but it's still on the same order of magnitude as
// the happy path.
//
// This API should only be used by people who know what they are doing. Note
// that Reader implementations consume one character beyond the number that is
// parsed, because there is no terminator character.
object UnsafeNumbers {

  // should never escape into user code
  case object UnsafeNumber
      extends Exception(
        "if you see this a dev made a mistake using UnsafeNumbers"
      )
      with NoStackTrace

  def byte(num: String): Byte =
    byte_(new FastStringReader(num), true)

  def byte_(in: OneCharReader, consume: Boolean): Byte = {
    val n = int_(in, consume)
    if (n < -128 || n > 127) throw UnsafeNumber
    n.toByte
  }

  def short(num: String): Short =
    short_(new FastStringReader(num), true)

  def short_(in: OneCharReader, consume: Boolean): Short = {
    val n = int_(in, consume)
    if (n < -32768 || n > 32767) throw UnsafeNumber
    n.toShort
  }

  def int(num: String): Int =
    int_(new FastStringReader(num), true)

  def int_(in: OneCharReader, consume: Boolean): Int = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    if (current < '0' || current > '9') throw UnsafeNumber
    var accum = '0' - current
    while ({
      current = in.read()
      '0' <= current && current <= '9'
    }) {
      if (
        accum < -214748364 || {
          accum = accum * 10 + ('0' - current)
          accum > 0
        }
      ) throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (negate) accum
    else if (accum != -2147483648) -accum
    else throw UnsafeNumber
  }

  def long(num: String): Long =
    long_(new FastStringReader(num), true)

  def long_(in: OneCharReader, consume: Boolean): Long = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    if (current < '0' || current > '9') throw UnsafeNumber
    var accum = ('0' - current).toLong
    while ({
      current = in.read()
      '0' <= current && current <= '9'
    }) {
      if (
        accum < -922337203685477580L || {
          accum = (accum << 3) + (accum << 1) + ('0' - current)
          accum > 0
        }
      ) throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (negate) accum
    else if (accum != -9223372036854775808L) -accum
    else throw UnsafeNumber
  }

  def bigInteger(num: String, max_bits: Int): java.math.BigInteger =
    bigInteger_(new FastStringReader(num), true, max_bits)

  def bigInteger_(in: OneCharReader, consume: Boolean, max_bits: Int): java.math.BigInteger = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    if (current < '0' || current > '9') throw UnsafeNumber
    var bigSig: java.math.BigInteger = null
    var sig                          = (current - '0').toLong
    while ({
      current = in.read()
      '0' <= current && current <= '9'
    }) {
      if (sig < 922337203685477580L) sig = (sig << 3) + (sig << 1) + (current - '0')
      else {
        if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
        bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
        if (bigSig.bitLength >= max_bits) throw UnsafeNumber
      }
    }
    if (consume && current != -1) throw UnsafeNumber
    if (bigSig eq null) {
      if (negate) sig = -sig
      return java.math.BigInteger.valueOf(sig)
    }
    if (negate) bigSig = bigSig.negate
    bigSig
  }

  def float(num: String, max_bits: Int): Float =
    float_(new FastStringReader(num), true, max_bits)

  def float_(in: OneCharReader, consume: Boolean, max_bits: Int): Float = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    if (current == 'N') {
      readAll(in, "aN", consume)
      return Float.NaN
    }
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    if (current == 'I' || current == '+') {
      if (current == '+') {
        current = in.readChar().toInt
        if (current != 'I') throw UnsafeNumber
      }
      readAll(in, "nfinity", consume)
      return if (negate) Float.NegativeInfinity else Float.PositiveInfinity
    }
    var sig                          = -1L
    var bigSig: java.math.BigInteger = null
    if ('0' <= current && current <= '9') {
      sig = (current - '0').toLong
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        if (sig < 922337203685477580L) sig = (sig << 3) + (sig << 1) + (current - '0')
        else {
          if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
          bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
          if (bigSig.bitLength >= max_bits) throw UnsafeNumber
        }
      }
    }
    var scale, exp = 0
    if (current == '.') {
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        scale += 1
        if (sig < 922337203685477580L) {
          if (sig < 0) sig = (current - '0').toLong
          else sig = (sig << 3) + (sig << 1) + (current - '0')
        } else {
          if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
          bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
          if (bigSig.bitLength >= max_bits) throw UnsafeNumber
        }
      }
    }
    if (sig < 0) throw UnsafeNumber
    if ((current | 0x20) == 'e') {
      current = in.readChar().toInt
      val negateExp = current == '-'
      if (negateExp || current == '+') current = in.readChar().toInt
      if (current < '0' || current > '9') throw UnsafeNumber
      exp = '0' - current
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        if (
          exp < -214748364 || {
            exp = exp * 10 + ('0' - current)
            exp > 0
          }
        ) throw UnsafeNumber
      }
      if (negateExp) {}
      else if (exp != -2147483648) exp = -exp
      else throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (sig == 0) {
      return if (negate) -0.0f else 0.0f
    } else if (bigSig eq null) {
      if (negate) sig = -sig
      return java.math.BigDecimal.valueOf(sig, scale - exp).floatValue()
    }
    if (negate) bigSig = bigSig.negate
    new java.math.BigDecimal(bigSig, scale - exp).floatValue()
  }

  def double(num: String, max_bits: Int): Double =
    double_(new FastStringReader(num), true, max_bits)

  def double_(in: OneCharReader, consume: Boolean, max_bits: Int): Double = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    if (current == 'N') {
      readAll(in, "aN", consume)
      return Double.NaN
    }
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    if (current == 'I' || current == '+') {
      if (current == '+') {
        current = in.readChar().toInt
        if (current != 'I') throw UnsafeNumber
      }
      readAll(in, "nfinity", consume)
      return if (negate) Double.NegativeInfinity else Double.PositiveInfinity
    }
    var sig                          = -1L
    var bigSig: java.math.BigInteger = null
    if ('0' <= current && current <= '9') {
      sig = (current - '0').toLong
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        if (sig < 922337203685477580L) sig = (sig << 3) + (sig << 1) + (current - '0')
        else {
          if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
          bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
          if (bigSig.bitLength >= max_bits) throw UnsafeNumber
        }
      }
    }
    var scale, exp = 0
    if (current == '.') {
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        scale += 1
        if (sig < 922337203685477580L) {
          if (sig < 0) sig = (current - '0').toLong
          else sig = (sig << 3) + (sig << 1) + (current - '0')
        } else {
          if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
          bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
          if (bigSig.bitLength >= max_bits) throw UnsafeNumber
        }
      }
    }
    if (sig < 0) throw UnsafeNumber
    if ((current | 0x20) == 'e') {
      current = in.readChar().toInt
      val negateExp = current == '-'
      if (negateExp || current == '+') current = in.readChar().toInt
      if (current < '0' || current > '9') throw UnsafeNumber
      exp = '0' - current
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        if (
          exp < -214748364 || {
            exp = exp * 10 + ('0' - current)
            exp > 0
          }
        ) throw UnsafeNumber
      }
      if (negateExp) {}
      else if (exp != -2147483648) exp = -exp
      else throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (sig == 0) {
      return if (negate) -0.0 else 0.0
    } else if (bigSig eq null) {
      if (negate) sig = -sig
      return java.math.BigDecimal.valueOf(sig, scale - exp).doubleValue()
    }
    if (negate) bigSig = bigSig.negate
    new java.math.BigDecimal(bigSig, scale - exp).doubleValue()
  }

  def bigDecimal(num: String, max_bits: Int): java.math.BigDecimal =
    bigDecimal_(new FastStringReader(num), true, max_bits)

  def bigDecimal_(in: OneCharReader, consume: Boolean, max_bits: Int): java.math.BigDecimal = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    var bigSig: java.math.BigInteger = null
    var sig                          = -1L
    if ('0' <= current && current <= '9') {
      sig = (current - '0').toLong
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        if (sig < 922337203685477580L) sig = (sig << 3) + (sig << 1) + (current - '0')
        else {
          if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
          bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
          if (bigSig.bitLength >= max_bits) throw UnsafeNumber
        }
      }
    }
    var scale, exp = 0
    if (current == '.') {
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        scale += 1
        if (sig < 922337203685477580L) {
          if (sig < 0) sig = (current - '0').toLong
          else sig = (sig << 3) + (sig << 1) + (current - '0')
        } else {
          if (bigSig eq null) bigSig = java.math.BigInteger.valueOf(sig)
          bigSig = bigSig.multiply(java.math.BigInteger.TEN).add(bigIntegers(current - '0'))
          if (bigSig.bitLength >= max_bits) throw UnsafeNumber
        }
      }
    }
    if (sig < 0) throw UnsafeNumber
    if ((current | 0x20) == 'e') {
      current = in.readChar().toInt
      val negateExp = current == '-'
      if (negateExp || current == '+') current = in.readChar().toInt
      if (current < '0' || current > '9') throw UnsafeNumber
      exp = '0' - current
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        if (
          exp < -214748364 || {
            exp = exp * 10 + ('0' - current)
            exp > 0
          }
        ) throw UnsafeNumber
      }
      if (negateExp) {}
      else if (exp != -2147483648) exp = -exp
      else throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (bigSig eq null) {
      if (negate) sig = -sig
      return java.math.BigDecimal.valueOf(sig, scale - exp)
    }
    if (negate) bigSig = bigSig.negate
    new java.math.BigDecimal(bigSig, scale - exp)
  }

  @noinline
  private[this] def readAll(in: OneCharReader, s: String, consume: Boolean): Unit = {
    val len = s.length
    var i   = 0
    while (i < len) {
      if (in.readChar() != s.charAt(i)) throw UnsafeNumber
      i += 1
    }
    val current = in.read() // to be consistent read the terminator
    if (consume && current != -1) throw UnsafeNumber
  }

  // note that bigDecimal does not have a negative zero
  private[this] val bigIntegers: Array[java.math.BigInteger] =
    (0L to 9L).map(java.math.BigInteger.valueOf).toArray
}
