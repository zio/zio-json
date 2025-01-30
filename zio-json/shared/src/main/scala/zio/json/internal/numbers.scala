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
    val n = int__(in, consume)
    if (n < -128 || n > 127) throw UnsafeNumber
    n.toByte
  }

  def short(num: String): Short =
    short_(new FastStringReader(num), true)

  def short_(in: OneCharReader, consume: Boolean): Short = {
    val n = int__(in, consume)
    if (n < -32768 || n > 32767) throw UnsafeNumber
    n.toShort
  }

  def int(num: String): Int =
    int_(new FastStringReader(num), true)

  def int_(in: OneCharReader, consume: Boolean): Int =
    int__(in, consume)

  def long(num: String): Long =
    long_(new FastStringReader(num), true)

  def long_(in: OneCharReader, consume: Boolean): Long =
    long__(in, consume)

  def bigInteger(num: String, max_bits: Int): java.math.BigInteger =
    bigInteger_(new FastStringReader(num), true, max_bits)

  def bigInteger_(
    in: OneCharReader,
    consume: Boolean,
    max_bits: Int
  ): java.math.BigInteger = {
    var current =
      if (consume) in.readChar()
      else in.nextNonWhitespace()
    val negative = current == '-'
    if (negative) current = in.readChar()
    bigDecimal__(in, consume, negative, current, true, max_bits).unscaledValue
  }

  @inline def int__(in: OneCharReader, consume: Boolean): Int = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negative = current == '-'
    if (negative) current = in.readChar().toInt
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
    if (negative) accum
    else if (accum != -2147483648) -accum
    else throw UnsafeNumber
  }

  @inline def long__(in: OneCharReader, consume: Boolean): Long = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negative = current == '-'
    if (negative) current = in.readChar().toInt
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
    if (negative) accum
    else if (accum != -9223372036854775808L) -accum
    else throw UnsafeNumber
  }

  def float(num: String, max_bits: Int): Float =
    float_(new FastStringReader(num), true, max_bits)

  def float_(in: OneCharReader, consume: Boolean, max_bits: Int): Float = {
    var current =
      if (consume) in.readChar()
      else in.nextNonWhitespace()
    if (current == 'N') {
      readAll(in, "aN", consume)
      return Float.NaN
    }
    val negative = current == '-'
    if (negative) current = in.readChar()
    if (current == 'I' || current == '+') {
      if (current == '+') {
        current = in.readChar()
        if (current != 'I') throw UnsafeNumber
      }
      readAll(in, "nfinity", consume)
      if (negative) return Float.NegativeInfinity
      else return Float.PositiveInfinity
    }
    val res = bigDecimal__(in, consume, negative = negative, initial = current, int_only = false, max_bits = max_bits)
    if (negative && res.unscaledValue == java.math.BigInteger.ZERO) -0.0f
    else res.floatValue
  }

  def double(num: String, max_bits: Int): Double =
    double_(new FastStringReader(num), true, max_bits)

  def double_(in: OneCharReader, consume: Boolean, max_bits: Int): Double = {
    var current =
      if (consume) in.readChar()
      else in.nextNonWhitespace()
    if (current == 'N') {
      readAll(in, "aN", consume)
      return Double.NaN
    }
    val negative = current == '-'
    if (negative) current = in.readChar()
    if (current == 'I' || current == '+') {
      if (current == '+') {
        current = in.readChar()
        if (current != 'I') throw UnsafeNumber
      }
      readAll(in, "nfinity", consume)
      if (negative) return Double.NegativeInfinity
      else return Double.PositiveInfinity
    }
    // we could avoid going via BigDecimal if we wanted to do something like
    // https://github.com/plokhotnyuk/jsoniter-scala/blob/56ff2a60e28aa27bd4788caf3b1557a558c00fa1/jsoniter-scala-core/jvm/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/core/JsonReader.scala#L1395-L1425
    // based on
    // https://www.reddit.com/r/rust/comments/a6j5j1/making_rust_float_parsing_fast_and_correct
    //
    // the fallback of .doubleValue tends to call out to parseDouble which
    // ultimately uses strtod from the system libraries and they may loop until
    // the answer converges
    // https://github.com/rust-lang/rust/pull/27307/files#diff-fe6c36003393c49bf7e5c413458d6d9cR43-R84
    val res = bigDecimal__(in, consume, negative, current, false, max_bits)
    // BigDecimal doesn't have a negative zero, so we need to apply manually
    if (negative && res.unscaledValue == java.math.BigInteger.ZERO) -0.0
    // TODO implement Algorithm M or Bigcomp and avoid going via BigDecimal
    else res.doubleValue
  }

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

  def bigDecimal(num: String, max_bits: Int): java.math.BigDecimal =
    bigDecimal_(new FastStringReader(num), true, max_bits)

  def bigDecimal_(
    in: OneCharReader,
    consume: Boolean,
    max_bits: Int
  ): java.math.BigDecimal = {
    var current =
      if (consume) in.readChar()
      else in.nextNonWhitespace()
    val negative = current == '-'
    if (negative) current = in.readChar()
    bigDecimal__(in, consume, negative, current, false, max_bits)
  }

  def bigDecimal__(
    in: OneCharReader,
    consume: Boolean,
    negative: Boolean,
    initial: Char,
    int_only: Boolean,
    max_bits: Int
  ): java.math.BigDecimal = {
    var current: Int = initial.toInt
    // record the significand as Long until it overflows, then swap to BigInteger
    var sig: Long                   = -1   // -1 means it hasn't been seen yet
    var sig_ : java.math.BigInteger = null // non-null wins over sig
    var dot: Int                    = 0    // counts from the right
    var exp: Int                    = 0    // implied

    // skip trailing zero on the left
    while (current == '0') {
      sig = 0
      current = in.read()
      if (current == -1)
        return java.math.BigDecimal.ZERO
    }

    while ('0' <= current && current <= '9') {
      val digit = current - '0'
      if (sig_ != null) {
        sig_ = sig_.multiply(java.math.BigInteger.TEN).add(bigIntegers(digit))
        if (sig_.bitLength >= max_bits) throw UnsafeNumber
      } else if (sig >= 922337203685477580L)
        sig_ = java.math.BigInteger.valueOf(sig).multiply(java.math.BigInteger.TEN).add(bigIntegers(digit))
      else if (sig < 0) sig = digit.toLong
      else sig = (sig << 3) + (sig << 1) + digit
      current = in.read()
      if (current == -1)
        return significand(sig, sig_, negative, 0)
    }

    if (int_only) {
      if (consume && current != -1) throw UnsafeNumber
      return significand(sig, sig_, negative, 0)
    }

    if (current == '.') {
      if (sig < 0) sig = 0 // e.g. ".1" is shorthand for "0.1"
      current = in.read()
      if (current == -1)
        return significand(sig, sig_, negative, 0)
      while ('0' <= current && current <= '9') {
        dot += 1
        if (sig > 0 || current != '0') {
          val digit = current - '0'
          if (sig_ != null) {
            sig_ = sig_.multiply(java.math.BigInteger.TEN).add(bigIntegers(digit))
            if (sig_.bitLength >= max_bits) throw UnsafeNumber
          } else if (sig >= 922337203685477580L)
            sig_ = java.math.BigInteger.valueOf(sig).multiply(java.math.BigInteger.TEN).add(bigIntegers(digit))
          else if (sig < 0) sig = digit.toLong
          else sig = (sig << 3) + (sig << 1) + digit
        }
        // overflowed...
        if (dot < 0) throw UnsafeNumber
        current = in.read()
      }
    }

    if (sig < 0) throw UnsafeNumber // no significand

    if (current == 'E' || current == 'e') {
      current = in.read()
      val negativeExp = current == '-'
      if (negativeExp || current == '+') current = in.read()
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
      if (consume && current != -1) throw UnsafeNumber
      if (negativeExp) {
      } else if (exp != -2147483648) exp = -exp
      else throw UnsafeNumber
    } else if (consume && current != -1) throw UnsafeNumber

    significand(sig, sig_, negative, {
      if (dot < 1) -exp
      else dot - exp
    })
  }

  private[this] def significand(sig: Long, sig_ : java.math.BigInteger, negative: Boolean, scale: Int): java.math.BigDecimal =
    if (sig <= 0) java.math.BigDecimal.ZERO
    else if (sig_ != null) {
      new java.math.BigDecimal({
        if (negative) sig_.negate
        else sig_
      }, scale)
    } else java.math.BigDecimal.valueOf({
      if (negative) -sig
      else sig
    }, scale)

  // note that bigDecimal does not have a negative zero
  private[this] val bigIntegers: Array[java.math.BigInteger] =
    (0L to 9L).map(java.math.BigInteger.valueOf).toArray
}
