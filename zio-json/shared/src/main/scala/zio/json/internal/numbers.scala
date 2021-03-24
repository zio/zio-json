package zio.json.internal

import java.io._

import scala.util.control.NoStackTrace

/**
 * Total, fast, number parsing.
 *
 * The Java and Scala standard libraries throw exceptions when we attempt to
 * parse an invalid number. Unfortunately, exceptions are very expensive, and
 * untrusted data can be maliciously constructed to DOS a server.
 *
 * This suite of functions mitigates against such attacks by building up the
 * numbers one character at a time, which has been shown through extensive
 * benchmarking to be orders of magnitude faster than exception-throwing stdlib
 * parsers, for valid and invalid inputs. This approach, proposed by alexknvl,
 * was also benchmarked against regexp-based pre-validation.
 *
 * Note that although the behaviour is identical to the Java stdlib when given
 * the canonical form of a primitive (i.e. the .toString) of a number there may
 * be differences in behaviour for non-canonical forms. e.g. the Java stdlib
 * may reject "1.0" when parsed as an `BigInteger` but we may parse it as a
 * `1`, although "1.1" would be rejected. Parsing of `BigDecimal` preserves the
 * trailing zeros on the right but not on the left, e.g. "000.00001000" will be
 * "1.000e-5", which is useful in cases where the trailing zeros denote
 * measurement accuracy.
 *
 * `BigInteger`, `BigDecimal`, `Float` and `Double` have a configurable bit
 * limit on the size of the significand, to avoid OOM style attacks, which is
 * 128 bits by default.
 *
 * Results are contained in a specialisation of Option that avoids boxing.
 */
// TODO hex radix
// TODO octal radix
object SafeNumbers {
  import UnsafeNumbers.UnsafeNumber

  def byte(num: String): ByteOption =
    try ByteSome(UnsafeNumbers.byte(num))
    catch { case UnsafeNumber => ByteNone }

  def short(num: String): ShortOption =
    try ShortSome(UnsafeNumbers.short(num))
    catch { case UnsafeNumber => ShortNone }

  def int(num: String): IntOption =
    try IntSome(UnsafeNumbers.int(num))
    catch { case UnsafeNumber => IntNone }

  def long(num: String): LongOption =
    try LongSome(UnsafeNumbers.long(num))
    catch { case UnsafeNumber => LongNone }

  def bigInteger(
    num: String,
    max_bits: Int = 128
  ): Option[java.math.BigInteger] =
    try Some(UnsafeNumbers.bigInteger(num, max_bits))
    catch { case UnsafeNumber => None }

  def float(num: String, max_bits: Int = 128): FloatOption =
    try FloatSome(UnsafeNumbers.float(num, max_bits))
    catch { case UnsafeNumber => FloatNone }

  def double(num: String, max_bits: Int = 128): DoubleOption =
    try DoubleSome(UnsafeNumbers.double(num, max_bits))
    catch { case UnsafeNumber => DoubleNone }

  def bigDecimal(
    num: String,
    max_bits: Int = 128
  ): Option[java.math.BigDecimal] =
    try Some(UnsafeNumbers.bigDecimal(num, max_bits))
    catch { case UnsafeNumber => None }

}

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
  def byte_(in: Reader, consume: Boolean): Byte =
    long__(in, Byte.MinValue, Byte.MaxValue, consume).toByte

  def short(num: String): Short =
    short_(new FastStringReader(num), true)
  def short_(in: Reader, consume: Boolean): Short =
    long__(in, Short.MinValue, Short.MaxValue, consume).toShort

  def int(num: String): Int =
    int_(new FastStringReader(num), true)
  def int_(in: Reader, consume: Boolean): Int =
    long__(in, Int.MinValue, Int.MaxValue, consume).toInt

  def long(num: String): Long =
    long_(new FastStringReader(num), true)
  def long_(in: Reader, consume: Boolean): Long =
    long__(in, Long.MinValue, Long.MaxValue, consume)

  def bigInteger(num: String, max_bits: Int): java.math.BigInteger =
    bigInteger_(new FastStringReader(num), true, max_bits)
  def bigInteger_(
    in: Reader,
    consume: Boolean,
    max_bits: Int
  ): java.math.BigInteger = {
    var current: Int = in.read()
    var negative     = false

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+')
      current = in.read()
    if (current == -1) throw UnsafeNumber

    bigDecimal__(in, consume, negative, current, true, max_bits).unscaledValue
  }

  // measured faster than Character.isDigit
  @inline private[this] def isDigit(i: Int): Boolean =
    '0' <= i && i <= '9'

  // is it worth keeping this custom long__ instead of using bigInteger since it
  // is approximately double the performance.
  def long__(in: Reader, lower: Long, upper: Long, consume: Boolean): Long = {
    var current: Int = 0

    current = in.read()
    if (current == -1) throw UnsafeNumber
    var negative = false
    if (current == '-') {
      negative = true
      current = in.read()
      if (current == -1) throw UnsafeNumber
    } else if (current == '+') {
      current = in.read()
      if (current == -1) throw UnsafeNumber
    }

    if (!isDigit(current))
      throw UnsafeNumber

    var accum: Long = 0L
    do {
      val c = current - '0'
      if (accum <= longunderflow)
        if (accum < longunderflow)
          throw UnsafeNumber
        else if (accum == longunderflow && c == 9)
          throw UnsafeNumber
      // count down, not up, because it is larger
      accum = accum * 10 - c // should never underflow
      current = in.read()
    } while (current != -1 && isDigit(current))

    if (consume && current != -1) throw UnsafeNumber

    if (negative)
      if (accum < lower || upper < accum) throw UnsafeNumber
      else accum
    else if (accum == Long.MinValue)
      throw UnsafeNumber
    else {
      accum = -accum
      if (accum < lower || upper < accum) throw UnsafeNumber
      else accum
    }
  }

  def float(num: String, max_bits: Int): Float =
    float_(new FastStringReader(num), true, max_bits)

  def float_(in: Reader, consume: Boolean, max_bits: Int): Float = {
    var current: Int = in.read()
    var negative     = false

    def readAll(s: String): Unit = {
      var i   = 0
      val len = s.length

      while (i < len) {
        current = in.read()
        if (current != s(i)) throw UnsafeNumber
        i += 1
      }

      current = in.read() // to be consistent read the terminator

      if (consume && current != -1)
        throw UnsafeNumber
    }

    if (current == 'N') {
      readAll("aN")
      return Float.NaN
    }

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+') {
      current = in.read()
    }

    if (current == 'I') {
      readAll("nfinity")

      if (negative) return Float.NegativeInfinity
      else return Float.PositiveInfinity
    }

    if (current == -1)
      throw UnsafeNumber

    val res = bigDecimal__(in, consume, negative = negative, initial = current, int_only = false, max_bits = max_bits)

    if (negative && res.unscaledValue == java.math.BigInteger.ZERO) -0.0f
    else res.floatValue
  }

  def double(num: String, max_bits: Int): Double =
    double_(new FastStringReader(num), true, max_bits)

  def double_(in: Reader, consume: Boolean, max_bits: Int): Double = {
    var current: Int = in.read()
    var negative     = false

    def readall(s: String): Unit = {
      var i   = 0
      val len = s.length
      while (i < len) {
        current = in.read()
        if (current != s(i)) throw UnsafeNumber
        i += 1
      }
      current = in.read() // to be consistent read the terminator
      if (consume && current != -1) throw UnsafeNumber
    }

    if (current == 'N') {
      readall("aN")
      return Double.NaN
    }

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+')
      current = in.read()

    if (current == 'I') {
      readall("nfinity")
      if (negative) return Double.NegativeInfinity
      else return Double.PositiveInfinity
    }

    if (current == -1) throw UnsafeNumber

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

  def bigDecimal(num: String, max_bits: Int): java.math.BigDecimal =
    bigDecimal_(new FastStringReader(num), true, max_bits)
  def bigDecimal_(
    in: Reader,
    consume: Boolean,
    max_bits: Int
  ): java.math.BigDecimal = {
    var current: Int = in.read()
    var negative     = false

    if (current == '-') {
      negative = true
      current = in.read()
    } else if (current == '+')
      current = in.read()
    if (current == -1) throw UnsafeNumber

    bigDecimal__(in, consume, negative, current, false, max_bits)
  }

  def bigDecimal__(
    in: Reader,
    consume: Boolean,
    negative: Boolean,
    initial: Int,
    int_only: Boolean,
    max_bits: Int
  ): java.math.BigDecimal = {
    var current: Int = initial
    // record the significand as Long until it overflows, then swap to BigInteger
    var sig: Long                   = -1   // -1 means it hasn't been seen yet
    var sig_ : java.math.BigInteger = null // non-null wins over sig
    var dot: Int                    = 0    // counts from the right
    var exp: Int                    = 0    // implied

    def advance(): Boolean = {
      current = in.read()
      current != -1
    }

    // skip trailing zero on the left
    while (current == '0') {
      sig = 0
      if (!advance())
        return java.math.BigDecimal.ZERO
    }

    def push_sig(): Unit = {
      val c = current - '0'
      // would be nice if there was a fused instruction...
      if (sig_ != null) {
        sig_ = sig_
          .multiply(java.math.BigInteger.TEN)
          .add(bigIntegers(c))
        // arbitrary limit on BigInteger size to avoid OOM attacks
        if (sig_.bitLength >= max_bits)
          throw UnsafeNumber
      } else if (sig >= longoverflow)
        sig_ = java.math.BigInteger
          .valueOf(sig)
          .multiply(java.math.BigInteger.TEN)
          .add(bigIntegers(c))
      else if (sig < 0) sig = c.toLong
      else sig = sig * 10 + c
    }

    def significand() =
      if (sig <= 0) java.math.BigDecimal.ZERO
      else {
        val res =
          if (sig_ != null)
            new java.math.BigDecimal(sig_)
          else
            new java.math.BigDecimal(sig)
        if (negative) res.negate else res
      }

    while (isDigit(current)) {
      push_sig()
      if (!advance())
        return significand()
    }

    if (int_only) {
      if (consume && current != -1)
        throw UnsafeNumber
      return significand()
    }

    if (current == '.') {
      if (sig < 0) sig = 0 // e.g. ".1" is shorthand for "0.1"
      if (!advance())
        return significand()
      while (isDigit(current)) {
        dot += 1
        if (sig > 0 || current != '0')
          push_sig()
        // overflowed...
        if (dot < 0) throw UnsafeNumber
        advance()
      }
    }

    if (sig < 0) throw UnsafeNumber // no significand

    if (current == 'E' || current == 'e')
      exp = int_(in, consume)
    else if (consume && current != -1)
      throw UnsafeNumber

    val scale = if (dot < 1) exp else exp - dot
    val res   = significand()
    if (scale != 0)
      res.scaleByPowerOfTen(scale)
    else
      res
  }
  // note that bigDecimal does not have a negative zero
  val bigIntegers: Array[java.math.BigInteger] =
    (0L to 9L).map(java.math.BigInteger.valueOf(_)).toArray
  val longunderflow: Long = Long.MinValue / 10L
  val longoverflow: Long  = Long.MaxValue / 10L

}
