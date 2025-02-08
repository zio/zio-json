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
      extends Exception("if you see this a dev made a mistake using UnsafeNumbers")
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
          accum = accum * 10 + ('0' - current)
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
    var loM10                       = (current - '0').toLong
    var loDigits                    = 1
    var hiM10: java.math.BigDecimal = null
    while ({
      current = in.read()
      '0' <= current && current <= '9'
    }) {
      loM10 = loM10 * 10 + (current - '0')
      loDigits += 1
      if (loM10 >= 100000000000000000L) {
        if (negate) loM10 = -loM10
        val bd = java.math.BigDecimal.valueOf(loM10)
        if (hiM10 eq null) hiM10 = bd
        else {
          hiM10 = hiM10.scaleByPowerOfTen(loDigits).add(bd)
          if (hiM10.unscaledValue.bitLength >= max_bits) throw UnsafeNumber
        }
        loM10 = 0
        loDigits = 0
      }
    }
    if (consume && current != -1) throw UnsafeNumber
    if (hiM10 eq null) {
      if (negate) loM10 = -loM10
      return java.math.BigInteger.valueOf(loM10)
    }
    if (loDigits != 0) {
      if (negate) loM10 = -loM10
      hiM10 = hiM10.scaleByPowerOfTen(loDigits).add(java.math.BigDecimal.valueOf(loM10))
      if (hiM10.unscaledValue.bitLength >= max_bits) throw UnsafeNumber
    }
    hiM10.unscaledValue
  }

  def bigDecimal(num: String, max_bits: Int): java.math.BigDecimal =
    bigDecimal_(new FastStringReader(num), true, max_bits)

  def bigDecimal_(in: OneCharReader, consume: Boolean, max_bits: Int): java.math.BigDecimal = {
    var current =
      if (consume) in.readChar().toInt
      else in.nextNonWhitespace().toInt
    val negate = current == '-'
    if (negate) current = in.readChar().toInt
    var loM10                       = 0L
    var loDigits                    = 0
    var hiM10: java.math.BigDecimal = null
    if ('0' <= current && current <= '9') {
      loM10 = (current - '0').toLong
      loDigits += 1
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        loM10 = loM10 * 10 + (current - '0')
        loDigits += 1
        if (loM10 >= 100000000000000000L) {
          hiM10 = toBigDecimal(hiM10, loM10, loDigits, 0, max_bits, negate)
          loM10 = 0
          loDigits = 0
        }
      }
    }
    var e10 = 0
    if (current == '.') {
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        loM10 = loM10 * 10 + (current - '0')
        loDigits += 1
        e10 -= 1
        if (loM10 >= 100000000000000000L) {
          hiM10 = toBigDecimal(hiM10, loM10, loDigits, 0, max_bits, negate)
          loM10 = 0
          loDigits = 0
        }
      }
    }
    if ((hiM10 eq null) && loDigits == 0) throw UnsafeNumber
    if ((current | 0x20) == 'e') {
      current = in.readChar().toInt
      val negateExp = current == '-'
      if (negateExp || current == '+') current = in.readChar().toInt
      if (current < '0' || current > '9') throw UnsafeNumber
      var exp = '0' - current
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
      if (negateExp) e10 += exp
      else if (exp != -2147483648) e10 -= exp
      else throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (hiM10 eq null) {
      if (negate) loM10 = -loM10
      return java.math.BigDecimal.valueOf(loM10, -e10)
    }
    toBigDecimal(hiM10, loM10, loDigits, e10, max_bits, negate)
  }

  @noinline private[this] def toBigDecimal(
    hi: java.math.BigDecimal,
    lo: Long,
    loDigits: Int,
    e10: Int,
    max_bits: Int,
    negate: Boolean
  ): java.math.BigDecimal = {
    var loM10 = lo
    if (negate) loM10 = -loM10
    val bd =
      if (loDigits != 0) java.math.BigDecimal.valueOf(loM10, -e10)
      else java.math.BigDecimal.ZERO
    if (hi eq null) return bd
    var hiM10 = hi
    val scale = loDigits + e10
    if (scale != 0) hiM10 = hiM10.scaleByPowerOfTen(scale)
    hiM10 = hiM10.add(bd)
    if (hiM10.unscaledValue.bitLength >= max_bits) throw UnsafeNumber
    hiM10
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
    var loM10                       = 0L
    var loDigits                    = 0
    var hiM10: java.math.BigDecimal = null
    if ('0' <= current && current <= '9') {
      loM10 = (current - '0').toLong
      loDigits += 1
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        loM10 = loM10 * 10 + (current - '0')
        loDigits += 1
        if (loM10 >= 100000000000000000L) {
          hiM10 = toBigDecimal(hiM10, loM10, loDigits, 0, max_bits, negate)
          loM10 = 0
          loDigits = 0
        }
      }
    }
    var e10 = 0
    if (current == '.') {
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        loM10 = loM10 * 10 + (current - '0')
        loDigits += 1
        e10 -= 1
        if (loM10 >= 100000000000000000L) {
          hiM10 = toBigDecimal(hiM10, loM10, loDigits, 0, max_bits, negate)
          loM10 = 0
          loDigits = 0
        }
      }
    }
    if ((hiM10 eq null) && loDigits == 0) throw UnsafeNumber
    if ((current | 0x20) == 'e') {
      current = in.readChar().toInt
      val negateExp = current == '-'
      if (negateExp || current == '+') current = in.readChar().toInt
      if (current < '0' || current > '9') throw UnsafeNumber
      var exp = '0' - current
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
      if (negateExp) e10 += exp
      else if (exp != -2147483648) e10 -= exp
      else throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (hiM10 eq null) {
      var x: Float =
        if (e10 == 0) loM10.toFloat
        else {
          if (loM10 < 4294967296L && e10 >= loDigits - 23 && e10 <= 19 - loDigits) {
            val pow10 = pow10Doubles
            (if (e10 < 0) loM10 / pow10(-e10)
             else loM10 * pow10(e10)).toFloat
          } else toFloat(loM10, e10)
        }
      if (negate) x = -x
      return x
    }
    toBigDecimal(hiM10, loM10, loDigits, e10, max_bits, negate).floatValue()
  }

  // Based on the 'Moderate Path' algorithm from the awesome library of Alexander Huszagh: https://github.com/Alexhuszagh/rust-lexical
  // Here is his inspiring post: https://www.reddit.com/r/rust/comments/a6j5j1/making_rust_float_parsing_fast_and_correct
  private[this] def toFloat(m10: Long, e10: Int): Float =
    if (m10 == 0 || e10 < -64) 0.0f
    else if (e10 >= 39) Float.PositiveInfinity
    else {
      var shift = java.lang.Long.numberOfLeadingZeros(m10)
      var m2    = unsignedMultiplyHigh(pow10Mantissas(e10 + 343), m10 << shift)
      var e2    = (e10 * 108853 >> 15) - shift + 1 // (e10 * Math.log(10) / Math.log(2)).toInt - shift + 1
      shift = java.lang.Long.numberOfLeadingZeros(m2)
      m2 <<= shift
      e2 -= shift
      val truncatedBitNum = Math.max(-149 - e2, 40)
      val savedBitNum     = 64 - truncatedBitNum
      val mask            = -1L >>> Math.max(savedBitNum, 0)
      val halfwayDiff     = (m2 & mask) - (mask >>> 1)
      if (Math.abs(halfwayDiff) > 1 || savedBitNum <= 0) java.lang.Float.intBitsToFloat {
        var mf = 0
        if (savedBitNum > 0) mf = (m2 >>> truncatedBitNum).toInt
        e2 += truncatedBitNum
        if (savedBitNum >= 0 && halfwayDiff > 0) {
          if (mf == 0xffffff) {
            mf = 0x800000
            e2 += 1
          } else mf += 1
        }
        if (e2 == -149) mf
        else if (e2 >= 105) 0x7f800000
        else e2 + 150 << 23 | mf & 0x7fffff
      }
      else java.math.BigDecimal.valueOf(m10, -e10).floatValue()
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
    var loM10                       = 0L
    var loDigits                    = 0
    var hiM10: java.math.BigDecimal = null
    if ('0' <= current && current <= '9') {
      loM10 = (current - '0').toLong
      loDigits += 1
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        loM10 = loM10 * 10 + (current - '0')
        loDigits += 1
        if (loM10 >= 100000000000000000L) {
          hiM10 = toBigDecimal(hiM10, loM10, loDigits, 0, max_bits, negate)
          loM10 = 0
          loDigits = 0
        }
      }
    }
    var e10 = 0
    if (current == '.') {
      while ({
        current = in.read()
        '0' <= current && current <= '9'
      }) {
        loM10 = loM10 * 10 + (current - '0')
        loDigits += 1
        e10 -= 1
        if (loM10 >= 100000000000000000L) {
          hiM10 = toBigDecimal(hiM10, loM10, loDigits, 0, max_bits, negate)
          loM10 = 0
          loDigits = 0
        }
      }
    }
    if ((hiM10 eq null) && loDigits == 0) throw UnsafeNumber
    if ((current | 0x20) == 'e') {
      current = in.readChar().toInt
      val negateExp = current == '-'
      if (negateExp || current == '+') current = in.readChar().toInt
      if (current < '0' || current > '9') throw UnsafeNumber
      var exp = '0' - current
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
      if (negateExp) e10 += exp
      else if (exp != -2147483648) e10 -= exp
      else throw UnsafeNumber
    }
    if (consume && current != -1) throw UnsafeNumber
    if (hiM10 eq null) {
      var x: Double =
        if (e10 == 0) loM10.toDouble
        else {
          if (loM10 < 4503599627370496L && e10 >= -22 && e10 <= 38 - loDigits) {
            val pow10 = pow10Doubles
            if (e10 < 0) loM10 / pow10(-e10)
            else if (e10 <= 22) loM10 * pow10(e10)
            else {
              val slop = 16 - loDigits
              (loM10 * pow10(slop)) * pow10(e10 - slop)
            }
          } else toDouble(loM10, e10)
        }
      if (negate) x = -x
      return x
    }
    toBigDecimal(hiM10, loM10, loDigits, e10, max_bits, negate).doubleValue()
  }

  // Based on the 'Moderate Path' algorithm from the awesome library of Alexander Huszagh: https://github.com/Alexhuszagh/rust-lexical
  // Here is his inspiring post: https://www.reddit.com/r/rust/comments/a6j5j1/making_rust_float_parsing_fast_and_correct
  @inline private[this] def toDouble(m10: Long, e10: Int): Double =
    if (m10 == 0 || e10 < -343) 0.0
    else if (e10 >= 310) Double.PositiveInfinity
    else {
      var shift = java.lang.Long.numberOfLeadingZeros(m10)
      var m2    = unsignedMultiplyHigh(pow10Mantissas(e10 + 343), m10 << shift)
      var e2    = (e10 * 108853 >> 15) - shift + 1 // (e10 * Math.log(10) / Math.log(2)).toInt - shift + 1
      shift = java.lang.Long.numberOfLeadingZeros(m2)
      m2 <<= shift
      e2 -= shift
      val truncatedBitNum = Math.max(-1074 - e2, 11)
      val savedBitNum     = 64 - truncatedBitNum
      val mask            = -1L >>> Math.max(savedBitNum, 0)
      val halfwayDiff     = (m2 & mask) - (mask >>> 1)
      if (Math.abs(halfwayDiff) > 1 || savedBitNum <= 0) java.lang.Double.longBitsToDouble {
        if (savedBitNum <= 0) m2 = 0
        m2 >>>= truncatedBitNum
        e2 += truncatedBitNum
        if (savedBitNum >= 0 && halfwayDiff > 0) {
          if (m2 == 0x1fffffffffffffL) {
            m2 = 0x10000000000000L
            e2 += 1
          } else m2 += 1
        }
        if (e2 == -1074) m2
        else if (e2 >= 972) 0x7ff0000000000000L
        else (e2 + 1075).toLong << 52 | m2 & 0xfffffffffffffL
      }
      else java.math.BigDecimal.valueOf(m10, -e10).doubleValue()
    }

  @noinline private[this] def readAll(in: OneCharReader, s: String, consume: Boolean): Unit = {
    val len = s.length
    var i   = 0
    while (i < len) {
      if (in.readChar() != s.charAt(i)) throw UnsafeNumber
      i += 1
    }
    val current = in.read() // to be consistent read the terminator
    if (consume && current != -1) throw UnsafeNumber
  }

  @inline private[this] def unsignedMultiplyHigh(x: Long, y: Long): Long =
    Math.multiplyHigh(x, y) + x + y // FIXME: Use Math.unsignedMultiplyHigh after dropping of JDK 17 support

  private[this] final val pow10Doubles: Array[Double] =
    Array(1, 1e+1, 1e+2, 1e+3, 1e+4, 1e+5, 1e+6, 1e+7, 1e+8, 1e+9, 1e+10, 1e+11, 1e+12, 1e+13, 1e+14, 1e+15, 1e+16,
      1e+17, 1e+18, 1e+19, 1e+20, 1e+21, 1e+22)

  private[this] final val pow10Mantissas: Array[Long] = Array(
    -4671960508600951122L, -1228264617323800998L, -7685194413468457480L, -4994806998408183946L, -1631822729582842029L,
    -7937418233630358124L, -5310086773610559751L, -2025922448585811785L, -8183730558007214222L, -5617977179081629873L,
    -2410785455424649437L, -8424269937281487754L, -5918651403174471789L, -2786628235540701832L, -8659171674854020501L,
    -6212278575140137722L, -3153662200497784248L, -8888567902952197011L, -6499023860262858360L, -3512093806901185046L,
    -9112587656954322510L, -6779048552765515233L, -3862124672529506138L, -215969822234494768L, -7052510166537641086L,
    -4203951689744663454L, -643253593753441413L, -7319562523736982739L, -4537767136243840520L, -1060522901877412746L,
    -7580355841314464822L, -4863758783215693124L, -1468012460592228501L, -7835036815511224669L, -5182110000961642932L,
    -1865951482774665761L, -8083748704375247957L, -5492999862041672042L, -2254563809124702148L, -8326631408344020699L,
    -5796603242002637969L, -2634068034075909558L, -8563821548938525330L, -6093090917745768758L, -3004677628754823043L,
    -8795452545612846258L, -6382629663588669919L, -3366601061058449494L, -9021654690802612790L, -6665382345075878084L,
    -3720041912917459700L, -38366372719436721L, -6941508010590729807L, -4065198994811024355L, -469812725086392539L,
    -7211161980820077193L, -4402266457597708587L, -891147053569747830L, -7474495936122174250L, -4731433901725329908L,
    -1302606358729274481L, -7731658001846878407L, -5052886483881210105L, -1704422086424124727L, -7982792831656159810L,
    -5366805021142811859L, -2096820258001126919L, -8228041688891786181L, -5673366092687344822L, -2480021597431793123L,
    -8467542526035952558L, -5972742139117552794L, -2854241655469553088L, -8701430062309552536L, -6265101559459552766L,
    -3219690930897053053L, -8929835859451740015L, -6550608805887287114L, -3576574988931720989L, -9152888395723407474L,
    -6829424476226871438L, -3925094576856201394L, -294682202642863838L, -7101705404292871755L, -4265445736938701790L,
    -720121152745989333L, -7367604748107325189L, -4597819916706768583L, -1135588877456072824L, -7627272076051127371L,
    -4922404076636521310L, -1541319077368263733L, -7880853450996246689L, -5239380795317920458L, -1937539975720012668L,
    -8128491512466089774L, -5548928372155224313L, -2324474446766642487L, -8370325556870233411L, -5851220927660403859L,
    -2702340141148116920L, -8606491615858654931L, -6146428501395930760L, -3071349608317525546L, -8837122532839535322L,
    -6434717147622031249L, -3431710416100151157L, -9062348037703676329L, -6716249028702207507L, -3783625267450371480L,
    -117845565885576446L, -6991182506319567135L, -4127292114472071014L, -547429124662700864L, -7259672230555269896L,
    -4462904269766699466L, -966944318780986428L, -7521869226879198374L, -4790650515171610063L, -1376627125537124675L,
    -7777920981101784778L, -5110715207949843068L, -1776707991509915931L, -8027971522334779313L, -5423278384491086237L,
    -2167411962186469893L, -8272161504007625539L, -5728515861582144020L, -2548958808550292121L, -8510628282985014432L,
    -6026599335303880135L, -2921563150702462265L, -8743505996830120772L, -6317696477610263061L, -3285434578585440922L,
    -8970925639256982432L, -6601971030643840136L, -3640777769877412266L, -9193015133814464522L, -6879582898840692749L,
    -3987792605123478032L, -373054737976959636L, -7150688238876681629L, -4326674280168464132L, -796656831783192261L,
    -7415439547505577019L, -4657613415954583370L, -1210330751515841308L, -7673985747338482674L, -4980796165745715438L,
    -1614309188754756393L, -7926472270612804602L, -5296404319838617848L, -2008819381370884406L, -8173041140997884610L,
    -5604615407819967859L, -2394083241347571919L, -8413831053483314306L, -5905602798426754978L, -2770317479606055818L,
    -8648977452394866743L, -6199535797066195524L, -3137733727905356501L, -8878612607581929669L, -6486579741050024183L,
    -3496538657885142324L, -9102865688819295809L, -6766896092596731857L, -3846934097318526917L, -196981603220770742L,
    -7040642529654063570L, -4189117143640191558L, -624710411122851544L, -7307973034592864071L, -4523280274813692185L,
    -1042414325089727327L, -7569037980822161435L, -4849611457600313890L, -1450328303573004458L, -7823984217374209643L,
    -5168294253290374149L, -1848681798185579782L, -8072955151507069220L, -5479507920956448621L, -2237698882768172872L,
    -8316090829371189901L, -5783427518286599473L, -2617598379430861437L, -8553528014785370254L, -6080224000054324913L,
    -2988593981640518238L, -8785400266166405755L, -6370064314280619289L, -3350894374423386208L, -9011838011655698236L,
    -6653111496142234891L, -3704703351750405709L, -19193171260619233L, -6929524759678968877L, -4050219931171323192L,
    -451088895536766085L, -7199459587351560659L, -4387638465762062920L, -872862063775190746L, -7463067817500576073L,
    -4717148753448332187L, -1284749923383027329L, -7720497729755473937L, -5038936143766954517L, -1686984161281305242L,
    -7971894128441897632L, -5353181642124984136L, -2079791034228842266L, -8217398424034108273L, -5660062011615247437L,
    -2463391496091671392L, -8457148712698376476L, -5959749872445582691L, -2838001322129590460L, -8691279853972075893L,
    -6252413799037706963L, -3203831230369745799L, -8919923546622172981L, -6538218414850328322L, -3561087000135522498L,
    -9143208402725783417L, -6817324484979841368L, -3909969587797413806L, -275775966319379353L, -7089889006590693952L,
    -4250675239810979535L, -701658031336336515L, -7356065297226292178L, -4583395603105477319L, -1117558485454458744L,
    -7616003081050118571L, -4908317832885260310L, -1523711272679187483L, -7869848573065574033L, -5225624697904579637L,
    -1920344853953336643L, -8117744561361917258L, -5535494683275008668L, -2307682335666372931L, -8359830487432564938L,
    -5838102090863318269L, -2685941595151759932L, -8596242524610931813L, -6133617137336276863L, -3055335403242958174L,
    -8827113654667930715L, -6422206049907525490L, -3416071543957018958L, -9052573742614218705L, -6704031159840385477L,
    -3768352931373093942L, -98755145788979524L, -6979250993759194058L, -4112377723771604669L, -528786136287117932L,
    -7248020362820530564L, -4448339435098275301L, -948738275445456222L, -7510490449794491995L, -4776427043815727089L,
    -1358847786342270957L, -7766808894105001205L, -5096825099203863602L, -1759345355577441598L, -8017119874876982855L,
    -5409713825168840664L, -2150456263033662926L, -8261564192037121185L, -5715269221619013577L, -2532400508596379068L,
    -8500279345513818773L, -6013663163464885563L, -2905392935903719049L, -8733399612580906262L, -6305063497298744923L,
    -3269643353196043250L, -8961056123388608887L, -6589634135808373205L, -3625356651333078602L, -9183376934724255983L,
    -6867535149977932074L, -3972732919045027189L, -354230130378896082L, -7138922859127891907L, -4311967555482476980L,
    -778273425925708321L, -7403949918844649557L, -4643251380128424042L, -1192378206733142148L, -7662765406849295699L,
    -4966770740134231719L, -1596777406740401745L, -7915514906853832947L, -5282707615139903279L, -1991698500497491195L,
    -8162340590452013853L, -5591239719637629412L, -2377363631119648861L, -8403381297090862394L, -5892540602936190089L,
    -2753989735242849707L, -8638772612167862923L, -6186779746782440750L, -3121788665050663033L, -8868646943297746252L,
    -6474122660694794911L, -3480967307441105734L, -9093133594791772940L, -6754730975062328271L, -3831727700400522434L,
    -177973607073265139L, -7028762532061872568L, -4174267146649952806L, -606147914885053103L, -7296371474444240046L,
    -4508778324627912153L, -1024286887357502287L, -7557708332239520786L, -4835449396872013078L, -1432625727662628443L,
    -7812920107430224633L, -5154464115860392887L, -1831394126398103205L, -8062150356639896359L, -5466001927372482545L,
    -2220816390788215277L, -8305539271883716405L, -5770238071427257602L, -2601111570856684098L, -8543223759426509417L,
    -6067343680855748868L, -2972493582642298180L, -8775337516792518219L, -6357485877563259869L, -3335171328526686933L,
    -9002011107970261189L, -6640827866535438582L, -3689348814741910324L, -9223372036854775808L, -6917529027641081856L,
    -4035225266123964416L, -432345564227567616L, -7187745005283311616L, -4372995238176751616L, -854558029293551616L,
    -7451627795949551616L, -4702848726509551616L, -1266874889709551616L, -7709325833709551616L, -5024971273709551616L,
    -1669528073709551616L, -7960984073709551616L, -5339544073709551616L, -2062744073709551616L, -8206744073709551616L,
    -5646744073709551616L, -2446744073709551616L, -8446744073709551616L, -5946744073709551616L, -2821744073709551616L,
    -8681119073709551616L, -6239712823709551616L, -3187955011209551616L, -8910000909647051616L, -6525815118631426616L,
    -3545582879861895366L, -9133518327554766460L, -6805211891016070171L, -3894828845342699810L, -256850038250986858L,
    -7078060301547948643L, -4235889358507547899L, -683175679707046970L, -7344513827457986212L, -4568956265895094861L,
    -1099509313941480672L, -7604722348854507276L, -4894216917640746191L, -1506085128623544835L, -7858832233030797378L,
    -5211854272861108819L, -1903131822648998119L, -8106986416796705681L, -5522047002568494197L, -2290872734783229842L,
    -8349324486880600507L, -5824969590173362730L, -2669525969289315508L, -8585982758446904049L, -6120792429631242157L,
    -3039304518611664792L, -8817094351773372351L, -6409681921289327535L, -3400416383184271515L, -9042789267131251553L,
    -6691800565486676537L, -3753064688430957767L, -79644842111309304L, -6967307053960650171L, -4097447799023424810L,
    -510123730351893109L, -7236356359111015049L, -4433759430461380907L, -930513269649338230L, -7499099821171918250L,
    -4762188758037509908L, -1341049929119499481L, -7755685233340769032L, -5082920523248573386L, -1741964635633328828L,
    -8006256924911912374L, -5396135137712502563L, -2133482903713240300L, -8250955842461857044L, -5702008784649933400L,
    -2515824962385028846L, -8489919629131724885L, -6000713517987268202L, -2889205879056697349L, -8723282702051517699L,
    -6292417359137009220L, -3253835680493873621L, -8951176327949752869L, -6577284391509803182L, -3609919470959866074L,
    -9173728696990998152L, -6855474852811359786L, -3957657547586811828L, -335385916056126881L, -7127145225176161157L,
    -4297245513042813542L, -759870872876129024L, -7392448323188662496L, -4628874385558440216L, -1174406963520662366L,
    -7651533379841495835L, -4952730706374481889L, -1579227364540714458L, -7904546130479028392L, -5268996644671397586L,
    -1974559787411859078L, -8151628894773493780L, -5577850100039479321L, -2360626606621961247L, -8392920656779807636L,
    -5879464802547371641L, -2737644984756826647L, -8628557143114098510L, -6174010410465235234L, -3105826994654156138L,
    -8858670899299929442L, -6461652605697523899L, -3465379738694516970L, -9083391364325154962L, -6742553186979055799L,
    -3816505465296431844L, -158945813193151901L, -7016870160886801794L, -4159401682681114339L, -587566084924005019L,
    -7284757830718584993L, -4494261269970843337L, -1006140569036166268L, -7546366883288685774L, -4821272585683469313L,
    -1414904713676948737L, -7801844473689174817L, -5140619573684080617L, -1814088448677712867L, -8051334308064652398L,
    -5452481866653427593L, -2203916314889396588L, -8294976724446954723L, -5757034887131305500L, -2584607590486743971L,
    -8532908771695296838L, -6054449946191733143L, -2956376414312278525L, -8765264286586255934L, -6344894339805432014L,
    -3319431906329402113L, -8992173969096958177L, -6628531442943809817L, -3673978285252374367L, -9213765455923815836L,
    -6905520801477381891L, -4020214983419339459L, -413582710846786420L, -7176018221920323369L, -4358336758973016307L,
    -836234930288882479L, -7440175859071633406L, -4688533805412153853L, -1248981238337804412L, -7698142301602209614L,
    -5010991858575374113L, -1652053804791829737L, -7950062655635975442L, -5325892301117581398L, -2045679357969588844L,
    -8196078626372074883L, -5633412264537705700L, -2430079312244744221L, -8436328597794046994L, -5933724728815170839L,
    -2805469892591575644L, -8670947710510816634L, -6226998619711132888L, -3172062256211528206L, -8900067937773286985L,
    -6513398903789220827L, -3530062611309138130L, -9123818159709293187L, -6793086681209228580L, -3879672333084147821L,
    -237904397927796872L, -7066219276345954901L, -4221088077005055722L, -664674077828931749L, -7332950326284164199L,
    -4554501889427817345L, -1081441343357383777L, -7593429867239446717L, -4880101315621920492L, -1488440626100012711L,
    -7847804418953589800L, -5198069505264599346L, -1885900863153361279L, -8096217067111932656L, -5508585315462527915L,
    -2274045625900771990L, -8338807543829064350L, -5811823411358942533L, -2653093245771290262L, -8575712306248138270L,
    -6107954364382784934L, -3023256937051093263L, -8807064613298015146L, -6397144748195131028L, -3384744916816525881L,
    -9032994600651410532L, -6679557232386875260L, -3737760522056206171L, -60514634142869810L, -6955350673980375487L,
    -4082502324048081455L, -491441886632713915L, -7224680206786528053L, -4419164240055772162L, -912269281642327298L,
    -7487697328667536418L, -4747935642407032618L, -1323233534581402868L, -7744549986754458649L, -5069001465015685407L,
    -1724565812842218855L, -7995382660667468640L, -5382542307406947896L, -2116491865831296966L, -8240336443785642460L,
    -5688734536304665171L, -2499232151953443560L, -8479549122611984081L, -5987750384837592197L, -2873001962619602342L,
    -8713155254278333320L, -6279758049420528746L, -3238011543348273028L, -8941286242233752499L, -6564921784364802720L,
    -3594466212028615495L, -9164070410158966541L, -6843401994271320272L, -3942566474411762436L, -316522074587315140L,
    -7115355324258153819L, -4282508136895304370L, -741449152691742558L, -7380934748073420955L, -4614482416664388289L,
    -1156417002403097458L, -7640289654143017767L, -4938676049251384305L, -1561659043136842477L, -7893565929601608404L,
    -5255271393574622601L, -1957403223540890347L, -8140906042354138323L, -5564446534515285000L, -2343872149716718346L,
    -8382449121214030822L, -5866375383090150624L, -2721283210435300376L, -8618331034163144591L, -6161227774276542835L,
    -3089848699418290639L, -8848684464777513506L, -6449169562544503978L, -3449775934753242068L, -9073638986861858149L,
    -6730362715149934782L, -3801267375510030573L, -139898200960150313L, -7004965403241175802L, -4144520735624081848L,
    -568964901102714406L, -7273132090830278360L, -4479729095110460046L, -987975350460687153L, -7535013621679011327L,
    -4807081008671376254L, -1397165242411832414L, -7790757304148477115L, -5126760611758208489L, -1796764746270372707L,
    -8040506994060064798L, -5438947724147693094L, -2186998636757228463L, -8284403175614349646L, -5743817951090549153L,
    -2568086420435798537L, -8522583040413455942L, -6041542782089432023L, -2940242459184402125L, -8755180564631333184L,
    -6332289687361778576L, -3303676090774835316L, -8982326584375353929L, -6616222212041804507L, -3658591746624867729L,
    -9204148869281624187L, -6893500068174642330L, -4005189066790915008L, -394800315061255856L, -7164279224554366766L,
    -4343663012265570553L, -817892746904575288L, -7428711994456441411L, -4674203974643163860L, -1231068949876566920L,
    -7686947121313936181L, -4996997883215032323L, -1634561335591402499L, -7939129862385708418L, -5312226309554747619L,
    -2028596868516046619L, -8185402070463610993L, -5620066569652125837L
  )
}
