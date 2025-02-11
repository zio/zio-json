package zio.json.internal

import zio.json.Gens._
import zio.test.Assertion._
import zio.test.TestAspect.jvmOnly
import zio.test._

object SafeNumbersSpec extends ZIOSpecDefault {
  val spec =
    suite("SafeNumbers")(
      suite("BigDecimal")(
        test("valid big decimals") {
          check(genBigDecimal)(x => assert(SafeNumbers.bigDecimal(x.toString))(isSome(equalTo(x))))
        },
        test("invalid big decimals") {
          val invalidBigDecimalEdgeCases = List(
            "N",
            "Inf",
            "-NaN",
            "+NaN",
            "e1",
            "1.1.1",
            "1 ",
            "NaN",
            "Infinity",
            "+Infinity",
            "-Infinity",
            "1eO",
            "1e+2147483648",
            "1e+3147483648",
            "9" * 99,
            "0." + "9" * 99
          ).map(s => SafeNumbers.bigDecimal(s))

          assert(invalidBigDecimalEdgeCases)(forall(isNone))
        },
        test("valid big decimal edge cases") {
          val invalidBigDecimalEdgeCases = List(
            ".0",
            "-.0",
            "0",
            "0.0",
            "-0.0", // zeroes
            "0000.1",
            "0.00001",
            "000.00001000" // various trailing zeros, should be preserved
          )

          check(Gen.fromIterable(invalidBigDecimalEdgeCases)) { s =>
            assert(SafeNumbers.bigDecimal(s).get.compareTo(new java.math.BigDecimal(s)))(equalTo(0))
          }
        },
        test("invalid BigDecimal text") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.bigDecimal(s))(isNone))
        }
      ),
      suite("BigInteger")(
        test("valid BigInteger edge cases") {
          val inputs = List(
            "0",
            "0123",
            "-123",
            "-9223372036854775807",
            "9223372036854775806",
            "-9223372036854775809",
            "9223372036854775808"
          )

          check(Gen.fromIterable(inputs)) { s =>
            assert(SafeNumbers.bigInteger(s))(
              isSome(
                equalTo(new java.math.BigInteger(s))
              )
            )
          }
        },
        test("invalid BigInteger edge cases") {
          val inputs = List("0e+1", "01E-1", "0.1", "", "1 ")

          check(Gen.fromIterable(inputs))(s => assert(SafeNumbers.bigInteger(s))(isNone))
        },
        test("valid big Integer") {
          check(genBigInteger)(x => assert(SafeNumbers.bigInteger(x.toString, 2048))(isSome(equalTo(x))))
        },
        test("invalid BigInteger") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.bigInteger(s))(isNone))
        }
      ),
      suite("Byte")(
        test("valid Byte") {
          check(Gen.byte(Byte.MinValue, Byte.MaxValue)) { x =>
            val r = SafeNumbers.byte(x.toString)
            assert(r)(equalTo(ByteSome(x))) && assert(r.isEmpty)(equalTo(false))
          }
        },
        test("invalid Byte (numbers)") {
          check(Gen.int.filter(x => x < Byte.MinValue || x > Byte.MaxValue)) { x =>
            assert(SafeNumbers.byte(x.toString))(equalTo(ByteNone))
          }
        },
        test("invalid Byte (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.byte(s).isEmpty)(equalTo(true)))
        }
      ),
      suite("Double")(
        test("valid") {
          check(Gen.double.filterNot(_.isNaN)) { x =>
            val r = SafeNumbers.double(x.toString)
            assert(r)(equalTo(DoubleSome(x))) && assert(r.isEmpty)(equalTo(false))
          }
        },
        test("valid (from Int)") {
          check(Gen.int)(x => assert(SafeNumbers.double(x.toString))(equalTo(DoubleSome(x.toDouble))))
        },
        test("valid (from Long)") {
          check(Gen.long)(x => assert(SafeNumbers.double(x.toString))(equalTo(DoubleSome(x.toDouble))))
        },
        test("valid (from BigDecimal)") {
          check(genBigDecimal)(x => assert(SafeNumbers.double(x.toString))(equalTo(DoubleSome(x.doubleValue))))
        },
        test("invalid edge cases") {
          val inputs = List(
            "N",
            "Inf",
            "Info",
            "-NaN",
            "+NaN",
            "e1",
            "1.1.1",
            "1 ",
            "1eO",
            "1e+2147483648",
            "1e+3147483648",
            "9" * 99,
            "0." + "9" * 99
          )

          check(Gen.fromIterable(inputs))(s => assert(SafeNumbers.double(s))(equalTo(DoubleNone)))
        },
        test("valid edge cases") {
          val inputs = List(
            ".0",
            "-.0",
            "0",
            "0.0",
            "-0.0", // zeroes
            "0000.1",
            "0.00001",
            "0.0e-12",
            "1.1e-12",
            "1.1e-1234",
            "1.1e+1234",
            "000.00001000", // trailing zeros
            "NaN",
            "92233720368547758070", // overflows a Long significand
            "Infinity",
            "+Infinity",
            "-Infinity",
            "503599627370496E+13",    // fast path
            "503599627370496E+23",    // fast path with slop
            "3.976210887433566E-281", // rounds if a naive scaling is used
            "9007199254740993.0",     // round-down, halfway
            "18014398509481986.0",
            "9223372036854776832.0",
            "9007199254740995.0", // round-up, halfway
            "18014398509481990.0",
            "9223372036854778880.0",
            "9223372036854776833.0", // round-up, above halfway
            "36028797018963967.0",   // 2^n - 1 integer regression
            "2.2250738585072014E-308",
            "2.2250738585072013E-308",
            "2.2250738585072012E-308",
            "2.2250738585072011E-308"
          )

          check(Gen.fromIterable(inputs)) { s =>
            // better to do the comparison on strings to deal with NaNs
            assert(SafeNumbers.double(s).toString)(
              equalTo(DoubleSome(s.toDouble).toString)
            )
          }
        },
        test("valid magic doubles") {
          assert(SafeNumbers.double("NaN"))(not(equalTo(DoubleNone))) &&
          assert(SafeNumbers.double("Infinity"))(not(equalTo(DoubleNone))) &&
          assert(SafeNumbers.double("+Infinity"))(not(equalTo(DoubleNone))) &&
          assert(SafeNumbers.double("-Infinity"))(not(equalTo(DoubleNone)))
        },
        test("invalid doubles (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.double(s).isEmpty)(equalTo(true)))
        }
      ),
      suite("Float")(
        test("valid") {
          check(Gen.float.filterNot(_.isNaN)) { x =>
            val r = SafeNumbers.float(x.toString)
            assert(r)(equalTo(FloatSome(x))) && assert(r.isEmpty)(equalTo(false))
          }
        },
        test("large mantissa") {
          // https://github.com/zio/zio-json/issues/221
          assert(SafeNumbers.float("1.199999988079071"))(equalTo(FloatSome(1.1999999f)))
        } @@ jvmOnly,
        test("valid (from Int)") {
          check(Gen.int)(x => assert(SafeNumbers.float(x.toString))(equalTo(FloatSome(x.toFloat))))
        },
        test("valid (from Long)") {
          check(Gen.long)(x => assert(SafeNumbers.float(x.toString))(equalTo(FloatSome(x.toFloat))))
        },
        test("invalid edge cases") {
          val inputs = List(
            "N",
            "Inf",
            "Info",
            "-NaN",
            "+NaN",
            "e1",
            "1.1.1",
            "1eO",
            "1e+2147483648",
            "1e+3147483648",
            "9" * 99,
            "0." + "9" * 99
          )

          check(Gen.fromIterable(inputs))(s => assert(SafeNumbers.float(s))(equalTo(FloatNone)))
        },
        test("valid edge cases") {
          val inputs = List(
            ".0",
            "-.0",
            "0",
            "0.0",
            "-0.0", // zeroes
            "0000.1",
            "0.00001",
            "0.0e-12",
            "1.1e-12",
            "1.1e-1234",
            "1.1e+1234",
            "000.00001000", // trailing zeros
            "NaN",
            "92233720368547758070", // overflows a Long significand
            "Infinity",
            "+Infinity",
            "-Infinity",
            "16777217.0", // round-down, halfway
            "33554434.0",
            "17179870208.0",
            "16777219.0", // round-up, halfway
            "33554438.0",
            "17179872256.0",
            "33554435.0", // round-up, above halfway
            "17179870209.0",
            "37930954282500097", // fast path with `toFloat`
            "48696272630054913",
            // TODO: uncomment after release of Scala Native 0.5.7
            // "1.00000017881393432617187499", // check exactly halfway, round-up at halfway
            // "1.000000178813934326171875",
            // "1.00000017881393432617187501",
            "36028797018963967.0", // 2^n - 1 integer regression
            "1.17549435E-38",
            "1.17549434E-38",
            "1.17549433E-38",
            "1.17549432E-38",
            "1.17549431E-38",
            "1.17549430E-38",
            "1.17549429E-38",
            "1.17549428E-38"
          )

          check(Gen.fromIterable(inputs)) { s =>
            // better to do the comparison on strings to deal with NaNs
            assert(SafeNumbers.float(s).toString)(
              equalTo(FloatSome(s.toFloat).toString)
            )
          }
        },
        test("valid (from Double)") {
          check(Gen.double.filterNot(_.isNaN)) { x =>
            assert(SafeNumbers.float(x.toString))(equalTo(FloatSome(x.toFloat)))
          }
        },
        test("valid (from BigDecimal)") {
          check(genBigDecimal)(i => assert(SafeNumbers.float(i.toString))(equalTo(FloatSome(i.floatValue))))
        },
        test("invalid float (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.float(s).isEmpty)(equalTo(true)))
        }
      ),
      suite("Int")(
        test("valid edge cases") {
          val input = List("00", "01", "0000001", "-2147483648", "2147483647")

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.int(x))(equalTo(IntSome(x.toInt))))
        },
        test("valid") {
          check(Gen.int) { x =>
            val r = SafeNumbers.int(x.toString)
            assert(r)(equalTo(IntSome(x))) && assert(r.isEmpty)(equalTo(false))
          }
        },
        test("invalid (edge cases)") {
          val input = List(
            "1e3",
            "1E-2",
            "0.1",
            "",
            "1 ",
            "-2147483649",
            "2147483648"
          )

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.int(x))(equalTo(IntNone)))
        },
        test("invalid (out of range)") {
          check(Gen.long.filter(i => i < Int.MinValue || i > Int.MaxValue))(d =>
            assert(SafeNumbers.int(d.toString))(equalTo(IntNone))
          )
        },
        test("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.int(s))(equalTo(IntNone)))
        }
      ),
      suite("Long")(
        test("valid edge cases") {
          val input = List("00", "01", "0000001", "-9223372036854775808", "9223372036854775807")

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.long(x))(equalTo(LongSome(x.toLong))))
        },
        test("invalid (edge cases)") {
          val input = List(
            "1e3foo",
            "1E-2",
            "0.1",
            "",
            "1 ",
            "-9223372036854775809",
            "9223372036854775808"
          )

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.long(x))(equalTo(LongNone)))
        },
        test("valid") {
          check(Gen.long) { x =>
            val r = SafeNumbers.long(x.toString)
            assert(r)(equalTo(LongSome(x))) && assert(r.isEmpty)(equalTo(false))
          }
        },
        test("invalid (out of range)") {
          val outOfRange = genBigInteger.filter(_.bitLength > 63)

          check(outOfRange)(x => assert(SafeNumbers.long(x.toString))(equalTo(LongNone)))
        },
        test("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.long(s).isEmpty)(equalTo(true)))
        }
      ),
      suite("Short")(
        test("valid") {
          check(Gen.short) { x =>
            val r = SafeNumbers.short(x.toString)
            assert(r)(equalTo(ShortSome(x))) && assert(r.isEmpty)(equalTo(false))
          }
        },
        test("invalid (out of range)") {
          check(Gen.int.filter(i => i < Short.MinValue || i > Short.MaxValue))(d =>
            assert(SafeNumbers.short(d.toString))(equalTo(ShortNone))
          )
        },
        test("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.short(s).isEmpty)(equalTo(true)))
        }
      )
    )
}
