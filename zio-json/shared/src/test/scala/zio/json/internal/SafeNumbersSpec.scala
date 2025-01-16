package zio.json.internal

import zio.json.Gens._
import zio.test.Assertion._
import zio.test.TestAspect.jvmOnly
import zio.test._

object SafeNumbersSpec extends ZIOSpecDefault {
  val spec =
    suite("SafeNumbers")(
      test("valid big decimals") {
        check(genBigDecimal)(i => assert(SafeNumbers.bigDecimal(i.toString, 2048))(isSome(equalTo(i))))
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
          "-Infinity"
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
          assert(SafeNumbers.bigDecimal(s).map(_.toString))(
            isSome(
              equalTo((new java.math.BigDecimal(s)).toString)
            )
          )
        }
      },
      test("invalid BigDecimal text") {
        check(genAlphaLowerString)(s => assert(SafeNumbers.bigDecimal(s))(isNone))
      },
      test("valid BigInteger edge cases") {
        val inputs = List(
          "00",
          "01",
          "0000001",
          "-9223372036854775807",
          "9223372036854775806",
          "-9223372036854775809",
          "9223372036854775808"
        )

        check(Gen.fromIterable(inputs)) { s =>
          assert(SafeNumbers.bigInteger(s))(
            isSome(
              equalTo((new java.math.BigInteger(s)))
            )
          )
        }
      },
      test("invalid BigInteger edge cases") {
        val inputs = List("0foo", "01foo", "0.1", "", "1 ")

        check(Gen.fromIterable(inputs))(s => assert(SafeNumbers.bigInteger(s))(isNone))
      },
      test("valid big Integer") {
        check(genBigInteger)(i => assert(SafeNumbers.bigInteger(i.toString, 2048))(isSome(equalTo(i))))
      },
      test("invalid BigInteger") {
        check(genAlphaLowerString)(s => assert(SafeNumbers.bigInteger(s))(isNone))
      },
      test("valid Byte") {
        check(Gen.byte(Byte.MinValue, Byte.MaxValue)) { b =>
          assert(SafeNumbers.byte(b.toString))(equalTo(ByteSome(b)))
        }
      },
      test("invalid Byte (numbers)") {
        check(Gen.long.filter(i => i < Byte.MinValue || i > Byte.MaxValue)) { b =>
          assert(SafeNumbers.byte(b.toString))(equalTo(ByteNone))
        }
      },
      test("invalid Byte (text)") {
        check(genAlphaLowerString)(b => assert(SafeNumbers.byte(b.toString))(equalTo(ByteNone)))
      },
      suite("Double")(
        test("valid") {
          check(Gen.double.filterNot(_.isNaN)) { d =>
            assert(SafeNumbers.double(d.toString))(equalTo(DoubleSome(d)))
          }
        },
        test("valid (from Int)") {
          check(Gen.int)(i => assert(SafeNumbers.double(i.toString))(equalTo(DoubleSome(i.toDouble))))
        },
        test("valid (from Long)") {
          check(Gen.long)(i => assert(SafeNumbers.double(i.toString))(equalTo(DoubleSome(i.toDouble))))
        },
        test("invalid edge cases") {
          val inputs = List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1", "1 ")

          check(Gen.fromIterable(inputs))(i => assert(SafeNumbers.double(i))(equalTo(DoubleNone)))
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
            "000.00001000", // trailing zeros
            "NaN",
            "92233720368547758070", // overflows a Long significand
            "Infinity",
            "+Infinity",
            "-Infinity",
            "3.976210887433566E-281" // rounds if a naive scaling is used
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
          check(genAlphaLowerString)(s => assert(SafeNumbers.double(s))(equalTo(DoubleNone)))
        }
      ),
      suite("Float")(
        test("valid") {
          check(Gen.float.filterNot(_.isNaN))(d => assert(SafeNumbers.float(d.toString))(equalTo(FloatSome(d))))
        },
        test("large mantissa") {
          // https://github.com/zio/zio-json/issues/221
          assert(SafeNumbers.float("1.199999988079071"))(equalTo(FloatSome(1.1999999f)))
        } @@ jvmOnly,
        test("valid (from Int)") {
          check(Gen.int)(i => assert(SafeNumbers.float(i.toString))(equalTo(FloatSome(i.toFloat))))
        },
        test("valid (from Long)") {
          check(Gen.long)(i => assert(SafeNumbers.float(i.toString))(equalTo(FloatSome(i.toFloat))))
        },
        test("invalid edge cases") {
          val inputs = List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1")

          check(Gen.fromIterable(inputs))(i => assert(SafeNumbers.float(i))(equalTo(FloatNone)))
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
            "000.00001000", // trailing zeros
            "NaN",
            "92233720368547758070", // overflows a Long significand
            "Infinity",
            "+Infinity",
            "-Infinity"
          )

          check(Gen.fromIterable(inputs)) { s =>
            // better to do the comparison on strings to deal with NaNs
            assert(SafeNumbers.float(s).toString)(
              equalTo(FloatSome(s.toFloat).toString)
            )
          }
        },
        test("valid (from Double)") {
          check(Gen.double.filterNot(_.isNaN)) { d =>
            assert(SafeNumbers.float(d.toString))(equalTo(FloatSome(d.toFloat)))
          }
        },
        test("invalid float (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.float(s))(equalTo(FloatNone)))
        }
      ),
      suite("Int")(
        test("valid") {
          check(Gen.int)(d => assert(SafeNumbers.int(d.toString))(equalTo(IntSome(d))))
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
          val input = List("00", "01", "0000001", "-9223372036854775807", "9223372036854775806")

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.long(x))(equalTo(LongSome(x.toLong))))
        },
        test("in valid edge cases") {
          val input = List(
            "0foo",
            "01foo",
            "0.1",
            "",
            "1 ",
            "-9223372036854775809",
            "9223372036854775808"
          )

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.long(x))(equalTo(LongNone)))
        },
        test("valid") {
          check(Gen.long)(d => assert(SafeNumbers.long(d.toString))(equalTo(LongSome(d))))
        },
        test("invalid (out of range)") {
          val outOfRange = genBigInteger
            .filter(_.bitLength > 63)

          check(outOfRange)(x => assert(SafeNumbers.long(x.toString))(equalTo(LongNone)))
        },
        test("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.long(s))(equalTo(LongNone)))
        }
      ),
      suite("Short")(
        test("valid") {
          check(Gen.short)(d => assert(SafeNumbers.short(d.toString))(equalTo(ShortSome(d))))
        },
        test("invalid (out of range)") {
          check(Gen.long.filter(i => i < Short.MinValue || i > Short.MaxValue))(d =>
            assert(SafeNumbers.short(d.toString))(equalTo(ShortNone))
          )
        },
        test("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.short(s))(equalTo(ShortNone)))
        }
      )
    )
}
