package testzio.json.internal

import testzio.json.TestUtils._
import zio.json.internal._
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, _ }

object SafeNumbersSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("SafeNumbers")(
      testM("valid big decimals") {
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
      testM("valid big decimal edge cases") {
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
      testM("invalid BigDecimal text") {
        check(genAlphaLowerString)(s => assert(SafeNumbers.bigDecimal(s))(isNone))
      },
      testM("valid BigInteger edge cases") {
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
      testM("invalid BigInteger edge cases") {
        val inputs = List("0foo", "01foo", "0.1", "", "1 ")

        check(Gen.fromIterable(inputs))(s => assert(SafeNumbers.bigInteger(s))(isNone))
      },
      testM("valid big Integer") {
        check(genBigInteger)(i => assert(SafeNumbers.bigInteger(i.toString, 2048))(isSome(equalTo(i))))
      },
      testM("invalid BigInteger") {
        check(genAlphaLowerString)(s => assert(SafeNumbers.bigInteger(s))(isNone))
      },
      testM("valid Byte") {
        check(Gen.byte(Byte.MinValue, Byte.MaxValue)) { b =>
          assert(SafeNumbers.byte(b.toString))(equalTo(ByteSome(b)))
        }
      },
      testM("invalid Byte (numbers)") {
        check(Gen.anyLong.filter(i => i < Byte.MinValue || i > Byte.MaxValue)) { b =>
          assert(SafeNumbers.byte(b.toString))(equalTo(ByteNone))
        }
      },
      testM("invalid Byte (text)") {
        check(genAlphaLowerString)(b => assert(SafeNumbers.byte(b.toString))(equalTo(ByteNone)))
      },
      suite("Double")(
        testM("valid") {
          check(Gen.anyDouble.filterNot(_.isNaN)) { d =>
            assert(SafeNumbers.double(d.toString))(equalTo(DoubleSome(d)))
          }
        },
        testM("valid (from Int)") {
          check(Gen.anyInt)(i => assert(SafeNumbers.double(i.toString))(equalTo(DoubleSome(i.toDouble))))
        },
        testM("valid (from Long)") {
          check(Gen.anyLong)(i => assert(SafeNumbers.double(i.toString))(equalTo(DoubleSome(i.toDouble))))
        },
        testM("invalid edge cases") {
          val inputs = List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1", "1 ")

          check(Gen.fromIterable(inputs))(i => assert(SafeNumbers.double(i))(equalTo(DoubleNone)))
        },
        testM("valid edge cases") {
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
        testM("invalid doubles (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.double(s))(equalTo(DoubleNone)))
        }
      ),
      suite("Float")(
        testM("valid") {
          check(Gen.anyFloat.filterNot(_.isNaN))(d => assert(SafeNumbers.float(d.toString))(equalTo(FloatSome(d))))
        },
        test("large mantissa") {
          // https://github.com/zio/zio-json/issues/221
          assert(SafeNumbers.float("1.199999988079071"))(equalTo(FloatSome(1.1999999f)))
        },
        testM("valid (from Int)") {
          check(Gen.anyInt)(i => assert(SafeNumbers.float(i.toString))(equalTo(FloatSome(i.toFloat))))
        },
        testM("valid (from Long)") {
          check(Gen.anyLong)(i => assert(SafeNumbers.float(i.toString))(equalTo(FloatSome(i.toFloat))))
        },
        testM("invalid edge cases") {
          val inputs = List("N", "Inf", "-NaN", "+NaN", "e1", "1.1.1")

          check(Gen.fromIterable(inputs))(i => assert(SafeNumbers.float(i))(equalTo(FloatNone)))
        },
        testM("valid edge cases") {
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
        testM("valid (from Double)") {
          check(Gen.anyDouble.filterNot(_.isNaN)) { d =>
            assert(SafeNumbers.float(d.toString))(equalTo(FloatSome(d.toFloat)))
          }
        },
        testM("invalid float (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.float(s))(equalTo(FloatNone)))
        }
      ),
      suite("Int")(
        testM("valid") {
          check(Gen.anyInt)(d => assert(SafeNumbers.int(d.toString))(equalTo(IntSome(d))))
        },
        testM("invalid (out of range)") {
          check(Gen.anyLong.filter(i => i < Int.MinValue || i > Int.MaxValue))(d =>
            assert(SafeNumbers.int(d.toString))(equalTo(IntNone))
          )
        },
        testM("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.int(s))(equalTo(IntNone)))
        }
      ),
      suite("Long")(
        testM("valid edge cases") {
          val input = List("00", "01", "0000001", "-9223372036854775807", "9223372036854775806")

          check(Gen.fromIterable(input))(x => assert(SafeNumbers.long(x))(equalTo(LongSome(x.toLong))))
        },
        testM("in valid edge cases") {
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
        testM("valid") {
          check(Gen.anyLong)(d => assert(SafeNumbers.long(d.toString))(equalTo(LongSome(d))))
        },
        testM("invalid (out of range)") {
          val outOfRange = genBigInteger
            .filter(_.bitLength > 63)

          check(outOfRange)(x => assert(SafeNumbers.long(x.toString))(equalTo(LongNone)))
        },
        testM("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.long(s))(equalTo(LongNone)))
        }
      ),
      suite("Short")(
        testM("valid") {
          check(Gen.anyShort)(d => assert(SafeNumbers.short(d.toString))(equalTo(ShortSome(d))))
        },
        testM("invalid (out of range)") {
          check(Gen.anyLong.filter(i => i < Short.MinValue || i > Short.MaxValue))(d =>
            assert(SafeNumbers.short(d.toString))(equalTo(ShortNone))
          )
        },
        testM("invalid (text)") {
          check(genAlphaLowerString)(s => assert(SafeNumbers.short(s))(equalTo(ShortNone)))
        }
      )
    )
}
