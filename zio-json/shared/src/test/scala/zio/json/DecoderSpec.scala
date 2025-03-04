package zio.json

import zio._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect.jvmOnly
import zio.test._

import java.math.BigInteger
import java.time.{ Duration, OffsetDateTime, ZonedDateTime }
import java.util.UUID
import scala.collection.{ SortedMap, immutable, mutable }

object DecoderSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] =
    suite("Decoder")(
      suite("fromJson")(
        test("string") {
          assert(""""abc"""".fromJson[String])(isRight(equalTo("abc"))) &&
          assert(""""abc\n"""".fromJson[String])(isRight(equalTo("abc\n"))) &&
          assert("\"abc\\u0182\"".fromJson[String])(isRight(equalTo("abcƂ"))) &&
          assert("\"abc\\u1Ee1\"".fromJson[String])(isRight(equalTo("abcỡ"))) &&
          assert(""""abc\x"""".fromJson[String])(isLeft(equalTo("""(invalid '\x' in string)"""))) &&
          assert("\"\u0000\"".fromJson[String])(isLeft(equalTo("""(invalid control in string)"""))) &&
          assert("\"\\u0000\"".replace('0', 'g').fromJson[String])(isLeft(equalTo("""(invalid charcode in string)""")))
        },
        test("char") {
          assert(""""a"""".fromJson[Char])(isRight(equalTo('a'))) &&
          assert(""""\n"""".fromJson[Char])(isRight(equalTo('\n'))) &&
          assert("\"\\u0182\"".fromJson[Char])(isRight(equalTo('Ƃ'))) &&
          assert("\"\\u1Ee1\"".fromJson[Char])(isRight(equalTo('ỡ'))) &&
          assert(""""aa"""".fromJson[Char])(isLeft(equalTo("""(expected single character string)"""))) &&
          assert(""""\x"""".fromJson[Char])(isLeft(equalTo("""(invalid '\x' in string)"""))) &&
          assert("\"\u0000\"".fromJson[Char])(isLeft(equalTo("""(invalid control in string)"""))) &&
          assert("\"\\u0000\"".replace('0', 'g').fromJson[Char])(isLeft(equalTo("""(invalid charcode in string)""")))
        },
        test("byte") {
          assert("-128".fromJson[Byte])(isRight(equalTo(Byte.MinValue))) &&
          assert("127".fromJson[Byte])(isRight(equalTo(Byte.MaxValue))) &&
          assert("\"-123\"".fromJson[Byte])(isRight(equalTo(-123: Byte))) &&
          assert("\"123\"".fromJson[Byte])(isRight(equalTo(123: Byte))) &&
          assertTrue("+123".fromJson[Byte].isLeft) &&
          assertTrue("\"Infinity\"".fromJson[Byte].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[Byte].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[Byte].isLeft) &&
          assertTrue("\"NaN\"".fromJson[Byte].isLeft)
        },
        test("short") {
          assert("-32768".fromJson[Short])(isRight(equalTo(Short.MinValue))) &&
          assert("32767".fromJson[Short])(isRight(equalTo(Short.MaxValue))) &&
          assert("\"-12345\"".fromJson[Short])(isRight(equalTo(-12345: Short))) &&
          assert("\"12345\"".fromJson[Short])(isRight(equalTo(12345: Short))) &&
          assertTrue("+12345".fromJson[Short].isLeft) &&
          assertTrue("\"Infinity\"".fromJson[Short].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[Short].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[Short].isLeft) &&
          assertTrue("\"NaN\"".fromJson[Short].isLeft)
        },
        test("int") {
          assert("-2147483648".fromJson[Int])(isRight(equalTo(Int.MinValue))) &&
          assert("2147483647".fromJson[Int])(isRight(equalTo(Int.MaxValue))) &&
          assert("\"-1234567890\"".fromJson[Int])(isRight(equalTo(-1234567890))) &&
          assert("\"1234567890\"".fromJson[Int])(isRight(equalTo(1234567890))) &&
          assertTrue("+1234567890".fromJson[Int].isLeft) &&
          assertTrue("\"Infinity\"".fromJson[Int].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[Int].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[Int].isLeft) &&
          assertTrue("\"NaN\"".fromJson[Int].isLeft)
        },
        test("long") {
          assert("-9223372036854775808".fromJson[Long])(isRight(equalTo(Long.MinValue))) &&
          assert("9223372036854775807".fromJson[Long])(isRight(equalTo(Long.MaxValue))) &&
          assert("\"-123456789012345678\"".fromJson[Long])(isRight(equalTo(-123456789012345678L))) &&
          assert("\"123456789012345678\"".fromJson[Long])(isRight(equalTo(123456789012345678L))) &&
          assertTrue("+123456789012345678".fromJson[Long].isLeft) &&
          assertTrue("\"Infinity\"".fromJson[Long].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[Long].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[Long].isLeft) &&
          assertTrue("\"NaN\"".fromJson[Long].isLeft)
        },
        test("float") {
          assert("1.234567e9".fromJson[Float])(isRight(equalTo(1.234567e9f))) &&
          assert("-1.234567e9".fromJson[Float])(isRight(equalTo(-1.234567e9f))) &&
          assert("\"-1.234567e9\"".fromJson[Float])(isRight(equalTo(-1.234567e9f))) &&
          assert("1.4e-45".fromJson[Float])(isRight(equalTo(1.4e-45f))) &&
          assert("8.3e38".fromJson[Float])(isRight(equalTo(Float.PositiveInfinity))) &&
          assert("-8.3e38".fromJson[Float])(isRight(equalTo(Float.NegativeInfinity))) &&
          assert("1.23456789012345678901e-2147483648".fromJson[Float])(isLeft(equalTo("(expected a Float)"))) &&
          assert("123456789012345678901e+2147483647".fromJson[Float])(isLeft(equalTo("(expected a Float)"))) &&
          assert("12345678901234567890.1e+2147483647".fromJson[Float])(isLeft(equalTo("(expected a Float)"))) &&
          assert("1.0e-2147483647".fromJson[Float])(isRight(equalTo(0.0f))) &&
          assert("-1.0e-2147483647".fromJson[Float])(isRight(equalTo(-0.0f))) &&
          assert("1234567890123456789.01e+2147483647".fromJson[Float])(isRight(equalTo(Float.PositiveInfinity))) &&
          assert("-1234567890123456789.01e+2147483647".fromJson[Float])(isRight(equalTo(Float.NegativeInfinity))) &&
          assert("\"Infinity\"".fromJson[Float])(isRight(equalTo(Float.PositiveInfinity))) &&
          assert("\"+Infinity\"".fromJson[Float])(isRight(equalTo(Float.PositiveInfinity))) &&
          assert("\"-Infinity\"".fromJson[Float])(isRight(equalTo(Float.NegativeInfinity))) &&
          assertTrue("\"NaN\"".fromJson[Float].isRight) &&
          assertTrue("Infinity".fromJson[Float].isLeft) &&
          assertTrue("+Infinity".fromJson[Float].isLeft) &&
          assertTrue("-Infinity".fromJson[Float].isLeft) &&
          assertTrue("NaN".fromJson[Float].isLeft) &&
          assertTrue("+1.234567e9".fromJson[Float].isLeft)
        },
        test("double") {
          assert("1.23456789012345e9".fromJson[Double])(isRight(equalTo(1.23456789012345e9))) &&
          assert("-1.23456789012345e9".fromJson[Double])(isRight(equalTo(-1.23456789012345e9))) &&
          assert("\"-1.23456789012345e9\"".fromJson[Double])(isRight(equalTo(-1.23456789012345e9))) &&
          assert("4.9e-324".fromJson[Double])(isRight(equalTo(4.9e-324))) &&
          assert("1.8e308".fromJson[Double])(isRight(equalTo(Double.PositiveInfinity))) &&
          assert("-1.8e308".fromJson[Double])(isRight(equalTo(Double.NegativeInfinity))) &&
          assert("1.23456789012345678901e-2147483648".fromJson[Double])(isLeft(equalTo("(expected a Double)"))) &&
          assert("12345678901234567890.1e+2147483647".fromJson[Double])(isLeft(equalTo("(expected a Double)"))) &&
          assert("123456789012345678901e+2147483647".fromJson[Double])(isLeft(equalTo("(expected a Double)"))) &&
          assert("1.0e-2147483647".fromJson[Double])(isRight(equalTo(0.0))) &&
          assert("-1.0e-2147483647".fromJson[Double])(isRight(equalTo(-0.0))) &&
          assert("1234567890123456789.01e+2147483647".fromJson[Double])(isRight(equalTo(Double.PositiveInfinity))) &&
          assert("-1234567890123456789.01e+2147483647".fromJson[Double])(isRight(equalTo(Double.NegativeInfinity))) &&
          assert("\"Infinity\"".fromJson[Double])(isRight(equalTo(Double.PositiveInfinity))) &&
          assert("\"+Infinity\"".fromJson[Double])(isRight(equalTo(Double.PositiveInfinity))) &&
          assert("\"-Infinity\"".fromJson[Double])(isRight(equalTo(Double.NegativeInfinity))) &&
          assertTrue("\"NaN\"".fromJson[Double].isRight) &&
          assertTrue("Infinity".fromJson[Double].isLeft) &&
          assertTrue("+Infinity".fromJson[Double].isLeft) &&
          assertTrue("-Infinity".fromJson[Double].isLeft) &&
          assertTrue("NaN".fromJson[Double].isLeft) &&
          assertTrue("+1.23456789012345e9".fromJson[Double].isLeft)
        },
        test("BigDecimal") {
          assert("-123.0e123".fromJson[BigDecimal])(isRight(equalTo(BigDecimal("-123.0e123")))) &&
          assert("123.0e123".fromJson[BigDecimal])(isRight(equalTo(BigDecimal("123.0e123")))) &&
          assertTrue("\"Infinity\"".fromJson[BigDecimal].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[BigDecimal].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[BigDecimal].isLeft) &&
          assertTrue("\"NaN\"".fromJson[BigDecimal].isLeft) &&
          assert(
            "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851"
              .fromJson[BigDecimal]
          )(isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))) &&
          assert("1.23456789012345678901e-2147483648".fromJson[BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert("12345678901234567890.1e+2147483647".fromJson[BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert("123456789012345678901e+2147483647".fromJson[BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          )
        },
        test("java.math.BigDecimal") {
          assert("-123.0e123".fromJson[java.math.BigDecimal])(
            isRight(equalTo(new java.math.BigDecimal("-123.0e123")))
          ) &&
          assert("123.0e123".fromJson[java.math.BigDecimal])(isRight(equalTo(new java.math.BigDecimal("123.0e123")))) &&
          assertTrue("\"Infinity\"".fromJson[java.math.BigDecimal].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[java.math.BigDecimal].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[java.math.BigDecimal].isLeft) &&
          assertTrue("\"NaN\"".fromJson[java.math.BigDecimal].isLeft) &&
          assert(
            "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851"
              .fromJson[java.math.BigDecimal]
          )(isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))) &&
          assert("1.23456789012345678901e-2147483648".fromJson[java.math.BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert("12345678901234567890.1e+2147483647".fromJson[java.math.BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert("123456789012345678901e+2147483647".fromJson[java.math.BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          )
        },
        test("BigInteger") {
          assert("170141183460469231731687303715884105728".fromJson[BigInteger])(
            isRight(equalTo(new BigInteger("170141183460469231731687303715884105728")))
          ) &&
          assert("-170141183460469231731687303715884105728".fromJson[BigInteger])(
            isRight(equalTo(new BigInteger("-170141183460469231731687303715884105728")))
          ) &&
          assertTrue("\"Infinity\"".fromJson[BigInteger].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[BigInteger].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[BigInteger].isLeft) &&
          assertTrue("\"NaN\"".fromJson[BigInteger].isLeft) &&
          assert(
            "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851316546851"
              .fromJson[BigInteger]
          )(isLeft(equalTo("(expected a 256-bit BigInteger)"))) &&
          assert(
            "17014118346046923173168730371588410572848946516548466848651357486465481896465316846".fromJson[BigInteger]
          )(isLeft(equalTo("(expected a 256-bit BigInteger)")))
        },
        test("BigInt") {
          assert("170141183460469231731687303715884105728".fromJson[BigInt])(
            isRight(equalTo(BigInt("170141183460469231731687303715884105728")))
          ) &&
          assert("-170141183460469231731687303715884105728".fromJson[BigInt])(
            isRight(equalTo(BigInt("-170141183460469231731687303715884105728")))
          ) &&
          assertTrue("\"Infinity\"".fromJson[BigInt].isLeft) &&
          assertTrue("\"+Infinity\"".fromJson[BigInt].isLeft) &&
          assertTrue("\"-Infinity\"".fromJson[BigInt].isLeft) &&
          assertTrue("\"NaN\"".fromJson[BigInt].isLeft) &&
          assert(
            "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851316546851"
              .fromJson[BigInt]
          )(isLeft(equalTo("(expected a 256-bit BigInt)"))) &&
          assert(
            "17014118346046923173168730371588410572848946516548466848651357486465481896465316846".fromJson[BigInt]
          )(isLeft(equalTo("(expected a 256-bit BigInt)")))
        },
        test("collections") {
          val arr = """[1, 2, 3]"""
          val obj = """{ "a": 1 }"""

          assert(arr.fromJson[Array[Int]])(isRight(equalTo(Array(1, 2, 3)))) &&
          assert(arr.fromJson[IndexedSeq[Int]])(isRight(equalTo(IndexedSeq(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.LinearSeq[Int]])(isRight(equalTo(immutable.LinearSeq(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.ListSet[Int]])(isRight(equalTo(immutable.ListSet(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.TreeSet[Int]])(isRight(equalTo(immutable.TreeSet(1, 2, 3)))) &&
          assert(obj.fromJson[mutable.Map[String, Int]])(isRight(equalTo(mutable.Map("a" -> 1))))
        },
        test("eithers") {
          val bernies = List("""{"a":1}""", """{"left":1}""", """{"Left":1}""")
          val trumps  = List("""{"b":2}""", """{"right":2}""", """{"Right":2}""")

          assert(bernies.map(_.fromJson[Either[Int, Int]]))(
            forall(isRight(isLeft(equalTo(1))))
          ) && assert(trumps.map(_.fromJson[Either[Int, Int]]))(
            forall(isRight(isRight(equalTo(2))))
          )
        },
        test("tuples") {
          assert("""["a",3]""".fromJson[(String, Int)])(isRight(equalTo(("a", 3))))
          assert("""["a","b"]""".fromJson[(String, Int)])(isLeft(equalTo("[1](expected an Int)")))
          assert("""[[0.1,0.2],[0.3,0.4],[-0.3,-]]""".fromJson[Seq[(Double, Double)]])(
            isLeft(equalTo("[2][1](expected a Double)"))
          )
        },
        test("parameterless products") {
          import exampleproducts._

          // actually anything works... consider this a canary test because if only
          // the empty object is supported that's fine.
          assert("""{}""".fromJson[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert("""null""".fromJson[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert("""{"field":"value"}""".fromJson[Parameterless])(isRight(equalTo(Parameterless())))
        },
        test("typical") {
          case class Banana(ripe: Boolean, curvature: Double)
          implicit val decoder: JsonDecoder[Banana] = DeriveJsonDecoder.gen

          assert("""{"curvature": 7, "ripe": true}""".fromJson[Banana])(
            isRight(
              equalTo(Banana(curvature = 7, ripe = true))
            )
          )
        },
        test("no extra fields") {
          import exampleproducts._

          assert("""{"s":""}""".fromJson[OnlyString])(isRight(equalTo(OnlyString("")))) &&
          assert("""{"s":"","t":""}""".fromJson[OnlyString])(isLeft(equalTo("(invalid extra field)")))
        },
        test("aliases") {
          case class Apple(@jsonAliases("ripeness", "old") ripe: Boolean, taste: Double)
          implicit val decoder: JsonDecoder[Apple] = DeriveJsonDecoder.gen

          val expected = Apple(ripe = true, taste = 7)
          assert("""{"taste":7,"ripe":true}""".fromJson[Apple])(isRight(equalTo(expected))) &&
          assert("""{"taste":7,"ripeness":true}""".fromJson[Apple])(isRight(equalTo(expected))) &&
          assert("""{"taste":7,"old":true}""".fromJson[Apple])(isRight(equalTo(expected))) &&
          assert("""{"taste":1,"ripe":true,"old":true}""".fromJson[Apple])(isLeft(equalTo("(duplicate)"))) &&
          assert("""{"taste":1,"ripeness":true,"old":true}""".fromJson[Apple])(isLeft(equalTo("(duplicate)")))
        },
        test("aliases - alias collides with field name") {
          for {
            error <- ZIO.attempt {
                       case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("radius") r: Int)
                       DeriveJsonDecoder.gen[Mango]
                     }.flip
          } yield assertTrue(
            // Class name in Scala 2: zio.json.DecoderSpec.spec.Mango
            // Class name in Scala 3: zio.json.DecoderSpec.spec.$anonfun.Mango
            error.getMessage.matches(
              "Field names and aliases in case class zio.json.DecoderSpec.spec(.\\$anonfun)?.Mango must be distinct, alias\\(es\\) r collide with a field or another alias"
            )
          )
        },
        test("aliases - alias collides with another alias") {
          for {
            error <- ZIO.attempt {
                       case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("r") radius: Int)
                       DeriveJsonDecoder.gen[Mango]
                     }.flip
          } yield assertTrue(
            error.getMessage.matches(
              "Field names and aliases in case class zio.json.DecoderSpec.spec(.\\$anonfun)?.Mango must be distinct, alias\\(es\\) r collide with a field or another alias"
            )
          )
        },
        test("aliases - double alias") {
          for {
            error <- ZIO.attempt {
                       case class Mango(@jsonAliases("r", "r") roundness: Int, radius: Int)
                       DeriveJsonDecoder.gen[Mango]
                     }.flip
          } yield assertTrue(
            error.getMessage.matches(
              "Field names and aliases in case class zio.json.DecoderSpec.spec(.\\$anonfun)?.Mango must be distinct, alias\\(es\\) r collide with a field or another alias"
            )
          )
        },
        test("option") {
          case class WithOpt(id: Int, opt: Option[Int])
          implicit val decoder: JsonDecoder[WithOpt] = DeriveJsonDecoder.gen

          assert("""{ "id": 1, "opt": 42 }""".fromJson[WithOpt])(isRight(equalTo(WithOpt(1, Some(42))))) &&
          assert("""{ "id": 1 }""".fromJson[WithOpt])(isRight(equalTo(WithOpt(1, None))))
        },
        test("option - fromJsonAST") {
          case class WithOpt(id: Int, opt: Option[Int])
          implicit val decoder: JsonDecoder[WithOpt] = DeriveJsonDecoder.gen

          assert("""{ "id": 1, "opt": 42 }""".fromJson[Json].flatMap(decoder.fromJsonAST))(
            isRight(equalTo(WithOpt(1, Some(42))))
          ) &&
          assert("""{ "id": 1 }""".fromJson[Json].flatMap(decoder.fromJsonAST))(isRight(equalTo(WithOpt(1, None))))
        },
        test("default field value") {
          import exampleproducts._

          assert("""{}""".fromJson[DefaultString])(isRight(equalTo(DefaultString("")))) &&
          assert("""{"s": null}""".fromJson[DefaultString])(isRight(equalTo(DefaultString(""))))
        },
        test("dynamic default value") {
          case class DefaultDynamic(
            randomNumber: Double = scala.math.random(),
            instant: java.time.Instant = java.time.Instant.now()
          )

          object DefaultDynamic {
            implicit lazy val decoder: JsonDecoder[DefaultDynamic] = DeriveJsonDecoder.gen[DefaultDynamic]
          }

          def res = """{}""".stripMargin.fromJson[DefaultDynamic]

          for {
            dynamics1 <- ZIO.fromEither(res)
            _         <- ZIO.sleep(2.millis)
            dynamics2 <- ZIO.fromEither(res)
          } yield assertTrue(dynamics1.randomNumber != dynamics2.randomNumber) &&
            assertTrue(dynamics1.instant != dynamics2.instant)
        } @@ TestAspect.withLiveClock,
        test("sum encoding") {
          import examplesum._

          assert("""{"Child1":{}}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"Child2":{}}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"type":"Child1"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)")))
        },
        test("sum encoding with enumValuesAsStrings = true") {
          import examplesumobjects1._

          assert(""""Child1"""".fromJson[Parent])(isRight(equalTo(Child1))) &&
          assert(""""Child2"""".fromJson[Parent])(isRight(equalTo(Child2)))
        },
        test("sum encoding with enumValuesAsStrings = false") {
          import examplesumobjects2._

          assert("""{"Child1":{}}""".fromJson[Parent])(isRight(equalTo(Child1))) &&
          assert("""{"Child2":{}}""".fromJson[Parent])(isRight(equalTo(Child2)))
        },
        test("sum encoding with hint names") {
          import examplesumhintnames._

          assert("""{"child1":{}}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"child2":{}}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"Child1":{}}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert("""{"type":"child1"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert("""{"hint":"Cain"}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"hint":"Abel"}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"hint":"Samson"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert("""{"Cain":{}}""".fromJson[Parent])(isLeft(equalTo("(missing hint 'hint')")))
        },
        test("sum alternative encoding with hint names") {
          import examplealtsumhintnames._

          assert("""{"hint":"child1"}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"hint":"Abel"}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"hint":"Child2"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert("""{"child1":{}}""".fromJson[Parent])(isLeft(equalTo("(missing hint 'hint')")))
        },
        test("sum with duplicated case names") {
          for {
            error <- ZIO.attempt {
                       sealed trait Fruit
                       case class Banana(curvature: Double)                extends Fruit
                       @jsonHint("Banana") case class Apple(color: String) extends Fruit
                       DeriveJsonDecoder.gen[Fruit]
                     }.flip
          } yield assertTrue(
            error.getMessage.matches(
              """Case names in ADT zio.json.DecoderSpec.spec(.\$anonfun)?.Fruit must be distinct, name\(s\) Banana are duplicated"""
            )
          )
        },
        test("unicode") {
          assert(""""€🐵🥰"""".fromJson[String])(isRight(equalTo("€🐵🥰")))
        },
        test("Option: .map on derived JsonDecoder with missing value") {
          // More information about use case here https://github.com/zio/zio-json/issues/198
          // User wants to derive an alternative encoding of optionality
          sealed trait Assumed[+A]

          object Assumed {
            case object MissingAssumed       extends Assumed[Nothing]
            case class FoundAssumed[A](v: A) extends Assumed[A]

            implicit def decoder[A](implicit decoding: JsonDecoder[A]): JsonDecoder[Assumed[A]] =
              JsonDecoder.option[A].map {
                case None    => Assumed.MissingAssumed
                case Some(v) => Assumed.FoundAssumed[A](v)
              }
          }

          case class Example(a: Assumed[Boolean])
          implicit val exampleDecoder: JsonDecoder[Example] = DeriveJsonDecoder.gen[Example]

          assert("""{ "a": null }""".fromJson[Example])(isRight(equalTo(Example(Assumed.MissingAssumed)))) &&
          assert("""{ "a": true }""".fromJson[Example])(isRight(equalTo(Example(Assumed.FoundAssumed(true))))) &&
          assert("""{ "a": false }""".fromJson[Example])(isRight(equalTo(Example(Assumed.FoundAssumed(false))))) &&
          assert("""{ }""".fromJson[Example])(isRight(equalTo(Example(Assumed.MissingAssumed))))
        },
        test("Seq") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Seq("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Seq[String]])(isRight(equalTo(expected)))
        },
        test("Vector") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Vector("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Vector[String]])(isRight(equalTo(expected)))
        },
        test("SortedSet") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        },
        test("HashSet") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[immutable.HashSet[String]])(isRight(equalTo(expected)))
        },
        test("Set") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Set("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Set[String]])(isRight(equalTo(expected)))
        },
        test("Map") {
          val jsonStr  = """{"5XL":3,"2XL":14,"XL":159}"""
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(jsonStr.fromJson[Map[String, Int]])(isRight(equalTo(expected)))
        },
        test("Map with unicode keys") {
          val expected = Map(new String(Array('\u0007', '\n')) -> "value")
          val jsonStr  = JsonEncoder[Map[String, String]].encodeJson(expected, None)
          assert(jsonStr.fromJson[Map[String, String]])(isRight(equalTo(expected)))
        },
        test("Map with Int keys") {
          assert("""{"1234567890": "value"}""".fromJson[Map[Int, String]])(
            isRight(equalTo(Map(1234567890 -> "value")))
          ) &&
          assert("""{"xxx": "value"}""".fromJson[Map[Int, String]])(isLeft(containsString("Invalid Int: xxx")))
        },
        test("Map with Long keys") {
          assert("""{"1234567890123456789": "value"}""".fromJson[Map[Long, String]])(
            isRight(equalTo(Map(1234567890123456789L -> "value")))
          ) &&
          assert("""{"xxx": "value"}""".fromJson[Map[Long, String]])(isLeft(containsString("Invalid Long: xxx")))
        },
        test("Map with UUID keys") {
          def expectedMap(str: String): Map[UUID, String] = Map(UUID.fromString(str) -> "value")

          val ok1  = """{"64d7c38d-2afd-4514-9832-4e70afe4b0f8": "value"}"""
          val ok2  = """{"0000000064D7C38D-FD-14-32-70AFE4B0f8": "value"}"""
          val ok3  = """{"0-0-0-0-0": "value"}"""
          val bad1 = """{"": "value"}"""
          val bad2 = """{"64d7c38d-2afd-4514-9832-4e70afe4b0f80": "value"}"""
          val bad3 = """{"64d7c38d-2afd-4514-983-4e70afe4b0f80": "value"}"""
          val bad4 = """{"64d7c38d-2afd--9832-4e70afe4b0f8": "value"}"""
          val bad5 = """{"64d7c38d-2afd-XXXX-9832-4e70afe4b0f8": "value"}"""
          val bad6 = """{"64d7c38d-2afd-X-9832-4e70afe4b0f8": "value"}"""
          val bad7 = """{"0-0-0-0-00000000000000000": "value"}"""
          val bad8 = """{"64d7c38d-2аfd-4514-9832-4e70afe4b0f8": "value"}"""
          val bad9 = """{"0000000064D7C38D-FD-14-32-70АFE4B0f8": "value"}"""

          assert(ok1.fromJson[Map[UUID, String]])(
            isRight(equalTo(expectedMap("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))
          ) &&
          assert(ok2.fromJson[Map[UUID, String]])(
            isRight(equalTo(expectedMap("64D7C38D-00FD-0014-0032-0070AfE4B0f8")))
          ) &&
          assert(ok3.fromJson[Map[UUID, String]])(
            isRight(equalTo(expectedMap("00000000-0000-0000-0000-000000000000")))
          ) &&
          assert(bad1.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad2.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad3.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad4.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad5.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad6.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad7.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad8.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad9.fromJson[Map[UUID, String]])(isLeft(containsString("(expected a UUID)")))
        },
        test("zio.Chunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Chunk[String]])(isRight(equalTo(expected)))
        },
        test("zio.NonEmptyChunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[NonEmptyChunk[String]])(isRight(equalTo(expected)))
        },
        test("zio.NonEmptyChunk failure") {
          val jsonStr = "[]"

          assert(jsonStr.fromJson[NonEmptyChunk[String]])(isLeft(equalTo("(Chunk was empty)")))
        },
        test("java.util.UUID") {
          val ok1  = """"64d7c38d-2afd-4514-9832-4e70afe4b0f8""""
          val ok2  = """"0000000064D7C38D-FD-14-32-70AFE4B0f8""""
          val ok3  = """"0-0-0-0-0""""
          val bad1 = """"""""
          val bad2 = """"64d7c38d-2afd-4514-9832-4e70afe4b0f80""""
          val bad3 = """"64d7c38d-2afd-4514-983-4e70afe4b0f80""""
          val bad4 = """"64d7c38d-2afd--9832-4e70afe4b0f8""""
          val bad5 = """"64d7c38d-2afd-XXXX-9832-4e70afe4b0f8""""
          val bad6 = """"64d7c38d-2afd-X-9832-4e70afe4b0f8""""
          val bad7 = """"0-0-0-0-00000000000000000""""
          val bad8 = """"64d7c38d-2аfd-4514-9832-4e70afe4b0f8""""
          val bad9 = """"0000000064D7C38D-FD-14-32-70АFE4B0f8""""

          assert(ok1.fromJson[UUID])(isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))) &&
          assert(ok2.fromJson[UUID])(isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AfE4B0f8")))) &&
          assert(ok3.fromJson[UUID])(isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))) &&
          assert(bad1.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad2.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad3.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad4.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad5.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad6.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad7.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad8.fromJson[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad9.fromJson[UUID])(isLeft(containsString("(expected a UUID)")))
        },
        test("java.util.Currency") {
          assert(""""USD"""".fromJson[java.util.Currency])(isRight(equalTo(java.util.Currency.getInstance("USD")))) &&
          assert(""""LLL"""".fromJson[java.util.Currency])(isLeft)
        } @@ jvmOnly,
        test("java.time.Duration") {
          val ok1  = """"PT1H2M3S""""
          val ok2  = """"PT-0.5S"""" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
          val bad1 = """"PT-H""""

          assert(ok1.fromJson[Duration])(isRight(equalTo(Duration.parse("PT1H2M3S")))) &&
          assert(ok2.fromJson[Duration])(isRight(equalTo(Duration.ofNanos(-500000000)))) &&
          assert(bad1.fromJson[Duration])(
            isLeft(containsString("expected a Duration"))
          )
        },
        test("java.time.ZonedDateTime") {
          val ok1 = """"2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]""""
          val ok2 =
            """"2018-10-28T02:30+00:00[Europe/Warsaw]"""" // see https://bugs.openjdk.java.net/browse/JDK-8066982
          val bad1 = """"2018-10-28T02:30""""

          assert(ok1.fromJson[ZonedDateTime])(
            isRight(equalTo(ZonedDateTime.parse("2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]")))
          ) &&
          assert(ok2.fromJson[ZonedDateTime].map(_.toOffsetDateTime))(
            isRight(equalTo(OffsetDateTime.parse("2018-10-28T03:30+01:00")))
          ) &&
          assert(bad1.fromJson[ZonedDateTime])(isLeft(equalTo("(expected a ZonedDateTime)")))
        },
        test("bothWith") {
          final case class Foo(a: Int)
          final case class Bar(b: String)

          val fooDecoder: JsonDecoder[Foo]                       = DeriveJsonDecoder.gen
          val barDecoder: JsonDecoder[Bar]                       = DeriveJsonDecoder.gen
          implicit val fooAndBarDecoder: JsonDecoder[(Foo, Bar)] = fooDecoder.both(barDecoder)

          val json = """{"a": 1, "b": "foo"}"""
          assertTrue(
            json.fromJson[(Foo, Bar)] == Right((Foo(1), Bar("foo")))
          )
        },
        test("option custom codec") {
          val json = """{"keyStatus": "certified"}"""
          final case class Foo(v: String)
          final case class RudderSettings(keyStatus: String, policyMode: Option[Foo])
          implicit val encoderOptionPolicyMode: JsonEncoder[Option[Foo]] = JsonEncoder.string.contramap {
            case None    => "default"
            case Some(f) => f.v
          }
          implicit val decoderOptionPolicyMode: JsonDecoder[Option[Foo]] = JsonDecoder[Option[String]].mapOrFail {
            case None | Some("default") => Right(None)
            case Some(s)                => Right(Some(Foo(s)))
          }
          implicit lazy val codecRudderSettings: JsonCodec[RudderSettings] = DeriveJsonCodec.gen
          assertTrue(json.fromJson[RudderSettings] == Right(RudderSettings("certified", None)))
        }
      ),
      suite("fromJsonAST")(
        test("boolean") {
          assert(Json.Bool(true).as[Boolean])(isRight(equalTo(true))) &&
          assert(Json.Str("true").as[Boolean])(isLeft(equalTo("(expected boolean)")))
        },
        test("string") {
          assert(Json.Str("xxx").as[String])(isRight(equalTo("xxx"))) &&
          assert(Json.Bool(true).as[String])(isLeft(equalTo("(expected string)")))
        },
        test("char") {
          assert(Json.Str("x").as[Char])(isRight(equalTo('x'))) &&
          assert(Json.Str("xxx").as[Char])(isLeft(equalTo("(expected single character string)"))) &&
          assert(Json.Bool(true).as[Char])(isLeft(equalTo("(expected single character string)")))
        },
        test("byte") {
          assert(Json.Num(Byte.MinValue).as[Byte])(isRight(equalTo(Byte.MinValue))) &&
          assert(Json.Num(Byte.MaxValue).as[Byte])(isRight(equalTo(Byte.MaxValue))) &&
          assert(Json.Str(Byte.MinValue.toString).as[Byte])(isRight(equalTo(Byte.MinValue))) &&
          assert(Json.Str(Byte.MaxValue.toString).as[Byte])(isRight(equalTo(Byte.MaxValue))) &&
          assertTrue(Json.Num(Byte.MinValue.toInt - 1).as[Byte].isLeft) &&
          assertTrue(Json.Num(Byte.MaxValue.toInt + 1).as[Byte].isLeft) &&
          assertTrue(Json.Str((Byte.MinValue.toInt - 1).toString).as[Byte].isLeft) &&
          assertTrue(Json.Str((Byte.MaxValue.toInt + 1).toString).as[Byte].isLeft) &&
          assertTrue(Json.Str("\"-123\"").as[Byte].isLeft) &&
          assertTrue(Json.Str("\"123\"").as[Byte].isLeft) &&
          assertTrue(Json.Str("123abc").as[Byte].isLeft) &&
          assertTrue(Json.Str("+123").as[Byte].isLeft) &&
          assertTrue(Json.Str("Infinity").as[Byte].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[Byte].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[Byte].isLeft) &&
          assertTrue(Json.Str("NaN").as[Byte].isLeft)
        },
        test("short") {
          assert(Json.Num(Short.MinValue).as[Short])(isRight(equalTo(Short.MinValue))) &&
          assert(Json.Num(Short.MaxValue).as[Short])(isRight(equalTo(Short.MaxValue))) &&
          assert(Json.Str(Short.MinValue.toString).as[Short])(isRight(equalTo(Short.MinValue))) &&
          assert(Json.Str(Short.MaxValue.toString).as[Short])(isRight(equalTo(Short.MaxValue))) &&
          assertTrue(Json.Num(Short.MinValue.toInt - 1).as[Short].isLeft) &&
          assertTrue(Json.Num(Short.MaxValue.toInt + 1).as[Short].isLeft) &&
          assertTrue(Json.Str((Short.MinValue.toInt - 1).toString).as[Short].isLeft) &&
          assertTrue(Json.Str((Short.MaxValue.toInt + 1).toString).as[Short].isLeft) &&
          assertTrue(Json.Str("\"-12345\"").as[Short].isLeft) &&
          assertTrue(Json.Str("\"12345\"").as[Short].isLeft) &&
          assertTrue(Json.Str("12345abc").as[Short].isLeft) &&
          assertTrue(Json.Str("+12345").as[Short].isLeft) &&
          assertTrue(Json.Str("Infinity").as[Short].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[Short].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[Short].isLeft) &&
          assertTrue(Json.Str("NaN").as[Short].isLeft)
        },
        test("int") {
          assert(Json.Num(Int.MinValue).as[Int])(isRight(equalTo(Int.MinValue))) &&
          assert(Json.Num(Int.MaxValue).as[Int])(isRight(equalTo(Int.MaxValue))) &&
          assert(Json.Str(Int.MinValue.toString).as[Int])(isRight(equalTo(Int.MinValue))) &&
          assert(Json.Str(Int.MaxValue.toString).as[Int])(isRight(equalTo(Int.MaxValue))) &&
          assertTrue(Json.Num(Int.MinValue.toLong - 1).as[Int].isLeft) &&
          assertTrue(Json.Num(Int.MaxValue.toLong + 1).as[Int].isLeft) &&
          assertTrue(Json.Str((Int.MinValue.toLong - 1).toString).as[Int].isLeft) &&
          assertTrue(Json.Str((Int.MaxValue.toLong + 1).toString).as[Int].isLeft) &&
          assertTrue(Json.Str("\"-1234567890\"").as[Int].isLeft) &&
          assertTrue(Json.Str("\"1234567890\"").as[Int].isLeft) &&
          assertTrue(Json.Str("1234567890abc").as[Int].isLeft) &&
          assertTrue(Json.Str("+1234567890").as[Int].isLeft) &&
          assertTrue(Json.Str("Infinity").as[Int].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[Int].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[Int].isLeft) &&
          assertTrue(Json.Str("NaN").as[Int].isLeft)
        },
        test("long") {
          assert(Json.Num(Long.MinValue).as[Long])(isRight(equalTo(Long.MinValue))) &&
          assert(Json.Num(Long.MaxValue).as[Long])(isRight(equalTo(Long.MaxValue))) &&
          assert(Json.Str(Long.MinValue.toString).as[Long])(isRight(equalTo(Long.MinValue))) &&
          assert(Json.Str(Long.MaxValue.toString).as[Long])(isRight(equalTo(Long.MaxValue))) &&
          assertTrue(Json.Num(BigDecimal(Long.MinValue) - 1).as[Long].isLeft) &&
          assertTrue(Json.Num(BigDecimal(Long.MaxValue) + 1).as[Long].isLeft) &&
          assertTrue(Json.Str((BigDecimal(Long.MinValue) - 1).toString).as[Long].isLeft) &&
          assertTrue(Json.Str((BigDecimal(Long.MaxValue) + 1).toString).as[Long].isLeft) &&
          assertTrue(Json.Str("\"-123456789012345678\"").as[Long].isLeft) &&
          assertTrue(Json.Str("\"123456789012345678\"").as[Long].isLeft) &&
          assertTrue(Json.Str("123456789012345678abc").as[Long].isLeft) &&
          assertTrue(Json.Str("+123456789012345678").as[Long].isLeft) &&
          assertTrue(Json.Str("Infinity").as[Long].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[Long].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[Long].isLeft) &&
          assertTrue(Json.Str("NaN").as[Long].isLeft)
        },
        test("float") {
          assert(Json.Num(Float.MinValue).as[Float])(isRight(equalTo(Float.MinValue))) &&
          assert(Json.Num(Float.MaxValue).as[Float])(isRight(equalTo(Float.MaxValue))) &&
          assert(Json.Str(Float.MinValue.toString).as[Float])(isRight(equalTo(Float.MinValue))) &&
          assert(Json.Str(Float.MaxValue.toString).as[Float])(isRight(equalTo(Float.MaxValue))) &&
          assert(Json.Str("Infinity").as[Float])(isRight(equalTo(Float.PositiveInfinity))) &&
          assert(Json.Str("+Infinity").as[Float])(isRight(equalTo(Float.PositiveInfinity))) &&
          assert(Json.Str("-Infinity").as[Float])(isRight(equalTo(Float.NegativeInfinity))) &&
          assertTrue(Json.Str("NaN").as[Float].isRight) &&
          assertTrue(Json.Str("\"-1.234567e9\"").as[Float].isLeft) &&
          assertTrue(Json.Str("\"1.234567e9\"").as[Float].isLeft) &&
          assertTrue(Json.Str("1.234567e9abc").as[Float].isLeft) &&
          assertTrue(Json.Str("+1.234567e9").as[Float].isLeft)
        },
        test("double") {
          assert(Json.Num(Double.MinValue).as[Double])(isRight(equalTo(Double.MinValue))) &&
          assert(Json.Num(Double.MaxValue).as[Double])(isRight(equalTo(Double.MaxValue))) &&
          assert(Json.Str(Double.MinValue.toString).as[Double])(isRight(equalTo(Double.MinValue))) &&
          assert(Json.Str(Double.MaxValue.toString).as[Double])(isRight(equalTo(Double.MaxValue))) &&
          assert(Json.Str("Infinity").as[Double])(isRight(equalTo(Double.PositiveInfinity))) &&
          assert(Json.Str("+Infinity").as[Double])(isRight(equalTo(Double.PositiveInfinity))) &&
          assert(Json.Str("-Infinity").as[Double])(isRight(equalTo(Double.NegativeInfinity))) &&
          assertTrue(Json.Str("NaN").as[Double].isRight) &&
          assertTrue(Json.Str("\"-1.23456789012345e9\"").as[Double].isLeft) &&
          assertTrue(Json.Str("\"1.23456789012345e9\"").as[Double].isLeft) &&
          assertTrue(Json.Str("1.23456789012345e9abc").as[Double].isLeft) &&
          assertTrue(Json.Str("+1.23456789012345e9").as[Double].isLeft)
        },
        test("BigDecimal") {
          assert(Json.Num(BigDecimal("-123.0e123")).as[BigDecimal])(isRight(equalTo(BigDecimal("-123.0e123")))) &&
          assert(Json.Num(BigDecimal("123.0e123")).as[BigDecimal])(isRight(equalTo(BigDecimal("123.0e123")))) &&
          assert(Json.Str("-123.0e123").as[BigDecimal])(isRight(equalTo(BigDecimal("-123.0e123")))) &&
          assert(Json.Str("123.0e123").as[BigDecimal])(isRight(equalTo(BigDecimal("123.0e123")))) &&
          assertTrue(Json.Str("123.0abc").as[BigDecimal].isLeft) &&
          assertTrue(Json.Str("Infinity").as[BigDecimal].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[BigDecimal].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[BigDecimal].isLeft) &&
          assertTrue(Json.Str("NaN").as[BigDecimal].isLeft) &&
          assert(
            Json
              .Str(
                "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851"
              )
              .as[BigDecimal]
          )(isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))) &&
          assert(Json.Str("1.23456789012345678901e-2147483648").as[BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert(Json.Str("12345678901234567890.1e+2147483647").as[BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert(Json.Str("123456789012345678901e+2147483647").as[BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          )
        },
        test("java.math.BigDecimal") {
          assert(Json.Num(BigDecimal("-123.0e123")).as[java.math.BigDecimal])(
            isRight(equalTo(new java.math.BigDecimal("-123.0e123")))
          ) &&
          assert(Json.Num(BigDecimal("123.0e123")).as[java.math.BigDecimal])(
            isRight(equalTo(new java.math.BigDecimal("123.0e123")))
          ) &&
          assert(Json.Str("-123.0e123").as[java.math.BigDecimal])(
            isRight(equalTo(new java.math.BigDecimal("-123.0e123")))
          ) &&
          assert(Json.Str("123.0e123").as[java.math.BigDecimal])(
            isRight(equalTo(new java.math.BigDecimal("123.0e123")))
          ) &&
          assertTrue(Json.Str("123.0abc").as[java.math.BigDecimal].isLeft) &&
          assertTrue(Json.Str("Infinity").as[java.math.BigDecimal].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[java.math.BigDecimal].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[java.math.BigDecimal].isLeft) &&
          assertTrue(Json.Str("NaN").as[java.math.BigDecimal].isLeft) &&
          assert(
            Json
              .Str(
                "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851"
              )
              .as[java.math.BigDecimal]
          )(isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))) &&
          assert(Json.Str("1.23456789012345678901e-2147483648").as[java.math.BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert(Json.Str("12345678901234567890.1e+2147483647").as[java.math.BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert(Json.Str("123456789012345678901e+2147483647").as[java.math.BigDecimal])(
            isLeft(equalTo("(expected a BigDecimal with 256-bit mantissa)"))
          )
        },
        test("BigInteger") {
          assert(Json.Num(BigInt("170141183460469231731687303715884105728")).as[BigInteger])(
            isRight(equalTo(new BigInteger("170141183460469231731687303715884105728")))
          ) &&
          assert(Json.Num(BigInt("-170141183460469231731687303715884105728")).as[BigInteger])(
            isRight(equalTo(new BigInteger("-170141183460469231731687303715884105728")))
          ) &&
          assert(Json.Str("170141183460469231731687303715884105728").as[BigInteger])(
            isRight(equalTo(new BigInteger("170141183460469231731687303715884105728")))
          ) &&
          assert(Json.Str("-170141183460469231731687303715884105728").as[BigInteger])(
            isRight(equalTo(new BigInteger("-170141183460469231731687303715884105728")))
          ) &&
          assertTrue(Json.Str("123abc").as[BigInteger].isLeft) &&
          assertTrue(Json.Str("Infinity").as[BigInteger].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[BigInteger].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[BigInteger].isLeft) &&
          assertTrue(Json.Str("NaN").as[BigInteger].isLeft) &&
          assert(
            Json
              .Str(
                "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851316546851"
              )
              .as[BigInteger]
          )(isLeft(equalTo("(expected a 256-bit BigInteger)"))) &&
          assert(
            Json
              .Str("17014118346046923173168730371588410572848946516548466848651357486465481896465316846")
              .as[BigInteger]
          )(isLeft(equalTo("(expected a 256-bit BigInteger)")))
        },
        test("BigInt") {
          assert(Json.Num(BigInt("170141183460469231731687303715884105728")).as[BigInt])(
            isRight(equalTo(BigInt("170141183460469231731687303715884105728")))
          ) &&
          assert(Json.Num(BigInt("-170141183460469231731687303715884105728")).as[BigInt])(
            isRight(equalTo(BigInt("-170141183460469231731687303715884105728")))
          ) &&
          assert(Json.Str("170141183460469231731687303715884105728").as[BigInt])(
            isRight(equalTo(BigInt("170141183460469231731687303715884105728")))
          ) &&
          assert(Json.Str("-170141183460469231731687303715884105728").as[BigInt])(
            isRight(equalTo(BigInt("-170141183460469231731687303715884105728")))
          ) &&
          assertTrue(Json.Str("123abc").as[BigInt].isLeft) &&
          assertTrue(Json.Str("Infinity").as[BigInt].isLeft) &&
          assertTrue(Json.Str("+Infinity").as[BigInt].isLeft) &&
          assertTrue(Json.Str("-Infinity").as[BigInt].isLeft) &&
          assertTrue(Json.Str("NaN").as[BigInt].isLeft) &&
          assert(
            Json
              .Str(
                "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851316546851"
              )
              .as[BigInt]
          )(isLeft(equalTo("(expected a 256-bit BigInt)"))) &&
          assert(
            Json.Str("17014118346046923173168730371588410572848946516548466848651357486465481896465316846").as[BigInt]
          )(isLeft(equalTo("(expected a 256-bit BigInt)")))
        },
        test("eithers") {
          val bernies =
            List(Json.Obj("a" -> Json.Num(1)), Json.Obj("left" -> Json.Num(1)), Json.Obj("Left" -> Json.Num(1)))
          val trumps =
            List(Json.Obj("b" -> Json.Num(2)), Json.Obj("right" -> Json.Num(2)), Json.Obj("Right" -> Json.Num(2)))
          assert(bernies.map(_.as[Either[Int, Int]]))(forall(isRight(isLeft(equalTo(1))))) &&
          assert(trumps.map(_.as[Either[Int, Int]]))(forall(isRight(isRight(equalTo(2)))))
        },
        test("parameterless products") {
          import exampleproducts._

          assert(Json.Obj().as[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert(Json.Null.as[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert(Json.Obj("field" -> Json.Str("value")).as[Parameterless])(isRight(equalTo(Parameterless())))
        },
        test("no extra fields") {
          import exampleproducts._

          assert(Json.Obj("s" -> Json.Str("")).as[OnlyString])(isRight(equalTo(OnlyString("")))) &&
          assert(Json.Obj("s" -> Json.Str(""), "t" -> Json.Str("")).as[OnlyString])(
            isLeft(equalTo("(invalid extra field)"))
          )
        },
        test("preserve error path") {
          import exampleproducts._

          assert(Json.Obj("is" -> Json.Arr(Json.Obj("str" -> Json.Num(1)))).as[Outer])(
            isLeft(equalTo(".is[0].str(expected string)"))
          )
        },
        test("errors are consistent with direct decoding") {
          assert("""{}""".fromJson[Message])(isLeft(equalTo(".v1(missing)"))) &&
          assert("""{}""".fromJson[Json].flatMap(_.as[Message]))(isLeft(equalTo(".v1(missing)"))) &&
          assert("""{"v1":"","v2":""}""".fromJson[Message])(
            isLeft(equalTo(".v1(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert("""{"v1":"","v2":""}""".fromJson[Json].flatMap(_.as[Message]))(
            isLeft(equalTo(".v1(expected a BigDecimal with 256-bit mantissa)"))
          ) &&
          assert("""{"v1":1,"v2":1}""".fromJson[Message])(
            isLeft(equalTo(".v2(expected string)"))
          ) &&
          assert("""{"v1":1,"v2":1}""".fromJson[Json].flatMap(_.as[Message]))(
            isLeft(equalTo(".v2(expected string)"))
          )
        },
        test("default field value") {
          import exampleproducts._

          assert(Json.Obj().as[DefaultString])(isRight(equalTo(DefaultString("")))) &&
          assert(Json.Obj("s" -> Json.Null).as[DefaultString])(isRight(equalTo(DefaultString(""))))
        },
        test("dynamic default value") {
          case class DefaultDynamic(
            randomNumber: Double = scala.math.random(),
            instant: java.time.Instant = java.time.Instant.now()
          )

          object DefaultDynamic {
            implicit lazy val decoder: JsonDecoder[DefaultDynamic] = DeriveJsonDecoder.gen[DefaultDynamic]
          }

          for {
            dynamics1 <- ZIO.fromEither(Json.Obj().as[DefaultDynamic])
            _         <- ZIO.sleep(2.millis) // ensure java.time.Instant is different
            dynamics2 <- ZIO.fromEither(Json.Obj().as[DefaultDynamic])
          } yield assertTrue(
            dynamics1.randomNumber != dynamics2.randomNumber,
            dynamics1.instant != dynamics2.instant
          )
        } @@ TestAspect.withLiveClock,
        test("aliases") {
          import exampleproducts._

          val expected = Aliases(a = 7, d = 15)
          assert(Json.Obj("a" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(isRight(equalTo(expected))) &&
          assert(Json.Obj("b" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(isRight(equalTo(expected))) &&
          assert(Json.Obj("c" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(isRight(equalTo(expected))) &&
          assert(Json.Obj("a" -> Json.Num(7), "b" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(
            isLeft(equalTo("(duplicate)"))
          ) &&
          assert(Json.Obj("b" -> Json.Num(7), "c" -> Json.Num(7), "d" -> Json.Num(15)).as[Aliases])(
            isLeft(equalTo("(duplicate)"))
          )
        },
        test("sum encoding") {
          import examplesum._

          assert(Json.Obj("Child1" -> Json.Obj()).as[Parent])(isRight(equalTo(Child1()))) &&
          assert(Json.Obj("Child2" -> Json.Obj()).as[Parent])(isRight(equalTo(Child2()))) &&
          assert(Json.Obj("type" -> Json.Str("Child1")).as[Parent])(isLeft(equalTo("(invalid disambiguator)")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert(Json.Obj("hint" -> Json.Str("Cain")).as[Parent])(isRight(equalTo(Child1()))) &&
          assert(Json.Obj("hint" -> Json.Str("Abel")).as[Parent])(isRight(equalTo(Child2()))) &&
          assert(Json.Obj("hint" -> Json.Str("Samson")).as[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert(Json.Obj("Cain" -> Json.Obj()).as[Parent])(isLeft(equalTo("(missing hint 'hint')")))
        },
        test("Seq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Seq("5XL", "2XL", "XL")

          assert(json.as[Seq[String]])(isRight(equalTo(expected)))
        },
        test("IndexedSeq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = IndexedSeq("5XL", "2XL", "XL")

          assert(json.as[IndexedSeq[String]])(isRight(equalTo(expected)))
        },
        test("LinearSeq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.LinearSeq("5XL", "2XL", "XL")

          assert(json.as[immutable.LinearSeq[String]])(isRight(equalTo(expected)))
        },
        test("ListSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.ListSet("5XL", "2XL", "XL")

          assert(json.as[immutable.ListSet[String]])(isRight(equalTo(expected)))
        },
        test("TreeSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.TreeSet("5XL", "2XL", "XL")

          assert(json.as[immutable.TreeSet[String]])(isRight(equalTo(expected)))
        },
        test("Vector") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Vector("5XL", "2XL", "XL")

          assert(json.as[Vector[String]])(isRight(equalTo(expected)))
        },
        test("SortedSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(json.as[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        },
        test("HashSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(json.as[immutable.HashSet[String]])(isRight(equalTo(expected)))
        },
        test("Set") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Set("5XL", "2XL", "XL")

          assert(json.as[Set[String]])(isRight(equalTo(expected)))
        },
        test("Map") {
          val json     = Json.Obj("5XL" -> Json.Num(3), "2XL" -> Json.Num(14), "XL" -> Json.Num(159))
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(json.as[Map[String, Int]])(isRight(equalTo(expected)))
        },
        test("SortedMap") {
          val json     = Json.Obj("5XL" -> Json.Num(3), "2XL" -> Json.Num(14), "XL" -> Json.Num(159))
          val expected = SortedMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(json.as[SortedMap[String, Int]])(isRight(equalTo(expected)))
        },
        test("ListMap") {
          val json     = Json.Obj("5XL" -> Json.Num(3), "2XL" -> Json.Num(14), "XL" -> Json.Num(159))
          val expected = immutable.ListMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(json.as[immutable.ListMap[String, Int]])(isRight(equalTo(expected)))
        },
        test("Map, custom keys") {
          val json     = Json.Obj("1" -> Json.Str("a"), "2" -> Json.Str("b"))
          val expected = Map(1 -> "a", 2 -> "b")

          assert(json.as[Map[Int, String]])(isRight(equalTo(expected)))
        },
        test("zio.Chunk") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Chunk("5XL", "2XL", "XL")

          assert(json.as[Chunk[String]])(isRight(equalTo(expected)))
        },
        test("zio.NonEmptyChunk") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assert(json.as[NonEmptyChunk[String]])(isRight(equalTo(expected)))
        },
        test("java.util.UUID") {
          val ok1  = Json.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f8")
          val ok2  = Json.Str("0000000064D7C38D-FD-14-32-70AFE4B0f8")
          val ok3  = Json.Str("0-0-0-0-0")
          val bad1 = Json.Str("")
          val bad2 = Json.Str("64d7c38d-2afd-4514-9832-4e70afe4b0f80")
          val bad3 = Json.Str("64d7c38d-2afd-4514-983-4e70afe4b0f80")
          val bad4 = Json.Str("64d7c38d-2afd--9832-4e70afe4b0f8")
          val bad5 = Json.Str("64d7c38d-2afd-XXXX-9832-4e70afe4b0f8")
          val bad6 = Json.Str("64d7c38d-2afd-X-9832-4e70afe4b0f8")
          val bad7 = Json.Str("0-0-0-0-00000000000000000")
          val bad8 = Json.Str("64d7c38d-2аfd-4514-9832-4e70afe4b0f8")
          val bad9 = Json.Str("0000000064D7C38D-FD-14-32-70АFE4B0f8")

          assert(ok1.as[UUID])(isRight(equalTo(UUID.fromString("64d7c38d-2afd-4514-9832-4e70afe4b0f8")))) &&
          assert(ok2.as[UUID])(isRight(equalTo(UUID.fromString("64D7C38D-00FD-0014-0032-0070AFE4B0f8")))) &&
          assert(ok3.as[UUID])(isRight(equalTo(UUID.fromString("00000000-0000-0000-0000-000000000000")))) &&
          assert(bad1.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad2.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad3.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad4.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad5.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad6.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad7.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad8.as[UUID])(isLeft(containsString("(expected a UUID)"))) &&
          assert(bad9.as[UUID])(isLeft(containsString("(expected a UUID)")))
        }
      )
    )

  object exampleproducts {

    case class Parameterless()

    object Parameterless {

      implicit val decoder: JsonDecoder[Parameterless] =
        DeriveJsonDecoder.gen[Parameterless]
    }

    @jsonNoExtraFields
    case class OnlyString(s: String)

    object OnlyString {

      implicit val decoder: JsonDecoder[OnlyString] =
        DeriveJsonDecoder.gen[OnlyString]
    }

    case class DefaultString(s: String = "")

    object DefaultString {

      implicit val decoder: JsonDecoder[DefaultString] =
        DeriveJsonDecoder.gen[DefaultString]
    }

    case class Inner(str: String)

    object Inner {
      implicit val decoder: JsonDecoder[Inner] = DeriveJsonDecoder.gen
    }

    case class Outer(is: Chunk[Inner])

    object Outer {
      implicit val decoder: JsonDecoder[Outer] = DeriveJsonDecoder.gen
    }

    case class Aliases(@jsonAliases("b", "c") a: Int, d: Int)

    object Aliases {
      implicit val decoder: JsonDecoder[Aliases] = DeriveJsonDecoder.gen
    }

  }

  object examplesum {

    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case class Child1() extends Parent

    case class Child2() extends Parent

  }

  object examplesumobjects1 {
    implicit val config: JsonCodecConfiguration =
      JsonCodecConfiguration(enumValuesAsStrings = true)

    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case object Child1 extends Parent

    case object Child2 extends Parent

  }

  object examplesumobjects2 {
    implicit val config: JsonCodecConfiguration =
      JsonCodecConfiguration(enumValuesAsStrings = false)

    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case object Child1 extends Parent

    case object Child2 extends Parent

  }

  object examplesumhintnames {

    @jsonHintNames(CamelCase)
    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case class Child1() extends Parent

    case class Child2() extends Parent

  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent

  }

  object examplealtsumhintnames {

    @jsonDiscriminator("hint")
    @jsonHintNames(CamelCase)
    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent

  }

  object logEvent {

    case class Event(at: Long, message: String, fatal: Boolean = false, priority: Double = 0.0)

    implicit val eventDecoder: JsonDecoder[Event] = DeriveJsonDecoder.gen[Event]
    implicit val eventEncoder: JsonEncoder[Event] = DeriveJsonEncoder.gen[Event]
  }

  object fieldDecoder {
    case class PersonId(value: String)

    object PersonId {
      implicit val jsonFieldEncoder: JsonFieldEncoder[PersonId] = JsonFieldEncoder.string.contramap(_.value)
      implicit val jsonFieldDecoder: JsonFieldDecoder[PersonId] = JsonFieldDecoder.string.map(PersonId.apply)
    }

    implicitly[JsonFieldEncoder[PersonId]]
    implicitly[JsonFieldDecoder[PersonId]]
  }

  case class Message(v1: math.BigDecimal, v2: String)

  object Message {
    implicit val decoder: JsonDecoder[Message] = DeriveJsonDecoder.gen[Message]
  }
}
