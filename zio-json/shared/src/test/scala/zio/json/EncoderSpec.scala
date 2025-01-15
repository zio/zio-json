package zio.json

import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect.jvmOnly
import zio.test._
import zio.{ Chunk, NonEmptyChunk }

import java.util.UUID
import scala.collection.{ immutable, mutable }

// zioJsonJVM/testOnly zio.json.EncoderSpec
object EncoderSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("Encoder")(
      suite("toJson")(
        suite("primitives")(
          test("strings") {
            assert("hello world".toJson)(equalTo("\"hello world\"")) &&
            assert("hello\nworld".toJson)(equalTo("\"hello\\nworld\"")) &&
            assert("hello\rworld".toJson)(equalTo("\"hello\\rworld\"")) &&
            assert("hello\u0000world".toJson)(equalTo("\"hello\\u0000world\""))
          },
          test("boolean") {
            assert(true.toJson)(equalTo("true")) &&
            assert(false.toJson)(equalTo("false"))
          },
          test("char") {
            assert('c'.toJson)(equalTo("\"c\"")) &&
            assert(Symbol("c").toJson)(equalTo("\"c\""))
          },
          test("float") {
            assert(Float.NaN.toJson)(equalTo("\"NaN\"")) &&
            assert(Float.PositiveInfinity.toJson)(equalTo("\"Infinity\"")) &&
            assert(Float.NegativeInfinity.toJson)(equalTo("\"-Infinity\"")) &&
            assert(0.0f.toJson)(equalTo("0.0")) &&
            assert((-0.0f).toJson)(equalTo("-0.0")) &&
            assert(1.0f.toJson)(equalTo("1.0")) &&
            assert((-1.0f).toJson)(equalTo("-1.0")) &&
            assert(1.0e7f.toJson)(equalTo("1.0E7")) &&
            assert((-1.0e7f).toJson)(equalTo("-1.0E7")) &&
            assert(1.17549435e-38f.toJson)(equalTo("1.1754944E-38")) && // subnormal
            assert(9999999.0f.toJson)(equalTo("9999999.0")) &&
            assert(0.001f.toJson)(equalTo("0.001")) &&
            assert(9.999999e-4f.toJson)(equalTo("9.999999E-4")) &&
            // FIXME: sbt fmt cannot parse: assert((-3.4028235E38f).toJson)(equalTo("-3.4028235E38")) && // Float.MinValue
            assert(1.4e-45f.toJson)(equalTo("1.4E-45")) && // Float.MinPositiveValue
            // FIXME: sbt fmt cannot parse: assert(3.4028235E38f.toJson)(equalTo("3.4028235E38")) && // Float.MaxValue
            assert(3.3554448e7f.toJson)(equalTo("3.355445E7")) &&
            assert(8.999999e9f.toJson)(equalTo("9.0E9")) &&
            assert(3.4366718e10f.toJson)(equalTo("3.436672E10")) &&
            assert(4.7223665e21f.toJson)(equalTo("4.7223665E21")) &&
            assert(8388608.0f.toJson)(equalTo("8388608.0")) &&
            assert(1.6777216e7f.toJson)(equalTo("1.6777216E7")) &&
            assert(3.3554436e7f.toJson)(equalTo("3.3554436E7")) &&
            assert(6.7131496e7f.toJson)(equalTo("6.7131496E7")) &&
            assert(1.9310392e-38f.toJson)(equalTo("1.9310392E-38")) &&
            assert((-2.47e-43f).toJson)(equalTo("-2.47E-43")) &&
            assert(1.993244e-38f.toJson)(equalTo("1.993244E-38")) &&
            assert(4103.9004f.toJson)(equalTo("4103.9004")) &&
            assert(5.3399997e9f.toJson)(equalTo("5.3399997E9")) &&
            assert(6.0898e-39f.toJson)(equalTo("6.0898E-39")) &&
            assert(0.0010310042f.toJson)(equalTo("0.0010310042")) &&
            assert(2.8823261e17f.toJson)(equalTo("2.882326E17")) &&
            assert(7.038531e-26f.toJson)(equalTo("7.038531E-26")) &&
            assert(9.2234038e17f.toJson)(equalTo("9.223404E17")) &&
            assert(6.7108872e7f.toJson)(equalTo("6.710887E7")) &&
            assert(1.0e-44f.toJson)(equalTo("9.8E-45")) &&
            assert(2.816025e14f.toJson)(equalTo("2.816025E14")) &&
            assert(9.223372e18f.toJson)(equalTo("9.223372E18")) &&
            assert(1.5846086e29f.toJson)(equalTo("1.5846086E29")) &&
            assert(1.1811161e19f.toJson)(equalTo("1.1811161E19")) &&
            assert(5.368709e18f.toJson)(equalTo("5.368709E18")) &&
            assert(4.6143166e18f.toJson)(equalTo("4.6143166E18")) &&
            assert(0.007812537f.toJson)(equalTo("0.007812537")) &&
            assert(1.4e-45f.toJson)(equalTo("1.4E-45")) &&
            assert(1.18697725e20f.toJson)(equalTo("1.18697725E20")) &&
            assert(1.00014165e-36f.toJson)(equalTo("1.00014165E-36")) &&
            assert(200.0f.toJson)(equalTo("200.0")) &&
            assert(3.3554432e7f.toJson)(equalTo("3.3554432E7")) &&
            assert(1.26217745e-29f.toJson)(equalTo("1.2621775E-29")) &&
            assert(1.0e-43f.toJson)(equalTo("9.9E-44")) && // 71 * 2 ^ -149 == 9.94... * 10 ^ -44
            assert(1.0e-45f.toJson)(equalTo("1.4E-45")) && // 1 * 2 ^ -149 == 1.40... * 10 ^ -45
            assert(7.1e10f.toJson)(
              equalTo("7.1E10")
            ) && // Java serializes it to "7.0999998E10" (string of redundant 9s)
            assert(1.1e15f.toJson)(
              equalTo("1.1E15")
            ) && // Java serializes it to "1.09999998E15" (string of redundant 9s)
            assert(1.0e17f.toJson)(
              equalTo("1.0E17")
            ) &&                                       // Java serializes it to "9.9999998E16" (string of redundant 9s)
            assert(6.3e9f.toJson)(equalTo("6.3E9")) && // Java serializes it to "6.3000003E9" (string of redundant 0s)
            assert(3.0e10f.toJson)(
              equalTo("3.0E10")
            ) && // Java serializes it to "3.0000001E10" (string of redundant 0s)
            assert(1.1e10f.toJson)(
              equalTo("1.1E10")
            ) && // Java serializes it to "1.10000005E10" (string of redundant 0s)
            assert((-6.9390464e7f).toJson)(
              equalTo("-6.939046E7")
            ) && // See the issue: https://github.com/zio/zio-json/pull/375
            assert((-6939.0464f).toJson)(
              equalTo("-6939.0464")
            ) // See the issue: https://github.com/zio/zio-json/pull/375
          },
          test("double") {
            assert(Double.NaN.toJson)(equalTo("\"NaN\"")) &&
            assert(Double.PositiveInfinity.toJson)(equalTo("\"Infinity\"")) &&
            assert(Double.NegativeInfinity.toJson)(equalTo("\"-Infinity\"")) &&
            assert(0.0d.toJson)(equalTo("0.0")) &&
            assert((-0.0d).toJson)(equalTo("-0.0")) &&
            assert(1.0d.toJson)(equalTo("1.0")) &&
            assert((-1.0d).toJson)(equalTo("-1.0")) &&
            assert(2.2250738585072014e-308d.toJson)(equalTo("2.2250738585072014E-308")) && // subnormal
            assert(1.0e7d.toJson)(equalTo("1.0E7")) &&
            assert((-1.0e7d).toJson)(equalTo("-1.0E7")) &&
            assert(9999999.999999998d.toJson)(equalTo("9999999.999999998")) &&
            assert(0.001d.toJson)(equalTo("0.001")) &&
            assert(9.999999999999998e-4d.toJson)(equalTo("9.999999999999998E-4")) &&
            assert((-1.7976931348623157e308d).toJson)(equalTo("-1.7976931348623157E308")) && // Double.MinValue
            assert(4.9e-324d.toJson)(equalTo("4.9E-324")) &&                                 // Double.MinPositiveValue
            assert(1.7976931348623157e308d.toJson)(equalTo("1.7976931348623157E308")) &&     // Double.MaxValue
            assert((-2.109808898695963e16d).toJson)(equalTo("-2.109808898695963E16")) &&
            assert(4.940656e-318d.toJson)(equalTo("4.940656E-318")) &&
            assert(1.18575755e-316d.toJson)(equalTo("1.18575755E-316")) &&
            assert(2.989102097996e-312d.toJson)(equalTo("2.989102097996E-312")) &&
            assert(9.0608011534336e15d.toJson)(equalTo("9.0608011534336E15")) &&
            assert(4.708356024711512e18d.toJson)(equalTo("4.708356024711512E18")) &&
            assert(9.409340012568248e18d.toJson)(equalTo("9.409340012568248E18")) &&
            assert(1.8531501765868567e21d.toJson)(equalTo("1.8531501765868567E21")) &&
            assert((-3.347727380279489e33d).toJson)(equalTo("-3.347727380279489E33")) &&
            assert(1.9430376160308388e16d.toJson)(equalTo("1.9430376160308388E16")) &&
            assert((-6.9741824662760956e19d).toJson)(equalTo("-6.9741824662760956E19")) &&
            assert(4.3816050601147837e18d.toJson)(equalTo("4.3816050601147837E18")) &&
            assert(7.1202363472230444e-307d.toJson)(equalTo("7.120236347223045E-307")) &&
            assert(3.67301024534615e16d.toJson)(equalTo("3.67301024534615E16")) &&
            assert(5.9604644775390625e-8d.toJson)(equalTo("5.960464477539063E-8")) &&
            assert(1.0e-322d.toJson)(equalTo("9.9E-323")) && // 20 * 2 ^ -1074 == 9.88... * 10 ^ -323
            assert(5.0e-324d.toJson)(equalTo("4.9E-324")) && // 1 * 2 ^ -1074 == 4.94... * 10 ^ -324
            assert(1.0e23d.toJson)(
              equalTo("1.0E23")
            ) && // Java serializes it to "9.999999999999999E22" (string of redundant 9s)
            assert(8.41e21d.toJson)(
              equalTo("8.41E21")
            ) && // Java serializes it to "8.409999999999999E21" (string of redundant 9s)
            assert(7.3879e20d.toJson)(
              equalTo("7.3879E20")
            ) && // Java serializes it to "7.387900000000001E20" (string of redundant 0s)
            assert(3.1e22d.toJson)(
              equalTo("3.1E22")
            ) && // Java serializes it to "3.1000000000000002E22" (string of redundant 0s)
            assert(5.63e21d.toJson)(
              equalTo("5.63E21")
            ) && // Java serializes it to "5.630000000000001E21" (string of redundant 0s)
            assert(2.82879384806159e17d.toJson)(
              equalTo("2.82879384806159E17")
            ) && // Java serializes it to "2.82879384806159008E17" (18 digits, even though 17 digits are *always* enough)
            assert(1.387364135037754e18d.toJson)(
              equalTo("1.387364135037754E18")
            ) && // Java serializes it to "1.38736413503775411E18" (18 digits, even though 17 digits are *always* enough)
            assert(1.45800632428665e17d.toJson)(
              equalTo("1.45800632428665E17")
            ) && // Java serializes it to "1.45800632428664992E17" (18 digits, even though 17 digits are *always* enough)
            assert(1.790086667993e18d.toJson)(
              equalTo("1.790086667993E18")
            ) && // Java serializes it to "1.79008666799299994E18" (5 digits too much)
            assert(2.273317134858e18d.toJson)(
              equalTo("2.273317134858E18")
            ) && // Java serializes it to "2.27331713485799987E18" (5 digits too much)
            assert(7.68905065813e17d.toJson)(
              equalTo("7.68905065813E17")
            ) && // Java serializes it to "7.6890506581299994E17" (5 digits too much)
            assert(1.9400994884341945e25d.toJson)(
              equalTo("1.9400994884341945E25")
            ) && // Java serializes it to "1.9400994884341944E25" (not the closest to the intermediate double)
            assert(3.6131332396758635e25d.toJson)(
              equalTo("3.6131332396758635E25")
            ) && // Java serializes it to "3.6131332396758634E25" (not the closest to the intermediate double)
            assert(2.5138990223946153e25d.toJson)(
              equalTo("2.5138990223946153E25")
            ) && // Java serializes it to "2.5138990223946152E25" (not the closest to the intermediate double)
            assert((-3.644554028000364e16d).toJson)(
              equalTo("-3.644554028000364E16")
            ) && // See the issue: https://github.com/zio/zio-json/pull/375
            assert((-6939.0464d).toJson)(
              equalTo("-6939.0464")
            ) // See the issue: https://github.com/zio/zio-json/pull/375
          },
          test("other numerics") {
            val exampleBigIntStr     = "170141183460469231731687303715884105728"
            val exampleBigDecimalStr = "170141183460469231731687303715884105728.4433"
            assert((1: Byte).toJson)(equalTo("1")) &&
            assert((1: Short).toJson)(equalTo("1")) &&
            assert((1: Int).toJson)(equalTo("1")) &&
            assert(1L.toJson)(equalTo("1")) &&
            assert(new java.math.BigInteger("1").toJson)(equalTo("1")) &&
            assert(new java.math.BigInteger(exampleBigIntStr).toJson)(equalTo(exampleBigIntStr)) &&
            assert(BigInt(exampleBigIntStr).toJson)(equalTo(exampleBigIntStr)) &&
            assert(BigDecimal(exampleBigDecimalStr).toJson)(equalTo(exampleBigDecimalStr))
          }
        ),
        test("options") {
          assert((None: Option[Int]).toJson)(equalTo("null")) &&
          assert((Some(1): Option[Int]).toJson)(equalTo("1"))
        },
        test("eithers") {
          assert((Left(1): Either[Int, Int]).toJson)(equalTo("""{"Left":1}""")) &&
          assert((Right(1): Either[Int, Int]).toJson)(equalTo("""{"Right":1}""")) &&
          assert((Left(1): Either[Int, Int]).toJsonPretty)(equalTo("{\n  \"Left\" : 1\n}")) &&
          assert((Right(1): Either[Int, Int]).toJsonPretty)(equalTo("{\n  \"Right\" : 1\n}"))
        },
        test("collections") {
          assert(Chunk[Int]().toJson)(equalTo("[]")) &&
          assert(Chunk(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(NonEmptyChunk(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(List[Int]().toJson)(equalTo("[]")) &&
          assert(List(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Vector[Int]().toJson)(equalTo("[]")) &&
          assert(Vector(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Seq[Int]().toJson)(equalTo("[]")) &&
          assert(Seq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(IndexedSeq[Int]().toJson)(equalTo("[]")) &&
          assert(IndexedSeq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.LinearSeq[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.LinearSeq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.ListSet[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.ListSet(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.TreeSet[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.TreeSet(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Array[Int]().toJson)(equalTo("[]")) &&
          assert(Array(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Map[String, String]().toJson)(equalTo("{}")) &&
          assert(Map("hello" -> "world").toJson)(equalTo("""{"hello":"world"}""")) &&
          assert(mutable.Map("hello" -> "world").toJson)(equalTo("""{"hello":"world"}""")) &&
          assert(Map("hello" -> Some("world"), "goodbye" -> None).toJson)(equalTo("""{"hello":"world"}""")) &&
          assert(List[Int]().toJsonPretty)(equalTo("[]")) &&
          assert(List(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
          assert(Vector[Int]().toJsonPretty)(equalTo("[]")) &&
          assert(Vector(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
          assert(Seq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(Seq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(IndexedSeq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(IndexedSeq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(immutable.LinearSeq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.LinearSeq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(immutable.ListSet[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.ListSet("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(immutable.TreeSet[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.TreeSet("bar", "foo").toJsonPretty)(equalTo("[\n  \"bar\",\n  \"foo\"\n]")) &&
          assert(Array[Int]().toJsonPretty)(equalTo("[]")) &&
          assert(Array(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
          assert(Map[String, String]().toJsonPretty)(equalTo("{}")) &&
          assert(Map("hello" -> "world").toJsonPretty)(equalTo("{\n  \"hello\" : \"world\"\n}")) &&
          assert(Map("hello" -> Some("world"), "goodbye" -> None).toJsonPretty)(
            equalTo("{\n  \"hello\" : \"world\"\n}")
          ) &&
          assert(immutable.ListMap("hello" -> "world", "goodbye" -> "world").toJson)(
            equalTo("""{"hello":"world","goodbye":"world"}""")
          )
        },
        test("Map, custom keys") {
          assert(Map(1 -> "a").toJson)(equalTo("""{"1":"a"}"""))
        },
        test("Map, UUID keys") {
          assert(Map(UUID.fromString("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd") -> "abcd").toJson)(
            equalTo("""{"e142f1aa-6e9e-4352-adfe-7e6eb9814ccd":"abcd"}""")
          )
        },
        test("java.util.UUID") {
          assert(UUID.fromString("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd").toJson)(
            equalTo(""""e142f1aa-6e9e-4352-adfe-7e6eb9814ccd"""")
          )
        },
        test("java.util.Currency") {
          assert(java.util.Currency.getInstance("USD").toJson)(equalTo(""""USD""""))
        } @@ jvmOnly,
        test("parameterless products") {
          import exampleproducts._

          assert(Parameterless().toJson)(equalTo("{}")) &&
          assert(Parameterless().toJsonPretty)(equalTo("{}"))
        },
        test("tuples") {
          assert(("hello", "world").toJson)(equalTo("""["hello","world"]""")) &&
          assert(("hello", "world").toJsonPretty)(equalTo("""["hello", "world"]"""))
        },
        test("products") {
          import exampleproducts._

          assert(OnlyString("foo").toJson)(equalTo("""{"s":"foo"}""")) &&
          assert(CoupleOfThings(-1, Some(10.0f), false).toJson)(equalTo("""{"j":-1,"f":10.0,"b":false}""")) &&
          assert(CoupleOfThings(0, None, true).toJson)(equalTo("""{"j":0,"b":true}""")) &&
          assert(OnlyString("foo").toJsonPretty)(equalTo("{\n  \"s\" : \"foo\"\n}")) &&
          assert(CoupleOfThings(-1, Some(10.0f), false).toJsonPretty)(
            equalTo("{\n  \"j\" : -1,\n  \"f\" : 10.0,\n  \"b\" : false\n}")
          ) &&
          assert(CoupleOfThings(0, None, true).toJsonPretty)(equalTo("{\n  \"j\" : 0,\n  \"b\" : true\n}")) &&
          assert(OptionalAndRequired(None, "foo").toJson)(equalTo("""{"s":"foo"}"""))
          assert(OptionalExplicitNullAndRequired(None, "foo").toJson)(equalTo("""{"i":null,"s":"foo"}"""))
        },
        test("sum encoding") {
          import examplesum._

          assert((Child1(): Parent).toJson)(equalTo("""{"Child1":{}}""")) &&
          assert((Child2(): Parent).toJson)(equalTo("""{"Cain":{}}""")) &&
          assert((Child1(): Parent).toJsonPretty)(equalTo("{\n  \"Child1\" : {}\n}")) &&
          assert((Child2(): Parent).toJsonPretty)(equalTo("{\n  \"Cain\" : {}\n}"))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          // note lack of whitespace on last line
          assert((Child1(): Parent).toJson)(equalTo("""{"hint":"Child1"}""")) &&
          assert((Child2(None): Parent).toJson)(equalTo("""{"hint":"Abel"}""")) &&
          assert((Child2(Some("hello")): Parent).toJson)(equalTo("""{"hint":"Abel","s":"hello"}""")) &&
          assert((Child1(): Parent).toJsonPretty)(equalTo("{\n  \"hint\" : \"Child1\"}")) &&
          assert((Child2(None): Parent).toJsonPretty)(equalTo("{\n  \"hint\" : \"Abel\"}")) &&
          assert((Child2(Some("hello")): Parent).toJsonPretty)(
            equalTo("{\n  \"hint\" : \"Abel\",\n  \"s\" : \"hello\"\n}")
          )
        },
        test("exclude fields") {
          import exampleexcludefield._
          assert(Person(7, "Peter", 20).toJson)(equalTo("""{"name":"Peter"}"""))
        },
        test("aliases") {
          import exampleproducts._
          assert(Aliases(-1, "a").toJson)(equalTo("""{"i":-1,"f":"a"}"""))
        }
      ),
      suite("toJsonAST")(
        suite("primitives")(
          test("strings") {
            assert("hello world".toJsonAST)(isRight(equalTo(Json.Str("hello world"))))
          },
          test("boolean") {
            assert(true.toJsonAST)(isRight(equalTo(Json.Bool(true)))) &&
            assert(false.toJsonAST)(isRight(equalTo(Json.Bool(false))))
          },
          test("char") {
            assert('c'.toJsonAST)(isRight(equalTo(Json.Str("c"))))
          },
          test("numerics") {
            assert((1: Byte).toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert((1: Short).toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert((1: Int).toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(1L.toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(new java.math.BigInteger("1").toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(1.0f.toJsonAST)(isRight(equalTo(Json.Num(1.0f)))) &&
            assert(1.0d.toJsonAST)(isRight(equalTo(Json.Num(1.0))))
          }
        ),
        test("options") {
          assert((None: Option[Int]).toJsonAST)(isRight(equalTo(Json.Null))) &&
          assert((Some(1): Option[Int]).toJsonAST)(isRight(equalTo(Json.Num(1))))
        },
        test("eithers") {
          assert((Left(1): Either[Int, Int]).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Left" -> Json.Num(1)))))) &&
          assert((Right(1): Either[Int, Int]).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Right" -> Json.Num(1))))))
        },
        test("collections") {
          val arrEmpty = Json.Arr()
          val arr123   = Json.Arr(Json.Num(1), Json.Num(2), Json.Num(3))
          val objEmpty = Json.Obj()
          val objHW    = Json.Obj("hello" -> Json.Str("world"))

          assert(Chunk[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(Chunk(1, 2, 3).toJsonAST)(isRight(equalTo(arr123))) &&
          assert(NonEmptyChunk(1, 2, 3).toJsonAST)(isRight(equalTo(arr123))) &&
          assert(List[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(List(1, 2, 3).toJsonAST)(isRight(equalTo(arr123))) &&
          assert(Vector[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(Vector(1, 2, 3).toJsonAST)(isRight(equalTo(arr123))) &&
          assert(Map[String, String]().toJsonAST)(isRight(equalTo(objEmpty))) &&
          assert(Map("hello" -> "world").toJsonAST)(isRight(equalTo(objHW))) &&
          assert(Map("hello" -> Some("world"), "goodbye" -> None).toJsonAST)(isRight(equalTo(objHW)))
        },
        test("java.util.UUID") {
          assert(UUID.fromString("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd").toJsonAST)(
            isRight(equalTo(Json.Str("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd")))
          )
        },
        test("parameterless products") {
          import exampleproducts._

          assert(Parameterless().toJsonAST)(isRight(equalTo(Json.Obj())))
        },
        test("tuples") {
          assert(("hello", "world").toJsonAST)(isRight(equalTo(Json.Arr(Json.Str("hello"), Json.Str("world")))))
        },
        test("products") {
          import exampleproducts._

          assert(OnlyString("foo").toJsonAST)(isRight(equalTo(Json.Obj("s" -> Json.Str("foo"))))) &&
          assert(CoupleOfThings(-1, Some(10.0f), false).toJsonAST)(
            isRight(equalTo(Json.Obj("j" -> Json.Num(-1), "f" -> Json.Num(10.0f), "b" -> Json.Bool(false))))
          ) &&
          assert(CoupleOfThings(0, None, true).toJsonAST)(
            isRight(equalTo(Json.Obj("j" -> Json.Num(0), "b" -> Json.Bool(true))))
          ) &&
          assert(OptionalAndRequired(None, "foo").toJsonAST)(isRight(equalTo(Json.Obj("s" -> Json.Str("foo")))))
        },
        test("sum encoding") {
          import examplesum._

          assert((Child1(): Parent).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Child1" -> Json.Obj()))))) &&
          assert((Child2(): Parent).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Cain" -> Json.Obj())))))
        },
        test("sum encoding with hint names") {
          import examplesumhintnames._

          assert((Child1(): Parent).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("child1" -> Json.Obj()))))) &&
          assert((Child2(): Parent).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Cain" -> Json.Obj())))))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert((Child1(): Parent).toJsonAST)(isRight(equalTo(Json.Obj("hint" -> Json.Str("Child1"))))) &&
          assert((Child2(None): Parent).toJsonAST)(isRight(equalTo(Json.Obj("hint" -> Json.Str("Abel"))))) &&
          assert((Child2(Some("hello")): Parent).toJsonAST)(
            (isRight(equalTo(Json.Obj("s" -> Json.Str("hello"), "hint" -> Json.Str("Abel")))))
          )
        },
        test("sum alternative encoding with hint names") {
          import examplealtsumhintnames._

          assert((Child1(): Parent).toJsonAST)(isRight(equalTo(Json.Obj("hint" -> Json.Str("child1"))))) &&
          assert((Child2(None): Parent).toJsonAST)(isRight(equalTo(Json.Obj("hint" -> Json.Str("Abel"))))) &&
          assert((Child2(Some("hello")): Parent).toJsonAST)(
            (isRight(equalTo(Json.Obj("s" -> Json.Str("hello"), "hint" -> Json.Str("Abel")))))
          )
        },
        test("alias") {
          import exampleproducts._

          assert(Aliases(-1, "a").toJsonAST)(
            isRight(equalTo(Json.Obj("i" -> Json.Num(-1), "f" -> Json.Str("a"))))
          )
        }
      )
    )

  object exampleproducts {

    case class Parameterless()

    object Parameterless {

      implicit val encoder: JsonEncoder[Parameterless] =
        DeriveJsonEncoder.gen[Parameterless]
    }

    case class OnlyString(s: String)

    object OnlyString {

      implicit val encoder: JsonEncoder[OnlyString] =
        DeriveJsonEncoder.gen[OnlyString]
    }

    case class CoupleOfThings(@jsonField("j") i: Int, f: Option[Float], b: Boolean)

    object CoupleOfThings {

      implicit val encoder: JsonEncoder[CoupleOfThings] =
        DeriveJsonEncoder.gen[CoupleOfThings]
    }

    case class OptionalAndRequired(i: Option[Int], s: String)

    object OptionalAndRequired {

      implicit val encoder: JsonEncoder[OptionalAndRequired] =
        DeriveJsonEncoder.gen[OptionalAndRequired]
    }

    @jsonExplicitNull
    case class OptionalExplicitNullAndRequired(i: Option[Int], s: String)

    object OptionalExplicitNullAndRequired {

      implicit val encoder: JsonEncoder[OptionalExplicitNullAndRequired] =
        DeriveJsonEncoder.gen[OptionalExplicitNullAndRequired]
    }

    case class Aliases(@jsonAliases("j", "k") i: Int, f: String)

    object Aliases {

      implicit val encoder: JsonEncoder[Aliases] =
        DeriveJsonEncoder.gen[Aliases]
    }

  }

  object exampleexcludefield {

    case class Person(@jsonExclude id: Long, name: String, @jsonExclude age: Int)

    object Person {
      implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
    }

  }

  object examplesum {

    sealed abstract class Parent

    object Parent {
      implicit val encoder: JsonEncoder[Parent] = DeriveJsonEncoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Cain")
    case class Child2() extends Parent

  }

  object examplesumhintnames {

    @jsonHintNames(CamelCase)
    sealed abstract class Parent

    object Parent {
      implicit val encoder: JsonEncoder[Parent] = DeriveJsonEncoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Cain")
    case class Child2() extends Parent

  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    sealed abstract class Parent

    object Parent {
      implicit val encoder: JsonEncoder[Parent] = DeriveJsonEncoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2(s: Option[String]) extends Parent

  }

  object examplealtsumhintnames {

    @jsonDiscriminator("hint")
    @jsonHintNames(CamelCase)
    sealed abstract class Parent

    object Parent {
      implicit val encoder: JsonEncoder[Parent] = DeriveJsonEncoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2(s: Option[String]) extends Parent

  }

}
