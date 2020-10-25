package testzio.json

import java.io.IOException

import io.circe
import testzio.json.TestUtils._
import testzio.json.data.geojson.generated._
import testzio.json.data.googlemaps._
import testzio.json.data.twitter._

import zio.Chunk
import zio.blocking.Blocking
import zio.json._
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, assert, _ }

// zioJsonJVM/testOnly testzio.json.EncoderSpec
object EncoderSpec extends DefaultRunnableSpec {
  def spec: Spec[Blocking, TestFailure[Throwable], TestSuccess] =
    suite("Encoder")(
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
        test("numerics") {
          assert((1: Byte).toJson)(equalTo("1")) &&
          assert((1: Short).toJson)(equalTo("1")) &&
          assert((1: Int).toJson)(equalTo("1")) &&
          assert((1L).toJson)(equalTo("1")) &&
          assert((new java.math.BigInteger("1")).toJson)(equalTo("1")) &&
          assert((new java.math.BigInteger("170141183460469231731687303715884105728")).toJson)(
            equalTo("170141183460469231731687303715884105728")
          ) &&
          assert((1.0f).toJson)(equalTo("1.0")) &&
          assert((1.0d).toJson)(equalTo("1.0"))
        },
        test("NaN / Infinity") {
          assert(Float.NaN.toJson)(equalTo("\"NaN\"")) &&
          assert(Float.PositiveInfinity.toJson)(equalTo("\"Infinity\"")) &&
          assert(Float.NegativeInfinity.toJson)(equalTo("\"-Infinity\"")) &&
          assert(Double.NaN.toJson)(equalTo("\"NaN\"")) &&
          assert(Double.PositiveInfinity.toJson)(equalTo("\"Infinity\"")) &&
          assert(Double.NegativeInfinity.toJson)(equalTo("\"-Infinity\""))
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
        assert(List[Int]().toJson)(equalTo("[]")) &&
        assert(List(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
        assert(Vector[Int]().toJson)(equalTo("[]")) &&
        assert(Vector(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
        assert(Map[String, String]().toJson)(equalTo("{}")) &&
        assert(Map("hello" -> "world").toJson)(equalTo("""{"hello":"world"}""")) &&
        assert(Map("hello" -> Some("world"), "goodbye" -> None).toJson)(equalTo("""{"hello":"world"}""")) &&
        assert(List[Int]().toJsonPretty)(equalTo("[]")) &&
        assert(List(1, 2, 3).toJsonPretty)(equalTo("[1, 2, 3]")) &&
        assert(Vector[Int]().toJsonPretty)(equalTo("[]")) &&
        assert(Vector(1, 2, 3).toJsonPretty)(equalTo("[1, 2, 3]")) &&
        assert(Map[String, String]().toJsonPretty)(equalTo("{}")) &&
        assert(Map("hello" -> "world").toJsonPretty)(equalTo("{\n  \"hello\" : \"world\"\n}")) &&
        assert(Map("hello" -> Some("world"), "goodbye" -> None).toJsonPretty)(equalTo("{\n  \"hello\" : \"world\"\n}"))
      },
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
      suite("roundtrip")(
        testRoundTrip[DistanceMatrix]("google_maps_api_response"),
        testRoundTrip[List[Tweet]]("twitter_api_response"),
        testRoundTrip[GeoJSON]("che.geo")
      ),
      suite("ZIO Streams integration")(
        testM("encodes into a ZStream of Char") {
          val intEncoder = JsonEncoder[Int]
          val value      = 1234

          for {
            chars <- intEncoder.encodeJsonStream(value, indent = None).runCollect
          } yield {
            assert(chars.mkString)(equalTo("1234"))
          }
        },
        testM("encodes values that yield a result of length > DefaultChunkSize") {
          val longString = List.fill(ZStream.DefaultChunkSize * 2)('x').mkString

          for {
            chars <- JsonEncoder[String].encodeJsonStream(longString, indent = None).runCollect
          } yield {
            assert(chars)(hasSize(equalTo(ZStream.DefaultChunkSize * 2 + 2))) &&
            assert(chars.mkString(""))(equalTo("\"" ++ longString ++ "\""))
          }
        },
        testM("encodeJsonLinesTransducer") {
          val ints = ZStream(1, 2, 3, 4)

          for {
            xs <- ints.transduce(JsonEncoder[Int].encodeJsonLinesTransducer).runCollect
          } yield {
            assert(xs.mkString)(equalTo("1\n2\n3\n4\n"))
          }
        },
        testM("encodeJsonLinesTransducer handles elements which take up > DefaultChunkSize to encode") {
          val longString = List.fill(5000)('x').mkString

          val ints    = ZStream(longString, longString)
          val encoder = JsonEncoder[String]

          for {
            xs <- ints.transduce(encoder.encodeJsonLinesTransducer).runCollect
          } yield {
            // leading `"`, trailing `"` and `\n` = 3
            assert(xs.size)(equalTo((5000 + 3) * 2))
          }
        },
        testM("encodeJsonArrayTransducer") {
          val ints = ZStream(1, 2, 3).map(n => Json.Obj(Chunk("id" -> Json.Num(BigDecimal(n).bigDecimal))))

          for {
            xs <- ints.transduce(JsonEncoder[Json].encodeJsonArrayTransducer).runCollect
          } yield {
            assert(xs.mkString)(equalTo("""[{"id":1},{"id":2},{"id":3}]"""))
          }
        }
      )
    )

  def testRoundTrip[A: circe.Decoder: JsonEncoder](label: String): ZSpec[Blocking, IOException] =
    testM(label) {
      getResourceAsStringM(s"$label.json").map { input =>
        val circeDecoded  = circe.parser.decode[A](input)
        val circeRecoded  = circeDecoded.toOption.get.toJson
        val recodedPretty = circeDecoded.toOption.get.toJson

        assert(circe.parser.decode[A](circeRecoded))(equalTo(circeDecoded)) &&
        assert(circe.parser.decode[A](recodedPretty))(equalTo(circeDecoded))
      }
    }

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
    case class OptionalAndRequired (i: Option[Int], s: String)
    object OptionalAndRequired {
      implicit val encoder: JsonEncoder[OptionalAndRequired] =
        DeriveJsonEncoder.gen[OptionalAndRequired]
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
}
