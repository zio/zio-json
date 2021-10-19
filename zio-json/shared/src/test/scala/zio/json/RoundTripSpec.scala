package testzio.json

import testzio.json.Gens._
import zio.json._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.time._
import zio.{Has, Random}
import zio.test.Sized

object RoundTripSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("RoundTrip")(
      test("booleans") {
        check(Gen.boolean)(assertRoundtrips)
      },
      test("bytes") {
        check(Gen.anyByte)(assertRoundtrips)
      },
      test("shorts") {
        check(Gen.anyShort)(assertRoundtrips)
      } @@ samples(10000),
      test("ints") {
        check(Gen.anyInt)(assertRoundtrips)
      } @@ samples(10000),
      test("longs") {
        check(Gen.anyLong)(assertRoundtrips)
      } @@ samples(10000),
      test("bigInts") {
        check(genBigInteger)(assertRoundtrips)
      } @@ samples(10000),
      test("floats") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyFloat.filter(java.lang.Float.isFinite))(assertRoundtrips)
      } @@ samples(10000),
      test("doubles") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyDouble.filter(java.lang.Double.isFinite))(assertRoundtrips)
      } @@ samples(10000),
      test("AST") {
        check(genAst)(assertRoundtrips)
      },
      suite("java.time")(
        test("DayOfWeek") {
          check(genDayOfWeek)(assertRoundtrips)
        },
        test("Duration") {
          check(genDuration)(assertRoundtrips)
        } @@ samples(10000),
        test("Instant") {
          check(genInstant)(assertRoundtrips)
        } @@ samples(10000),
        test("LocalDate") {
          check(genLocalDate)(assertRoundtrips)
        } @@ samples(10000),
        test("LocalDateTime") {
          check(genLocalDateTime)(assertRoundtrips)
        } @@ samples(10000),
        test("LocalTime") {
          check(genLocalTime)(assertRoundtrips)
        } @@ samples(10000),
        test("Month") {
          check(genMonth)(assertRoundtrips)
        },
        test("MonthDay") {
          check(genMonthDay)(assertRoundtrips)
        },
        test("OffsetDateTime") {
          check(genOffsetDateTime)(assertRoundtrips)
        } @@ samples(10000),
        test("OffsetTime") {
          check(genOffsetTime)(assertRoundtrips)
        } @@ samples(10000),
        test("Period") {
          check(genPeriod)(assertRoundtrips)
        } @@ samples(10000),
        test("Year") {
          check(genYear)(assertRoundtrips)
        } @@ samples(10000),
        test("YearMonth") {
          check(genYearMonth)(assertRoundtrips)
        } @@ samples(10000),
        test("ZonedDateTime") {
          check(genZonedDateTime)(assertRoundtrips)
        } @@ samples(10000),
        test("ZoneId") {
          check(genZoneId)(assertRoundtrips[ZoneId])
        },
        test("ZoneOffset") {
          check(genZoneOffset)(assertRoundtrips[ZoneOffset])
        }
      )
    )

  lazy val genAst: Gen[Has[Random] with Has[Sized], Json] =
    Gen.size.flatMap { size =>
      val entry = genUsAsciiString <*> genAst
      val sz    = 0 min (size - 1)
      val obj   = Gen.chunkOfN(sz)(entry).map(Json.Obj(_))
      val arr   = Gen.chunkOfN(sz)(genAst).map(Json.Arr(_))
      val boo   = Gen.boolean.map(Json.Bool(_))
      val str   = genUsAsciiString.map(Json.Str(_))
      val num   = genBigDecimal.map(Json.Num(_))
      val nul   = Gen.const(Json.Null)

      Gen.oneOf(obj, arr, boo, str, num, nul)
    }

  private def assertRoundtrips[A: JsonEncoder: JsonDecoder](a: A) =
    assert(a.toJson.fromJson[A])(isRight(equalTo(a))) &&
      assert(a.toJsonPretty.fromJson[A])(isRight(equalTo(a)))
}
