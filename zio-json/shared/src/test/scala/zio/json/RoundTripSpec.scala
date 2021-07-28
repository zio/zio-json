package testzio.json

import testzio.json.Gens._
import zio.json._
import zio.json.ast.Json
import zio.random.Random
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.time._

object RoundTripSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("RoundTrip")(
      testM("booleans") {
        check(Gen.boolean)(assertRoundtrips)
      },
      testM("bytes") {
        check(Gen.anyByte)(assertRoundtrips)
      },
      testM("shorts") {
        check(Gen.anyShort)(assertRoundtrips)
      } @@ samples(10000),
      testM("ints") {
        check(Gen.anyInt)(assertRoundtrips)
      } @@ samples(10000),
      testM("longs") {
        check(Gen.anyLong)(assertRoundtrips)
      } @@ samples(10000),
      testM("bigInts") {
        check(genBigInteger)(assertRoundtrips)
      } @@ samples(10000),
      testM("floats") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyFloat.filter(java.lang.Float.isFinite))(assertRoundtrips)
      } @@ samples(10000),
      testM("doubles") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyDouble.filter(java.lang.Double.isFinite))(assertRoundtrips)
      } @@ samples(10000),
      testM("AST") {
        check(genAst)(assertRoundtrips)
      },
      suite("java.time")(
        testM("DayOfWeek") {
          check(genDayOfWeek)(assertRoundtrips)
        },
        testM("Duration") {
          check(genDuration)(assertRoundtrips)
        } @@ samples(10000),
        testM("Instant") {
          check(genInstant)(assertRoundtrips)
        } @@ samples(10000),
        testM("LocalDate") {
          check(genLocalDate)(assertRoundtrips)
        } @@ samples(10000),
        testM("LocalDateTime") {
          check(genLocalDateTime)(assertRoundtrips)
        } @@ samples(10000),
        testM("LocalTime") {
          check(genLocalTime)(assertRoundtrips)
        } @@ samples(10000),
        testM("Month") {
          check(genMonth)(assertRoundtrips)
        },
        testM("MonthDay") {
          check(genMonthDay)(assertRoundtrips)
        },
        testM("OffsetDateTime") {
          check(genOffsetDateTime)(assertRoundtrips)
        } @@ samples(10000),
        testM("OffsetTime") {
          check(genOffsetTime)(assertRoundtrips)
        } @@ samples(10000),
        testM("Period") {
          check(genPeriod)(assertRoundtrips)
        } @@ samples(10000),
        testM("Year") {
          check(genYear)(assertRoundtrips)
        } @@ samples(10000),
        testM("YearMonth") {
          check(genYearMonth)(assertRoundtrips)
        } @@ samples(10000),
        testM("ZonedDateTime") {
          check(genZonedDateTime)(assertRoundtrips)
        } @@ samples(10000),
        testM("ZoneId") {
          check(genZoneId)(assertRoundtrips[ZoneId])
        },
        testM("ZoneOffset") {
          check(genZoneOffset)(assertRoundtrips[ZoneOffset])
        }
      )
    )

  lazy val genAst: Gen[Random with Sized, Json] =
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
