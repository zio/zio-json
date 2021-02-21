package testzio.json

import java.time._

import testzio.json.TestUtils._

import zio.json._
import zio.json.ast.Json
import zio.random.Random
import zio.test.Assertion._
import zio.test._

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
      },
      testM("ints") {
        check(Gen.anyInt)(assertRoundtrips)
      },
      testM("longs") {
        check(Gen.anyLong)(assertRoundtrips)
      },
      testM("bigInts") {
        check(genBigInteger)(assertRoundtrips)
      },
      testM("floats") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyFloat.filter(java.lang.Float.isFinite))(assertRoundtrips)
      },
      testM("doubles") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyDouble.filter(java.lang.Double.isFinite))(assertRoundtrips)
      },
      testM("AST") {
        check(genAst)(assertRoundtrips)
      },
      suite("java.time")(
        test("DayOfWeek") {
          assertRoundtrips(DayOfWeek.MONDAY)
        },
        test("Duration") {
          assertRoundtrips(Duration.ofDays(1)) &&
          assertRoundtrips(Duration.ofSeconds(Long.MaxValue, 999999999L))
        },
        test("Instant") {
          assertRoundtrips(Instant.EPOCH)
        },
        test("LocalDate") {
          assertRoundtrips(LocalDate.of(2020, 1, 1))
        },
        test("LocalDateTime") {
          assertRoundtrips(LocalDateTime.of(2020, 1, 1, 12, 36, 0))
        },
        test("LocalTime") {
          assertRoundtrips(LocalTime.of(12, 36, 0))
        },
        test("Month") {
          assertRoundtrips(Month.JANUARY)
        },
        test("MonthDay") {
          assertRoundtrips(MonthDay.of(1, 1))
        },
        test("OffsetDateTime") {
          assertRoundtrips(OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC))
        },
        test("OffsetTime") {
          assertRoundtrips(OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4)))
        },
        test("Period") {
          assertRoundtrips(Period.ofDays(1))
        },
        test("Year") {
          assertRoundtrips(Year.of(1999)) &&
          assertRoundtrips(Year.of(10000)) &&
          assertRoundtrips(Year.MIN_VALUE) &&
          assertRoundtrips(Year.MAX_VALUE)
        },
        test("YearMonth") {
          assertRoundtrips(YearMonth.of(1999, 12))
        },
        test("ZonedDateTime") {
          assertRoundtrips(LocalDateTime.of(2020, 1, 1, 12, 36, 0))
        },
        test("ZoneId") {
          assertRoundtrips(ZoneId.of("Pacific/Auckland"))
        },
        test("ZoneOffset") {
          assertRoundtrips(ZoneOffset.ofHours(5))
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
