package zio.json

import zio.json.Gens._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.time._

object RoundTripSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("RoundTrip")(
      test("booleans") {
        check(Gen.boolean)(assertRoundtrips[Boolean])
      },
      test("bytes") {
        check(Gen.byte)(assertRoundtrips[Byte])
      },
      test("shorts") {
        check(Gen.short)(assertRoundtrips[Short])
      } @@ jvm(samples(10000)),
      test("ints") {
        check(Gen.int)(assertRoundtrips[Int])
      } @@ jvm(samples(10000)),
      test("longs") {
        check(Gen.long)(assertRoundtrips[Long])
      } @@ jvm(samples(10000)),
      test("bigInts") {
        check(genBigInteger)(assertRoundtrips[java.math.BigInteger])
      } @@ jvm(samples(10000)),
      test("floats") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.float.filter(java.lang.Float.isFinite))(assertRoundtrips[Float])
      } @@ jvm(samples(10000)),
      test("doubles") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.double.filter(java.lang.Double.isFinite))(assertRoundtrips[Double])
      } @@ jvm(samples(10000)),
      test("AST") {
        check(genAst)(assertRoundtrips[Json])
      },
      suite("java.time")(
        test("DayOfWeek") {
          check(genDayOfWeek)(assertRoundtrips[DayOfWeek])
        },
        test("Duration") {
          check(genDuration)(assertRoundtrips[Duration])
        } @@ jvm(samples(10000)),
        test("Instant") {
          check(genInstant)(assertRoundtrips[Instant])
        } @@ jvm(samples(10000)),
        test("LocalDate") {
          check(genLocalDate)(assertRoundtrips[LocalDate])
        } @@ jvm(samples(10000)),
        test("LocalDateTime") {
          check(genLocalDateTime)(assertRoundtrips[LocalDateTime])
        } @@ jvm(samples(10000)),
        test("LocalTime") {
          check(genLocalTime)(assertRoundtrips[LocalTime])
        } @@ jvm(samples(10000)),
        test("Month") {
          check(genMonth)(assertRoundtrips[Month])
        },
        test("MonthDay") {
          check(genMonthDay)(assertRoundtrips[MonthDay])
        },
        test("OffsetDateTime") {
          check(genOffsetDateTime)(assertRoundtrips[OffsetDateTime])
        } @@ jvm(samples(10000)),
        test("OffsetTime") {
          check(genOffsetTime)(assertRoundtrips[OffsetTime])
        } @@ jvm(samples(10000)),
        test("Period") {
          check(genPeriod)(assertRoundtrips[Period])
        } @@ jvm(samples(10000)),
        test("Year") {
          check(genYear)(assertRoundtrips[Year])
        } @@ jvm(samples(10000)),
        test("YearMonth") {
          check(genYearMonth)(assertRoundtrips[YearMonth])
        } @@ jvm(samples(10000)),
        test("ZonedDateTime") {
          check(genZonedDateTime)(assertRoundtrips[ZonedDateTime])
        } @@ jvm(samples(10000)),
        test("ZoneId") {
          check(genZoneId)(assertRoundtrips[ZoneId])
        },
        test("ZoneOffset") {
          check(genZoneOffset)(assertRoundtrips[ZoneOffset])
        }
      )
    )

  lazy val genAst: Gen[Sized, Json] =
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
