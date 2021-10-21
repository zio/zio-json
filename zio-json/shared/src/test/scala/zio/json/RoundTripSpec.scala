package testzio.json

import testzio.json.Gens._
import zio.json._
import zio.json.JsonEncoder._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.time._
import zio.{ Has, Random }
import zio.test.Sized
import zio.test.Gen

import java.math.BigInteger

object RoundTripSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("RoundTrip")(
      test("booleans") {
        check(Gen.boolean)(b => assertRoundtrips(b))
      },
      test("bytes") {
        check(Gen.byte)(assertRoundtrips[Byte])
      },
      test("shorts") {
        check(Gen.short)(assertRoundtrips[Short])
      } @@ samples(10000),
      test("ints") {
        check(Gen.int)(assertRoundtrips[Int])
      } @@ samples(10000),
      test("longs") {
        check(Gen.long)(assertRoundtrips[Long])
      } @@ samples(10000),
      test("bigInts") {
        check(genBigInteger)(assertRoundtrips[BigInteger])
      } @@ samples(10000),
      test("floats") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.float.filter(java.lang.Float.isFinite))(assertRoundtrips[Float])
      } @@ samples(10000),
      test("doubles") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.double.filter(java.lang.Double.isFinite))(assertRoundtrips[Double])
      } @@ samples(10000),
      test("AST") {
        check(genAst)(assertRoundtrips[Json])
      },
      suite("java.time")(
        test("DayOfWeek") {
          check(genDayOfWeek)(assertRoundtrips[DayOfWeek])
        },
        test("Duration") {
          check(genDuration)(assertRoundtrips[Duration])
        } @@ samples(10000),
        test("Instant") {
          check(genInstant)(assertRoundtrips[Instant])
        } @@ samples(10000),
        test("LocalDate") {
          check(genLocalDate)(assertRoundtrips[LocalDate])
        } @@ samples(10000),
        test("LocalDateTime") {
          check(genLocalDateTime)(assertRoundtrips[LocalDateTime])
        } @@ samples(10000),
        test("LocalTime") {
          check(genLocalTime)(assertRoundtrips[LocalTime])
        } @@ samples(10000),
        test("Month") {
          check(genMonth)(assertRoundtrips[Month])
        },
        test("MonthDay") {
          check(genMonthDay)(assertRoundtrips[MonthDay])
        },
        test("OffsetDateTime") {
          check(genOffsetDateTime)(assertRoundtrips[OffsetDateTime])
        } @@ samples(10000),
        test("OffsetTime") {
          check(genOffsetTime)(assertRoundtrips[OffsetTime])
        } @@ samples(10000),
        test("Period") {
          check(genPeriod)(assertRoundtrips[Period])
        } @@ samples(10000),
        test("Year") {
          check(genYear)(assertRoundtrips[Year])
        } @@ samples(10000),
        test("YearMonth") {
          check(genYearMonth)(assertRoundtrips[YearMonth])
        } @@ samples(10000),
        test("ZonedDateTime") {
          check(genZonedDateTime)(assertRoundtrips[ZonedDateTime])
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
