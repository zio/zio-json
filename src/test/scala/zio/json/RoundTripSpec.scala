package testzio.json

import zio.json._
import zio.json.ast.Json
import zio.random.Random
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, _ }

object RoundTripSpec extends DefaultRunnableSpec {
  def spec =
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
        check(Gen.anyFloat.filter(_.isFinite))(assertRoundtrips)
      },
      testM("doubles") {
        // NaN / Infinity is tested manually, because of == semantics
        check(Gen.anyDouble.filter(_.isFinite))(assertRoundtrips)
      },
      testM("AST") {
        check(genAst)(assertRoundtrips)
      }
    )

  val genBigInteger =
    Gen
      .bigInt((BigInt(2).pow(128) - 1) * -1, BigInt(2).pow(128) - 1)
      .map(_.bigInteger)
      .filter(_.bitLength < 128)

  val genBigDecimal =
    Gen
      .bigDecimal((BigDecimal(2).pow(128) - 1) * -1, BigDecimal(2).pow(128) - 1)
      .map(_.bigDecimal)
      .filter(_.toBigInteger.bitLength < 128)

  // Something seems to be up with zio-test’s Gen.usASCII, it returns
  // strings like 'Chunk(<>)' (Chunk#toString?) containing any ASCII chars
  // This generator matches ScalaProps
  val genUsAsciiString =
    Gen.string(Gen.oneOf(Gen.char('!', '~')))

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

      Gen.oneOf(obj, arr, boo, str, num)
    }

  private def assertRoundtrips[A: JsonEncoder: JsonDecoder](a: A) =
    assert(a.toJson.fromJson[A])(isRight(equalTo(a))) &&
      assert(a.toJsonPretty.fromJson[A])(isRight(equalTo(a)))
}
