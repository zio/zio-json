package enumeratum.values

import zio.json._
import zio.test._

object ZioJsonValueEnumSpec extends ZIOSpecDefault {

  def spec = suite("ZioJsonValueEnum")(
    testZioJsonEnum("LongZioJsonEnum", ZioJsonContentType),
    testZioJsonEnum("ShortZioJsonEnum", ZioJsonDrinks),
    testZioJsonEnum("IntZioJsonEnum", ZioJsonLibraryItem),
    testZioJsonEnum("StringZioJsonEnum", ZioJsonOperatingSystem),
    testZioJsonKeyEnum("StringZioJsonEnum", ZioJsonOperatingSystem),
    testZioJsonKeyEnum("IntZioJsonEnum", ZioJsonLibraryItem),
    testZioJsonKeyEnum("LongZioJsonEnum", ZioJsonContentType),
    testZioJsonKeyEnum("ShortZioJsonEnum", ZioJsonDrinks),
    testZioJsonKeyEnum("CharZioJsonEnum", ZioJsonAlphabet),
    testZioJsonKeyEnum("ByteZioJsonEnum", ZioJsonBites),
    testZioJsonEnum("CharEnum", ZioJsonAlphabet),
    testZioJsonEnum("ByteEnum", ZioJsonBites),
    testZioJsonEnum("IntZioJsonEnum with val value members", ZioJsonMovieGenre),
    suite("error messages")(
      test("IntZioJsonEnum should report unknown value") {
        assertTrue(
          "999".fromJson[ZioJsonLibraryItem] == Left("('999' is not a member of enum ZioJsonLibraryItem)")
        )
      },
      test("LongZioJsonEnum should report unknown value") {
        assertTrue(
          "999".fromJson[ZioJsonContentType] == Left("('999' is not a member of enum ZioJsonContentType)")
        )
      },
      test("StringZioJsonEnum should report unknown value") {
        assertTrue(
          """"unknown"""".fromJson[ZioJsonOperatingSystem] == Left(
            "('unknown' is not a member of enum ZioJsonOperatingSystem)"
          )
        )
      }
    )
  )

  private def testZioJsonEnum[ValueType: JsonEncoder, EntryType <: ValueEnumEntry[
    ValueType
  ]: JsonEncoder: JsonDecoder](
    enumKind: String,
    myEnum: ValueEnum[ValueType, EntryType] with ZioJsonValueEnum[ValueType, EntryType]
  ): Spec[Any, Nothing] =
    suite(enumKind)(
      suite("to JSON")(
        test("should work") {
          assertTrue(
            myEnum.values.forall(entry => entry.toJson == entry.value.toJson)
          )
        }
      ),
      suite("from JSON")(
        test("should parse to members when given proper JSON") {
          assertTrue(
            myEnum.values.forall(entry => entry.value.toJson.fromJson[EntryType] == Right(entry))
          )
        },
        test("should fail to parse random JSON to members") {
          assertTrue(
            """"GOBBLYGOOKITY"""".fromJson[EntryType].isLeft
          )
        }
      )
    )

  private def testZioJsonKeyEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]: JsonFieldEncoder: JsonFieldDecoder](
    enumKind: String,
    myEnum: ValueEnum[ValueType, EntryType] with ZioJsonValueEnum[ValueType, EntryType]
  ): Spec[Any, Nothing] =
    suite(s"$enumKind as Key")(
      suite("to JSON")(
        test("should round-trip") {
          val map  = myEnum.values.zipWithIndex.map { case (e, i) => e -> i }.toMap
          val json = map.toJson
          assertTrue(json.fromJson[Map[EntryType, Int]] == Right(map))
        }
      ),
      suite("from JSON")(
        test("should fail to parse invalid key") {
          assertTrue("""{"999":0}""".fromJson[Map[EntryType, Int]].isLeft)
        }
      )
    )

}
