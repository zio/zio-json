package enumeratum

import zio.json._
import zio.test._

object ZioJsonSpec extends ZIOSpecDefault {

  def spec = suite("ZioJson")(
    suite("to JSON")(
      test("should work") {
        assertTrue(
          ZioJsonShirtSize.values.forall(entry => entry.toJson == s""""${entry.entryName}"""")
        )
      },
      test("should work for lower case") {
        implicit val enc: JsonEncoder[ZioJsonShirtSize] =
          ZioJson.encoderLowercase(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.forall(entry => entry.toJson == s""""${entry.entryName.toLowerCase}"""")
        )
      },
      test("should work for upper case") {
        implicit val enc: JsonEncoder[ZioJsonShirtSize] =
          ZioJson.encoderUppercase(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.forall(entry => entry.toJson == s""""${entry.entryName.toUpperCase}"""")
        )
      }
    ),
    suite("round-trip")(
      test("should encode and decode lowercase") {
        implicit val enc: JsonEncoder[ZioJsonShirtSize] =
          ZioJson.encoderLowercase(ZioJsonShirtSize)
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderLowercaseOnly(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.forall(entry => entry.toJson.fromJson[ZioJsonShirtSize] == Right(entry))
        )
      },
      test("should encode and decode uppercase") {
        implicit val enc: JsonEncoder[ZioJsonShirtSize] =
          ZioJson.encoderUppercase(ZioJsonShirtSize)
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderUppercaseOnly(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.forall(entry => entry.toJson.fromJson[ZioJsonShirtSize] == Right(entry))
        )
      }
    ),
    suite("from JSON")(
      test("should parse to members when given proper JSON") {
        assertTrue(
          ZioJsonShirtSize.values.forall(entry =>
            s""""${entry.entryName}"""".fromJson[ZioJsonShirtSize] == Right(entry)
          )
        )
      },
      test("should parse to members when given proper JSON for lower case") {
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderLowercaseOnly(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.forall(entry =>
            s""""${entry.entryName.toLowerCase}"""".fromJson[ZioJsonShirtSize] == Right(entry)
          )
        )
      },
      test("should parse to members when given proper JSON for upper case") {
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderUppercaseOnly(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.forall(entry =>
            s""""${entry.entryName.toUpperCase}"""".fromJson[ZioJsonShirtSize] == Right(entry)
          )
        )
      },
      test("should parse to members when given proper JSON for ignoring case") {
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderCaseInsensitive(ZioJsonShirtSize)
        assertTrue(
          ZioJsonShirtSize.values.zipWithIndex.forall { case (entry, i) =>
            val entryName =
              if (i % 2 == 0) entry.entryName.toUpperCase
              else entry.entryName.toLowerCase
            s""""$entryName"""".fromJson[ZioJsonShirtSize] == Right(entry)
          }
        )
      },
      test("should fail to parse to members when given improper JSON, even when ignoring case") {
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderCaseInsensitive(ZioJsonShirtSize)
        assertTrue(
          """"123"""".fromJson[ZioJsonShirtSize] == Left("('123' is not a member of enum ZioJsonShirtSize)"),
          """"Jumbo"""".fromJson[ZioJsonShirtSize] == Left("('Jumbo' is not a member of enum ZioJsonShirtSize)")
        )
      },
      test("should fail to parse random JSON to members") {
        assertTrue(
          """"XXL"""".fromJson[ZioJsonShirtSize] == Left("('XXL' is not a member of enum ZioJsonShirtSize)"),
          "123".fromJson[ZioJsonShirtSize] == Left("(expected string)")
        )
      },
      test("should fail to parse mixed but not upper case") {
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderUppercaseOnly(ZioJsonShirtSize)
        assertTrue(
          Seq("Small", "Medium", "Large").forall(s =>
            s""""$s"""".fromJson[ZioJsonShirtSize] == Left(s"('$s' is not a member of enum ZioJsonShirtSize)")
          )
        )
      },
      test("should fail to parse mixed but not lower case") {
        implicit val dec: JsonDecoder[ZioJsonShirtSize] =
          ZioJson.decoderLowercaseOnly(ZioJsonShirtSize)
        assertTrue(
          Seq("Small", "Medium", "Large").forall(s =>
            s""""$s"""".fromJson[ZioJsonShirtSize] == Left(s"('$s' is not a member of enum ZioJsonShirtSize)")
          )
        )
      }
    )
  )

}
