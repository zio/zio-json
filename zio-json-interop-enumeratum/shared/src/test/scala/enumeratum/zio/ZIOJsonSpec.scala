package enumeratum.zio

import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

object ZIOJsonSpec extends ZIOSpecDefault {

  def spec =
    suite("ZIOJsonSpec")(
      test("JsonEncoder should work")(
        ShirtSize.values.toList.map { case a =>
          assert(a.toJsonAST)(isRight(equalTo(Json.Str(a.entryName))))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonEncoder should work lowercase")(
        ShirtSize.values.toList.map { case a =>
          assert(a.toJsonAST(ZIOJson.encoderLowercase))(isRight(equalTo(Json.Str(a.entryName.toLowerCase()))))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonEncoder should work uppercase")(
        ShirtSize.values.toList.map { case a =>
          assert(a.toJsonAST(ZIOJson.encoderUppercase))(isRight(equalTo(Json.Str(a.entryName.toUpperCase()))))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonDecoder should parse to members when given proper JSON")(
        ShirtSize.values.toList.map { case a =>
          assert(Json.Str(a.entryName).as[ShirtSize])(isRight(equalTo(a)))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonDecoder should parse to members when given proper JSON for lower case")(
        ShirtSize.values.toList.map { case a =>
          assert(Json.Str(a.entryName.toLowerCase()).as[ShirtSize](ZIOJson.decoderLowercaseOnly(ShirtSize)))(
            isRight(equalTo(a))
          )
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonDecoder should parse to members when given proper JSON for upper case")(
        ShirtSize.values.toList.map { case a =>
          assert(Json.Str(a.entryName.toUpperCase()).as[ShirtSize](ZIOJson.decoderUppercaseOnly(ShirtSize)))(
            isRight(equalTo(a))
          )
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonEncoder/Decoder should to members when given proper JSON for ignoring case")(
        ShirtSize.values.zipWithIndex.toList.map { case (entry, i) =>
          val entryName =
            if (i % 2 == 0)
              entry.entryName.toUpperCase
            else
              entry.entryName.toLowerCase

          assert(Json.Str(entryName).as[ShirtSize](ZIOJson.decodeCaseInsensitive(ShirtSize)))(isRight(equalTo(entry)))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonEncoder/Decoder soupd fail to parse to members when given improper JSON, even when ignoring case")(
        Seq("123", "Jumbo").map { case entry =>
          assert(Json.Str(entry).as[ShirtSize](ZIOJson.decodeCaseInsensitive(ShirtSize)))(
            isLeft(equalTo(s"'$entry' is not a member of enum ShirtSize"))
          )
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonDecoder should fail to parse mixed but not upper case")(
        Seq("Small", "Medium", "Large").map { case entry =>
          assert(Json.Str(entry).as[ShirtSize](ZIOJson.decoderUppercaseOnly(ShirtSize)))(
            isLeft(equalTo(s"'$entry' is not a member of enum ShirtSize"))
          )
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      ),
      test("JsonDecoder should fail to parse mixed but not upper case")(
        Seq("Small", "Medium", "Large").map { case entry =>
          assert(Json.Str(entry).as[ShirtSize](ZIOJson.decoderLowercaseOnly(ShirtSize)))(
            isLeft(equalTo(s"'$entry' is not a member of enum ShirtSize"))
          )
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      )
    )
}
