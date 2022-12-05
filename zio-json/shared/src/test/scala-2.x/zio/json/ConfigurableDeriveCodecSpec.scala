package zio.json

import zio.json.JsonCodecConfiguration.SumTypeHandling.DiscriminatorField
import zio.json.ast.Json
import zio.test._

object ConfigurableDeriveCodecSpec extends ZIOSpecDefault {
  case class ClassWithFields(someField: Int, someOtherField: String)

  sealed trait ST

  object ST {
    case object CaseObj          extends ST
    case class CaseClass(i: Int) extends ST
  }

  def spec = suite("ConfigurableDeriveCodecSpec")(
    suite("defaults")(
      suite("string")(
        test("should not map field names by default") {
          val expectedStr = """{"someField":1,"someOtherField":"a"}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should not use discriminator by default") {
          val expectedStr     = """{"CaseObj":{}}"""
          val expectedObj: ST = ST.CaseObj

          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should allow extra fields by default") {
          val jsonStr     = """{"someField":1,"someOtherField":"a","extra":123}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonStr.fromJson[ClassWithFields].toOption.get == expectedObj
          )
        }
      ),
      suite("AST")(
        test("should not map field names by default") {
          val expectedAST = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"))
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should not use discriminator by default") {
          val expectedAST     = Json.Obj("CaseObj" -> Json.Obj())
          val expectedObj: ST = ST.CaseObj

          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ST].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should allow extra fields by default") {
          val jsonAST     = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"), "extra" -> Json.Num(1))
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[ClassWithFields].toOption.get == expectedObj
          )
        }
      )
    ),
    suite("overrides")(
      suite("string")(
        test("should override field name mapping") {
          val expectedStr = """{"some_field":1,"some_other_field":"a"}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(fieldNameMapping = SnakeCase)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should specify discriminator") {
          val expectedStr     = """{"$type":"CaseClass","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("$type"))
          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should prevent extra fields") {
          val jsonStr = """{"someField":1,"someOtherField":"a","extra":123}"""

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(allowExtraFields = false)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonStr.fromJson[ClassWithFields].isLeft
          )
        }
      ),
      suite("AST")(
        test("should override field name mapping") {
          val expectedAST = Json.Obj("some_field" -> Json.Num(1), "some_other_field" -> Json.Str("a"))
          val expectedObj = ClassWithFields(1, "a")

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(fieldNameMapping = SnakeCase)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should specify discriminator") {
          val expectedAST     = Json.Obj("$type" -> Json.Str("CaseClass"), "i" -> Json.Num(1))
          val expectedObj: ST = ST.CaseClass(i = 1)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("$type"))
          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ST].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should prevent extra fields") {
          val jsonAST = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"), "extra" -> Json.Num(1))

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(allowExtraFields = false)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[ClassWithFields].isLeft
          )
        }
      )
    )
  )
}
