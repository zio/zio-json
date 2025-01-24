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

  case class OptionalField(a: Option[Int])

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
        },
        test("do not write nulls by default, decode missing nulls as None") {
          val expectedStr = """{}"""
          val expectedObj = OptionalField(None)

          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("write empty collections by default") {
          case class EmptySeq(a: Seq[Int])

          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("fail on decoding missing empty collections by default") {
          case class Empty(z: Option[Int])
          case class EmptyObj(a: Empty)
          case class EmptySeq(a: Seq[Int])

          implicit val codecEmpty: JsonCodec[Empty]       = DeriveJsonCodec.gen[Empty]
          implicit val codecEmptyObj: JsonCodec[EmptyObj] = DeriveJsonCodec.gen[EmptyObj]
          implicit val codecEmptySeq: JsonCodec[EmptySeq] = DeriveJsonCodec.gen[EmptySeq]

          assertTrue(
            """{}""".fromJson[EmptyObj] == Left(".a(missing)"),
            """{}""".fromJson[EmptySeq] == Left(".a(missing)")
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
        },
        test("do not write nulls by default") {
          val jsonAST     = Json.Obj()
          val expectedObj = OptionalField(None)

          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[OptionalField].toOption.get == expectedObj,
            expectedObj.toJsonAST == Right(jsonAST)
          )
        },
        test("write empty collections by default") {
          case class Empty()
          case class EmptySeq(a: Seq[Int], b: Empty)

          val jsonAST     = Json.Obj("a" -> Json.Arr(), "b" -> Json.Obj())
          val expectedObj = EmptySeq(Seq.empty, Empty())

          implicit val emptyCodec: JsonCodec[Empty] = DeriveJsonCodec.gen
          implicit val codec: JsonCodec[EmptySeq]   = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[EmptySeq].toOption.get == expectedObj,
            expectedObj.toJsonAST == Right(jsonAST)
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
        test("should override sum type mapping") {
          val expectedStr     = """{"$type":"case_class","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("$type"), sumTypeMapping = SnakeCase)
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
        },
        test("use explicit null values") {
          val expectedStr = """{"a":null}"""
          val expectedObj = OptionalField(None)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitNulls = true)
          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
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
        },
        test("use explicit null values") {
          val jsonAST     = Json.Obj("a" -> Json.Null)
          val expectedObj = OptionalField(None)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitNulls = true)
          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(jsonAST.as[OptionalField].toOption.get == expectedObj, expectedObj.toJsonAST == Right(jsonAST))
        },
        test("fail on decoding missing explicit nulls") {
          val jsonStr = """{}"""

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitNulls = true)
          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(jsonStr.fromJson[OptionalField].isLeft)
        } @@ TestAspect.ignore
      )
    )
  )
}
