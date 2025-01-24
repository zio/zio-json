package zio.json

import zio.json.ast.Json
import zio.test._

object AnnotationsCodecSpec extends ZIOSpecDefault {

  def spec = suite("ConfigurableDeriveCodecSpec")(
    suite("annotations overrides")(
      suite("string")(
        test("should override field name mapping") {
          @jsonMemberNames(SnakeCase)
          case class ClassWithFields(someField: Int, someOtherField: String)

          val expectedStr = """{"some_field":1,"some_other_field":"a"}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should specify discriminator") {
          @jsonDiscriminator("$type")
          sealed trait ST

          object ST {
            case object CaseObj          extends ST
            case class CaseClass(i: Int) extends ST

            implicit lazy val codec: JsonCodec[ST] = DeriveJsonCodec.gen
          }

          val expectedStr     = """{"$type":"CaseClass","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should override sum type mapping") {
          @jsonHintNames(SnakeCase)
          @jsonDiscriminator("$type")
          sealed trait ST

          object ST {
            case object CaseObj          extends ST
            case class CaseClass(i: Int) extends ST

            implicit lazy val codec: JsonCodec[ST] = DeriveJsonCodec.gen
          }

          val expectedStr     = """{"$type":"case_class","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should prevent extra fields") {
          @jsonNoExtraFields
          case class ClassWithFields(someField: Int, someOtherField: String)

          val jsonStr = """{"someField":1,"someOtherField":"a","extra":123}"""

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonStr.fromJson[ClassWithFields].isLeft
          )
        },
        test("use explicit null values") {
          @jsonExplicitNull
          case class OptionalField(a: Option[Int])

          val expectedStr = """{"a":null}"""
          val expectedObj = OptionalField(None)

          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        }
      ),
      suite("AST")(
        test("should override field name mapping") {
          @jsonMemberNames(SnakeCase)
          case class ClassWithFields(someField: Int, someOtherField: String)

          val expectedAST = Json.Obj("some_field" -> Json.Num(1), "some_other_field" -> Json.Str("a"))
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should specify discriminator") {
          @jsonDiscriminator("$type")
          sealed trait ST

          object ST {
            case object CaseObj          extends ST
            case class CaseClass(i: Int) extends ST

            implicit lazy val codec: JsonCodec[ST] = DeriveJsonCodec.gen
          }

          val expectedAST     = Json.Obj("$type" -> Json.Str("CaseClass"), "i" -> Json.Num(1))
          val expectedObj: ST = ST.CaseClass(i = 1)

          assertTrue(
            expectedAST.as[ST].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should prevent extra fields") {
          @jsonNoExtraFields
          case class ClassWithFields(someField: Int, someOtherField: String)

          val jsonAST = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"), "extra" -> Json.Num(1))

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[ClassWithFields].isLeft
          )
        },
        test("use explicit null values") {
          @jsonExplicitNull
          case class OptionalField(a: Option[Int])

          val jsonAST     = Json.Obj("a" -> Json.Null)
          val expectedObj = OptionalField(None)

          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(jsonAST.as[OptionalField].toOption.get == expectedObj, expectedObj.toJsonAST == Right(jsonAST))
        }
      )
    )
  )
}
