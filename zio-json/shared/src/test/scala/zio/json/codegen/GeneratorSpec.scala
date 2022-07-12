package zio.json.codegen

import zio.json._
import zio.json.ast.Json
import zio.test._

object GeneratorSpec extends ZIOSpecDefault {
  def spec = suite("GeneratorSpec")(
    suite("generates case classes from JSON strings")(
      test("simple object") {
        val json =
          """{
            |  "name": "John",
            |  "age": 30,
            |  "phones": [
            |    "+44 1234567",
            |    "+44 2345678"
            |  ]
            |}
          """.stripMargin

        val expected =
          """final case class RootObject(
            |  name: String,
            |  age: Int,
            |  phones: List[String]
            |)
            |
            |object RootObject {
            |  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
            |}
            |""".stripMargin.trim

        val result = Generator.generate(json.fromJson[ast.Json].toOption.get)

        assertTrue(result == expected)
      },
      test("nested objects") {
        // a person with nested pets

        val json =
          """{
            |  "name": "John",
            |  "age": 30,
            |  "pets": [
            |    {
            |      "name": "dog",
            |      "age": 5
            |    },
            |    {
            |      "name": "cat",
            |      "age": 2
            |    }
            |  ]
            |}
          """.stripMargin

        val expected =
          """final case class RootObject(
            |  name: String,
            |  age: Int,
            |  pets: List[Pets]
            |)
            |
            |object RootObject {
            |  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
            |}
            |
            |final case class Pets(
            |  name: String,
            |  age: Int
            |)
            |
            |object Pets {
            |  implicit val codec: JsonCodec[Pets] = DeriveJsonCodec.gen
            |}
            |""".stripMargin.trim

        val result = Generator.generate(json.fromJson[ast.Json].toOption.get)

        assertTrue(result == expected)
      },
      suite("unify types")(
        test("unify nulls and present keys to Option") {
          val json =
            """{
              |  "names": [
              |    "Cool",
              |    null,
              |    "Jim"
              |  ]
              |}
            """.stripMargin

          val expected =
            """final case class RootObject(
              |  names: List[Option[String]]
              |)
              |
              |object RootObject {
              |  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
              |}
              |""".stripMargin.trim

          val result = Generator.generate(json.fromJson[ast.Json].toOption.get)

          assertTrue(result == expected)
        },
        test("unify missing keys to Option") {
          val json =
            """{
              |  "people": [
              |    {
              |      "name": "Cool",
              |      "age": 30
              |    },
              |    {
              |      "name": "Jim"
              |    },
              |    {
              |      "name": "John",
              |      "age": 30
              |    }
              |  ]
              |}
              |""".stripMargin

          val expected =
            """final case class RootObject(
              |  people: List[People]
              |)
              |
              |object RootObject {
              |  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
              |}
              |
              |final case class People(
              |  name: String,
              |  age: Option[Int]
              |)
              |
              |object People {
              |  implicit val codec: JsonCodec[People] = DeriveJsonCodec.gen
              |}
              |""".stripMargin.trim

          val result = Generator.generate(json.fromJson[Json].toOption.get)

          assertTrue(result == expected)
        },
        test("unify Long and Int to Long") {
          val json = """
                       |{
                       |  "values": [
                       |    123,
                       |    99999999999,
                       |    5
                       |  ]
                       |}
                       |""".stripMargin

          val expected =
            """final case class RootObject(
              |  values: List[Long]
              |)
              |
              |object RootObject {
              |  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
              |}
              |""".stripMargin.trim

          val result = Generator.generate(json.fromJson[Json].toOption.get)
          assertTrue(result == expected)
        },
        test("unify double and int to double") {

          val json = """
                       |{
                       |  "values": [
                       |    123,
                       |    99999999999,
                       |    5.5
                       |  ]
                       |}
                       |""".stripMargin

          val expected =
            """final case class RootObject(
              |  values: List[Double]
              |)
              |
              |object RootObject {
              |  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
              |}
              |""".stripMargin.trim

          val result = Generator.generate(json.fromJson[Json].toOption.get)
          assertTrue(result == expected)

        }
      )
    )
  )
}
