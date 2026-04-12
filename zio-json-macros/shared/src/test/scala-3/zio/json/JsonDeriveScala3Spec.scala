package zio.json

import zio.test.*
import zio.test.Assertion.*

object JsonDeriveScala3Spec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)

  given JsonCodec[Person] = DeriveJsonCodec.gen[Person]

  def spec = suite("Scala 3 jsonDerive compatibility")(
    test("manual derivation works") {
      val json = """{"name":"John","age":30}"""

      assertTrue(
        json.fromJson[Person] == Right(Person("John", 30))
      )
    }
  )
}
