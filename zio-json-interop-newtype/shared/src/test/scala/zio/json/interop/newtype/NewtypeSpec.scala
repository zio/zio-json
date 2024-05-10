package zio.json.interop.newtype

import io.estatico.newtype.macros.newtype
import zio.json.{ DecoderOps, DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder }
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ Spec, ZIOSpecDefault, assert, assertTrue }

object NewtypeSpec extends ZIOSpecDefault {
  override def spec: Spec[Environment, Any] = suite("newtype")(
    test("newtype") {
      assert("""{"name":"fommil"}""".fromJson[Person])(isRight(equalTo(Person(Name("fommil"))))) &&
      assertTrue(Person(Name("fommil")).toJson == """{"name":"fommil"}""")
    }
  )

  @newtype case class Name(value: String)
  case class Person(name: Name)
  object Person {
    implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
    implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
  }
}
