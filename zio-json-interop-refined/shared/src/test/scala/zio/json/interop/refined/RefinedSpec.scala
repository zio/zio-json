package zio.json.interop.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import zio.json._
import zio.test.Assertion._
import zio.test._

object RefinedSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] =
    suite("Refined")(
      test("Refined") {
        val person    = Person(NonEmptyString.unsafeFrom("fommil"))
        val validJson = """{"name":"fommil"}"""
        assert("""{"name":""}""".fromJson[Person])(isLeft(equalTo(".name(Predicate isEmpty() did not fail.)"))) &&
        assert(validJson.fromJson[Person])(isRight(equalTo(person))) &&
        assert(person.toJson)(equalTo(validJson))
      }
    )

  case class Person(name: String Refined NonEmpty)

  object Person {
    implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
    implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
  }
}
