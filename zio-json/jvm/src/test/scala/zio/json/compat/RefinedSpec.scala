package testzio.json.compat

import zio.json._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.auto._
import zio.json.compat.refined._
import zio.test.Assertion._

import zio.test._

object RefinedSpec extends DefaultRunnableSpec {
  def spec =
    suite("Refined")(
      test("Refined") {
        assert("""{"name":""}""".fromJson[Person])(isLeft(equalTo(".name(Predicate isEmpty() did not fail.)"))) &&
        assert("""{"name":"fommil"}""".fromJson[Person])(isRight(equalTo(Person("fommil")))) &&
        assert(Person("fommil").toJson)(equalTo("""{"name":"fommil"}"""))
      }
    )

  case class Person(name: String Refined NonEmpty)

  object Person {
    implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
    implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
  }
}
