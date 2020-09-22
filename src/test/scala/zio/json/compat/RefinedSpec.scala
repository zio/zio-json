package testzio.json.compat

import io.circe
import testzio.json.TestUtils._
import testzio.json.data.geojson.generated._
import testzio.json.data.googlemaps._
import testzio.json.data.twitter._
import zio.json._
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, _ }
import zio.random.Random
import zio.json.ast.Json
import zio.Chunk

import zio.json
import zio.json._
import testzio.json.TestUtils._
import eu.timepit.refined.api.{ Refined }

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
