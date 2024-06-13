package testzio.json

import zio._
import zio.json._
import zio.test.Assertion._
import zio.test._

object DerivedDecoderSpec extends ZIOSpecDefault {

  val spec = suite("DerivedDecoderSpec")(
    test("Derives for a product type") {
      assertZIO(typeCheck {
        """
          case class Foo(bar: String) derives JsonDecoder

          "{\"bar\": \"hello\"}".fromJson[Foo]
        """
      })(isRight(anything))
    },
    test("Derives for a sum type") {
      assertZIO(typeCheck {
        """
          enum Foo derives JsonDecoder:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          "{\"Qux\":{\"foo\":{\"Bar\":{}}}}".fromJson[Foo]
        """
      })(isRight(anything))
    },
    test("Derives and decodes for a union of string-based literals") {
      case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonDecoder

      assertTrue("""{"aOrB": "A", "optA": "A"}""".fromJson[Foo] == Right(Foo("A", Some("A")))) &&
      assertTrue("""{"aOrB": "C"}""".fromJson[Foo] == Left(".aOrB(expected one of: A, B)"))
    }
  )
}
