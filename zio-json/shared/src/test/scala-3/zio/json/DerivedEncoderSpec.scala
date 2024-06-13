package testzio.json

import zio._
import zio.json._
import zio.test.Assertion._
import zio.test._

object DerivedEncoderSpec extends ZIOSpecDefault {
  val spec = suite("DerivedEncoderSpec")(
    test("Derives for a product type") {
      assertZIO(typeCheck {
        """
          case class Foo(bar: String) derives JsonEncoder

          Foo("bar").toJson
        """
      })(isRight(anything))
    },
    test("Derives for a sum type") {
      assertZIO(typeCheck {
        """
          enum Foo derives JsonEncoder:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          (Foo.Qux(Foo.Bar): Foo).toJson
        """
      })(isRight(anything))
    },
    test("Derives and encodes for a union of string-based literals") {
      case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonEncoder

      assertTrue(Foo("A", Some("A")).toJson == """{"aOrB":"A","optA":"A"}""")
    }
  )
}
