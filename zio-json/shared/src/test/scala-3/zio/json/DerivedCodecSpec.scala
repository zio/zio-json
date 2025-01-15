package zio.json

import zio._
import zio.test.Assertion._
import zio.test._

object DerivedCodecSpec extends ZIOSpecDefault {
  val spec = suite("DerivedCodecSpec")(
    test("Derives for a product type") {
      assertZIO(typeCheck {
        """
          case class Foo(bar: String) derives JsonCodec

          Foo("bar").toJson.fromJson[Foo]
        """
      })(isRight(anything))
    },
    test("Derives for a sum type") {
      assertZIO(typeCheck {
        """
          enum Foo derives JsonCodec:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          (Foo.Qux(Foo.Bar): Foo).toJson.fromJson[Foo]
        """
      })(isRight(anything))
    },
    test("Derives and encodes for a union of string-based literals") {
      case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonCodec

      assertTrue(Foo("A", Some("A")).toJson.fromJson[Foo] == Right(Foo("A", Some("A"))))
    }
  )
}
