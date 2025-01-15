package zio.json

import zio._
import zio.test.Assertion._
import zio.test._

object DerivedEncoderSpec extends ZIOSpecDefault {
  val spec = suite("DerivedEncoderSpec")(
    test("Derives for a product type") {
      case class Foo(bar: String) derives JsonEncoder

      val json = Foo("bar").toJson

      assertTrue(json == """{"bar":"bar"}""")
    },
    test("Derives for a sum enum Enumeration type") {
      enum Foo derives JsonEncoder:
        case Bar
        case Baz
        case Qux

      val json = (Foo.Qux: Foo).toJson

      assertTrue(json == """"Qux"""")
    },
    test("Derives for a sum enum Enumeration type with discriminator") {
      @jsonDiscriminator("$type")
      enum Foo derives JsonEncoder:
        case Bar
        case Baz
        case Qux

      val json = (Foo.Qux: Foo).toJson

      assertTrue(json == """{"$type":"Qux"}""")
    },
    test("Derives for a sum sealed trait Enumeration type") {
      sealed trait Foo derives JsonEncoder
      object Foo:
        case object Bar extends Foo
        case object Baz extends Foo
        case object Qux extends Foo

      val json = (Foo.Qux: Foo).toJson

      assertTrue(json == """"Qux"""")
    },
    test("Derives for a sum ADT type") {
      enum Foo derives JsonEncoder:
        case Bar
        case Baz(baz: String)
        case Qux(foo: Foo)

      val json = (Foo.Qux(Foo.Bar): Foo).toJson

      assertTrue(json == """{"Qux":{"foo":{"Bar":{}}}}""")
    },
    test("Derives and encodes for a union of string-based literals") {
      case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonEncoder

      assertTrue(Foo("A", Some("A")).toJson == """{"aOrB":"A","optA":"A"}""")
    }
  )
}
