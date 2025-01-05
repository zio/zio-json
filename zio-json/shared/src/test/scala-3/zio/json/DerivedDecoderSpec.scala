package testzio.json

import zio._
import zio.json._
import zio.test.Assertion._
import zio.test._

object DerivedDecoderSpec extends ZIOSpecDefault {

  val spec = suite("DerivedDecoderSpec")(
    test("Derives for a product type") {
      case class Foo(bar: String) derives JsonDecoder

      val result = "{\"bar\": \"hello\"}".fromJson[Foo]

      assertTrue(result == Right(Foo("hello")))
    },
    test("Derives for a sum enum Enumeration type") {
      enum Foo derives JsonDecoder:
        case Bar
        case Baz
        case Qux

      val result = "\"Qux\"".fromJson[Foo]

      assertTrue(result == Right(Foo.Qux))
    },
    test("Derives for a sum sealed trait Enumeration type") {
      sealed trait Foo derives JsonDecoder
      object Foo:
        case object Bar extends Foo
        case object Baz extends Foo
        case object Qux extends Foo

      val result = "\"Qux\"".fromJson[Foo]

      assertTrue(result == Right(Foo.Qux))
    },
    test("Derives for a sum sealed trait Enumeration type with discriminator") {
      @jsonDiscriminator("$type")
      sealed trait Foo derives JsonDecoder
      object Foo:
        case object Bar extends Foo
        case object Baz extends Foo
        case object Qux extends Foo

      val result = """{"$type":"Qux"}""".fromJson[Foo]

      assertTrue(result == Right(Foo.Qux))
    },
    test("Derives for a sum ADT type") {
      enum Foo derives JsonDecoder:
        case Bar
        case Baz(baz: String)
        case Qux(foo: Foo)

      val result = "{\"Qux\":{\"foo\":{\"Bar\":{}}}}".fromJson[Foo]

      assertTrue(result == Right(Foo.Qux(Foo.Bar)))
    },
    test("Derives and decodes for a union of string-based literals") {
      case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonDecoder

      assertTrue("""{"aOrB": "A", "optA": "A"}""".fromJson[Foo] == Right(Foo("A", Some("A")))) &&
      assertTrue("""{"aOrB": "C"}""".fromJson[Foo] == Left(".aOrB(expected one of: A, B)"))
    }
  )
}
