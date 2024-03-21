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
    test("Derives for a sum Enumeration type") {
      enum Foo derives JsonDecoder:
        case Bar
        case Baz
        case Qux

      val result = "\"Qux\"".fromJson[Foo]
    
      assertTrue(result == Right(Foo.Qux))
    },
    test("Derives for a sum ADT type") {
      enum Foo derives JsonDecoder:
        case Bar
        case Baz(baz: String)
        case Qux(foo: Foo)

      val result = "{\"Qux\":{\"foo\":{\"Bar\":{}}}}".fromJson[Foo]

      assertTrue(result == Right(Foo.Qux(Foo.Bar)))
    }
  )
}
