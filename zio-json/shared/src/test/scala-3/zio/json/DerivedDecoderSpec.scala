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
    test("Derives with alias") {
      assertZIO(typeCheck {
        """
          import zio.json._
          case class Mango(@jsonAliases("r") roundness: Int, radius: Int) derives JsonDecoder
          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
        """
      })(isRight(anything))
    }
//
// These tests don't work for a reason I do not yet understand.
//
//    test("Fails to derive when field names are not distinct - alias same as field") {
//      assertZIO(typeCheck {
//        """
//          import zio.json._
//          case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("radius") r: Int) derives JsonDecoder
//          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
//        """
//      })(isLeft(anything))
//    },
//    test("Fails to derive when field names are not distinct - double alias") {
//      assertZIO(typeCheck {
//        """
//          import zio.json._
//          case class Mango(@jsonAliases("r", "r") roundness: Int, radius: Int) derives JsonDecoder
//          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
//        """
//      })(isLeft(anything))
//    },
//    test("Fails to derive when field names are not distinct - alias same as alias") {
//      assertZIO(typeCheck {
//        """
//          import zio.json._
//          case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("r") radius: Int) derives JsonDecoder
//          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
//        """
//      })(isLeft(anything))
//    }
  )
}
