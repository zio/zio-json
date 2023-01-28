package zio.json

import zio.test.Assertion._
import zio.test._

object DerivedDecoderSpec extends ZIOSpecDefault {

  val spec = suite("DerivedDecoderSpec")(
    test("Derive with alias") {
      assertZIO(typeCheck {
        """
          import zio.json._
          case class Mango(@jsonAliases("r") roundness: Int, radius: Int)
          object Mango {
            implicit val decoder: JsonDecoder[Mango] = DeriveJsonDecoder.gen[Mango]
          }
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
//          case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("radius") r: Int)
//          object Mango {
//            implicit val decoder: JsonDecoder[Mango] = DeriveJsonDecoder.gen[Mango]
//          }
//          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
//        """
//      })(isLeft(anything))
//    },
//    test("Fails to derive when field names are not distinct - double alias") {
//      assertZIO(typeCheck {
//        """
//          import zio.json._
//          case class Mango(@jsonAliases("r", "r") roundness: Int, radius: Int)
//          object Mango {
//            implicit val decoder: JsonDecoder[Mango] = DeriveJsonDecoder.gen[Mango]
//          }
//          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
//        """
//      })(isLeft(anything))
//    },
//    test("Fails to derive when field names are not distinct - alias same as alias") {
//      assertZIO(typeCheck {
//        """
//          import zio.json._
//          case class Mango(@jsonAliases("r") roundness: Int, @jsonAliases("r") radius: Int)
//          object Mango {
//            implicit val decoder: JsonDecoder[Mango] = DeriveJsonDecoder.gen[Mango]
//          }
//          "{\"roundness\":0,\"radius\":0}".fromJson[Mango]
//        """
//      })(isLeft(anything))
//    }
  )
}
