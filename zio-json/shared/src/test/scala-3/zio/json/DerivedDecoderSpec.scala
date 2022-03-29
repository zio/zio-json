package testzio.json

import zio._
import zio.json._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object DerivedDecoderSpec extends DefaultRunnableSpec {

  val spec = suite("DerivedDecoderSpec")(
    testM("Derives for a product type") {
      assertM(typeCheck {
        """
          case class Foo(bar: String) derives JsonDecoder

          "{\"bar\": \"hello\"}".fromJson[Foo]
        """
      })(isRight(anything))
    },
    testM("Derives for a sum type") {
      assertM(typeCheck {
        """
          enum Foo derives JsonDecoder:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          "{\"Qux\":{\"foo\":{\"Bar\":{}}}}".fromJson[Foo]
        """
      })(isRight(anything))
    }
  )
}
