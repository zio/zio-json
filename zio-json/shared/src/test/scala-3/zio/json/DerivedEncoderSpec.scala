package testzio.json

import zio._
import zio.json._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object DerivedEncoderSpec extends DefaultRunnableSpec {
  val spec = suite("DerivedEncoderSpec")(
    testM("Derives for a product type") {
      assertM(typeCheck {
        """
          case class Foo(bar: String) derives JsonEncoder

          Foo("bar").toJson
        """
      })(isRight(anything))
    },
    testM("Derives for a sum type") {
      assertM(typeCheck {
        """
          enum Foo derives JsonEncoder:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          (Foo.Qux(Foo.Bar): Foo).toJson
        """
      })(isRight(anything))
    }
  )
}
