package testzio.json

import zio._
import zio.json._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object DerivedCodecSpec extends DefaultRunnableSpec {
  val spec = suite("DerivedCodecSpec")(
    testM("Derives for a product type") {
      assertM(typeCheck {
        """
          case class Foo(bar: String) derives JsonCodec

          Foo("bar").toJson.fromJson[Foo]
        """
      })(isRight(anything))
    },
    testM("Derives for a sum type") {
      assertM(typeCheck {
        """
          enum Foo derives JsonCodec:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          (Foo.Qux(Foo.Bar): Foo).toJson.fromJson[Foo]
        """
      })(isRight(anything))
    }
  )
}
