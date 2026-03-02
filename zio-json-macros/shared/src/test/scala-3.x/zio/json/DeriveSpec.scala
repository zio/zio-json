package zio.json

import zio.test.Assertion.*
import zio.test.*

object DeriveSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("DeriveCodec")(
      test("Scala 3 uses manual derivation") {
        import exampleproducts.*

        assert("""{}""".fromJson[Parameterless])(isRight(equalTo(Parameterless()))) &&
        assert(Parameterless().toJson)(equalTo("{}"))
      }
    )

  object exampleproducts {
    @jsonDerive
    case class Parameterless()

    object Parameterless {
      implicit val codecForParameterless: JsonCodec[Parameterless] =
        DeriveJsonCodec.gen[Parameterless]
    }
  }
}
