import zio.json._
import zio.json.golden._
import zio.test._
import zio.test.magnolia.DeriveGen

object EncodeDecodeSpec extends ZIOSpecDefault {
  case class Banana(curvature: Double)
  object Banana {
    implicit val codec: JsonCodec[Banana] = DeriveJsonCodec.gen[Banana]
  }

  def spec = suite("EncodeDecodeSpec")(
    goldenTest(DeriveGen[Banana])
  )
}
