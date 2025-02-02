import zio.json.DecoderOps
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

object CodecVersionSpecificSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] =
    suite("CodecSpec")(
      test("ArraySeq") {
        val jsonStr  = """["5XL","2XL","XL"]"""
        val expected = immutable.ArraySeq("5XL", "2XL", "XL")

        assert(jsonStr.fromJson[immutable.ArraySeq[String]])(isRight(equalTo(expected)))
      }
    )
}
