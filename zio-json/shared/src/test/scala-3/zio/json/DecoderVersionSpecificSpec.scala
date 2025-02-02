import zio.json.DecoderOps
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

object DecoderVersionSpecificSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("Decoder")(
      suite("fromJson")(
        test("ArraySeq") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.ArraySeq("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[immutable.ArraySeq[String]])(isRight(equalTo(expected)))
        },
      ),
      suite("fromJsonAST")(
        test("ArraySeq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.ArraySeq("5XL", "2XL", "XL")

          assert(json.as[Seq[String]])(isRight(equalTo(expected)))
        }
      )
    )
}
