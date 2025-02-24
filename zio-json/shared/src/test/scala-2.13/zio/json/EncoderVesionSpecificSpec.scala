package zio.json

import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

object EncoderVesionSpecificSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("EncoderVesionSpecific")(
      suite("toJson")(
        test("collections") {
          assert(immutable.ArraySeq[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.ArraySeq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.ArraySeq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.ArraySeq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]"))
        }
      ),
      suite("toJsonAST")(
        test("collections") {
          val arrEmpty = Json.Arr()
          val arr123   = Json.Arr(Json.Num(1), Json.Num(2), Json.Num(3))
          assert(immutable.ArraySeq[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(immutable.ArraySeq(1, 2, 3).toJsonAST)(isRight(equalTo(arr123)))
        }
      )
    )
}
