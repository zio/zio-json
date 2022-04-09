package zio.json.interop.scalaz

import scalaz._
import zio.json._
import zio.json.interop.scalaz7x._
import zio.test.Assertion._
import zio.test._

object ScalazSpec extends ZIOSpecDefault {
  val spec: ZSpec[Environment, Any] =
    suite("Scalaz")(
      test("scalaz.IList[A]") {
        assert(IList[Int]().toJson)(equalTo("[]")) &&
        assert(IList(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
        assert(IList[Int]().toJsonPretty)(equalTo("[]")) &&
        assert(IList(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
        assert("""[1,2,3]""".fromJson[IList[Int]])(isRight(equalTo(IList(1, 2, 3))))
      }
    )
}
