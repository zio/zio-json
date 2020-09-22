package testzio.json.compat

import _root_.scalaz._

import zio.json._
import zio.json.compat.scalaz._
import zio.test.Assertion._
import zio.test._

object ScalazSpec extends DefaultRunnableSpec {
  def spec =
    suite("Scalaz")(
      test("Scalaz") {
        assert(IList[Int]().toJson)(equalTo("[]")) &&
        assert(IList(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
        assert(IList[Int]().toJsonPretty)(equalTo("[]")) &&
        assert(IList(1, 2, 3).toJsonPretty)(equalTo("[1, 2, 3]")) &&
        assert("""[1,2,3]""".fromJson[IList[Int]])(isRight(equalTo(IList(1, 2, 3))))
      }
    )
}
