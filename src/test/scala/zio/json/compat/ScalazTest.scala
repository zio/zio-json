package testzio.json.compat

import zio.json._
import testzio.json.TestUtils._

import utest._
import _root_.scalaz._
import zio.json.compat.scalaz._

// testOnly *ScalazTest
object ScalazTest extends TestSuite {

  val tests = Tests {
    test("IList") {
      IList[Int]().toJson ==> "[]"
      IList(1, 2, 3).toJson ==> "[1,2,3]"

      IList[Int]().toJsonPretty ==> "[]"
      IList(1, 2, 3).toJsonPretty ==> "[1, 2, 3]"

      """[1,2,3]""".fromJson[IList[Int]] ==> Right(IList(1, 2, 3))
    }
  }

}
