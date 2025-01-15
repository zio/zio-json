package zio.json

import zio.json.TestUtils._
import zio._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.io.IOException

object JsonTestSuiteSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, IOException] = suite("JsonTestSuite")(
    // Uses files from JSONTestSuite by Nicolas Seriot:
    //   https://github.com/nst/JSONTestSuite
    test("passes all tests") {
      for {
        f <- getResourcePaths("json_test_suite")
        a <- ZIO.foreach(f.sorted) { path =>
               for {
                 input <- getResourceAsStringM(s"json_test_suite/$path")
                 exit <- ZIO.succeed {
                           // Catch Stack overflow
                           try {
                             JsonDecoder[Json]
                               .decodeJson(input)
                               .fold(Exit.fail, Exit.succeed)
                           } catch {
                             case t: Throwable =>
                               Exit.die(t)
                           }
                         }
               } yield
                 if (path.startsWith("y_")) {
                   assert(exit)(succeeds(anything).label(path))
                 } else {
                   assert(exit)(fails(anything).label(path))
                 }
             }
      } yield a.reduce(_ && _)
    }
  ) @@ ignore
}
