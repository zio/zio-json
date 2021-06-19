package testzio.json

import testzio.json.TestUtils._
import zio._
import zio.blocking.Blocking
import zio.json._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object JsonTestSuiteSpec extends DefaultRunnableSpec {

  def spec: Spec[Blocking with Annotations, TestFailure[Any], TestSuccess] = suite("JsonTestSuite")(
    // Uses files from JSONTestSuite by Nicolas Seriot:
    //   https://github.com/nst/JSONTestSuite
    testM("passes all tests") {
      for {
        f <- getResourcePaths("json_test_suite")
        a <- ZIO.foreach(f.sorted) { path =>
               for {
                 input <- getResourceAsStringM(s"json_test_suite/$path")
                 exit <- ZIO.effectTotal {
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
