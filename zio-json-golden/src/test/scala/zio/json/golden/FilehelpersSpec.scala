package zio.json.golden

import zio._
import zio.test._

import java.io.File

object FilehelpersSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment with Scope, Any] = suite("filehelpers.resolveModuleRoot")(
    test("finds the module root from `target` when no override is given") {
      for {
        moduleRoot <- filehelpers.resolveModuleRoot(None)
      } yield assertTrue(moduleRoot.getName == "zio-json-golden")
    },
    test("uses the override when given") {
      for {
        moduleRoot <- filehelpers.resolveModuleRoot(Some("/some/module"))
      } yield assertTrue(moduleRoot == new File("/some/module"))
    }
  )

}
