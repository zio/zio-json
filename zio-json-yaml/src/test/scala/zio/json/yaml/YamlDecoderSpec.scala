package zio.json.yaml

import zio.json.yaml.YamlEncoderSpec.{ Example, ex1, ex1Yaml, ex1Yaml2 }
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._
import zio.test.environment.TestEnvironment

object YamlDecoderSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Decoding from YAML")(
      test("object root") {
        assert(ex1Yaml.fromYaml[Example])(
          isRight(equalTo(ex1))
        )
      },
      test("object root, different indentation") {
        assert(ex1Yaml2.fromYaml[Example])(
          isRight(equalTo(ex1))
        )
      },
      test("scalar root") {
        assert("hello".fromYaml[String])(isRight(equalTo("hello")))
      },
      test("bool root") {
        assert("yes".fromYaml[Boolean])(isRight(equalTo(true)))
      },
      test("float root") {
        assert("3.14".fromYaml[Double])(isRight(equalTo(3.14)))
      },
      test("sequence root") {
        assert("- a\n- b\n- c".fromYaml[Vector[String]])(isRight(equalTo(Vector("a", "b", "c"))))
      }
    )
}
