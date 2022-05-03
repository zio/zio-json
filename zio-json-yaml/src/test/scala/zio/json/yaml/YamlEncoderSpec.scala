package zio.json.yaml

import org.yaml.snakeyaml.DumperOptions.{ LineBreak, NonPrintableStyle, ScalarStyle }
import zio.json._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

object YamlEncoderSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("YamlEncoderSpec")(
      test("object root") {
        assert(ex1.toJsonAST.flatMap(_.toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX))))(
          isRight(equalTo(ex1Yaml))
        )
      },
      test("object root, with extension method") {
        assert(ex1.toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX)))(
          isRight(equalTo(ex1Yaml))
        )
      },
      test("scalar root") {
        assert(Json.Str("hello").toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX)))(
          isRight(equalTo("hello\n"))
        )
      },
      test("special characters in string") {
        assert(Json.Arr(Json.Str("- [] &hello \\!")).toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX)))(
          isRight(equalTo("  - '- [] &hello \\!'\n"))
        )
      },
      test("multiline string") {
        assert(Json.Str("hello\nworld").toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX)))(
          isRight(equalTo("|-\n  hello\n  world\n"))
        )
      },
      test("multiline string quoted") {
        assert(
          Json
            .Str("hello\nworld")
            .toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX, scalarStyle = _ => ScalarStyle.DOUBLE_QUOTED))
        )(
          isRight(equalTo("\"hello\\nworld\"\n"))
        )
      },
      test("nonprintable string escape") {
        assert(
          Json
            .Str("hello\u0008world")
            .toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX, nonPrintableStyle = NonPrintableStyle.ESCAPE))
        )(
          isRight(equalTo("\"hello\\bworld\"\n"))
        )
      },
      test("nonprintable string binary") {
        assert(
          Json
            .Str("hello\u0008world")
            .toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX, nonPrintableStyle = NonPrintableStyle.BINARY))
        )(
          isRight(equalTo("!!binary |-\n  aGVsbG8Id29ybGQ=\n"))
        )
      },
      test("sequence root") {
        assert(
          Json
            .Arr(Json.Bool(true), Json.Bool(false), Json.Bool(true))
            .toYaml(YamlOptions.default.copy(lineBreak = LineBreak.UNIX))
        )(
          isRight(equalTo("""  - true
                            |  - false
                            |  - true
                            |""".stripMargin))
        )
      },
      test("indentation settings") {
        assert(
          ex1.toJsonAST.flatMap(
            _.toYaml(
              YamlOptions.default.copy(
                indentation = 4,
                sequenceIndentation = 0,
                lineBreak = LineBreak.UNIX
              )
            )
          )
        )(
          isRight(equalTo(ex1Yaml2))
        )
      }
    )

  val ex1: Example =
    Example(
      i = 100,
      d = 0.25,
      s = List("a", "b"),
      o = Some(
        Example(
          i = 200,
          d = 0.0,
          s = List.empty,
          o = None
        )
      )
    )

  val ex1Yaml: String =
    """i: 100
      |d: 0.25
      |s:
      |  - a
      |  - b
      |o:
      |  i: 200
      |  d: 0
      |  s: []
      |""".stripMargin

  val ex1Yaml2: String =
    """i: 100
      |d: 0.25
      |s:
      |- a
      |- b
      |o:
      |    i: 200
      |    d: 0
      |    s: []
      |""".stripMargin

  case class Example(i: Int, d: Double, s: List[String], o: Option[Example])
  object Example {
    implicit lazy val codec: JsonCodec[Example] = DeriveJsonCodec.gen[Example]
  }
}
