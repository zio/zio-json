package zio.json

import zio._
import zio.test.Assertion._
import zio.test._

object DerivedDecoderSpec extends ZIOSpecDefault {

  val spec = suite("DerivedDecoderSpec")(
    test("Derives for a product type") {
      case class Foo(bar: String) derives JsonDecoder

      assertTrue("{\"bar\": \"hello\"}".fromJson[Foo] == Right(Foo("hello")))
    },
    test("Derives for a sum enum Enumeration type") {
      @jsonHintNames(SnakeCase)
      enum Foo derives JsonDecoder:
        case Bar
        case Baz
        case Qux

      assertTrue("\"qux\"".fromJson[Foo] == Right(Foo.Qux))
      assertTrue("\"bar\"".fromJson[Foo] == Right(Foo.Bar))
    },
    test("Derives for a sum sealed trait Enumeration type") {
      sealed trait Foo derives JsonDecoder
      object Foo:
        @jsonHint("Barrr")
        case object Bar extends Foo
        case object Baz extends Foo
        case object Qux extends Foo

      assertTrue("\"Qux\"".fromJson[Foo] == Right(Foo.Qux))
      assertTrue("\"Barrr\"".fromJson[Foo] == Right(Foo.Bar))
    },
    test("Derives for a sum sealed trait Enumeration type with discriminator") {
      @jsonDiscriminator("$type")
      sealed trait Foo derives JsonDecoder
      object Foo:
        @jsonHint("Barrr")
        case object Bar extends Foo
        case object Baz extends Foo
        case object Qux extends Foo

      assertTrue("""{"$type":"Qux"}""".fromJson[Foo] == Right(Foo.Qux))
      assertTrue("""{"$type":"Barrr"}""".fromJson[Foo] == Right(Foo.Bar))
    },
    test("skip JSON encoded in a string value") {
      @jsonDiscriminator("type")
      sealed trait Example derives JsonDecoder {
        type Content
        def content: Content
      }
      object Example {
        @jsonHint("JSON")
        final case class JsonInput(content: String) extends Example {
          override type Content = String
        }
      }

      val json =
        """
          |{
          |  "content": "\"{\\n  \\\"name\\\": \\\"John\\\",\\\"location\\\":\\\"Sydney\\\",\\n  \\\"email\\\": \\\"jdoe@test.com\\\"\\n}\"",
          |  "type": "JSON"
          |}
          |""".stripMargin.trim
      assertTrue(json.fromJson[Example].isRight)
    },
    test("Derives for a recursive sum ADT type") {
      enum Foo derives JsonDecoder:
        case Bar
        case Baz(baz: String)
        case Qux(foo: Foo)

      assertTrue("{\"Qux\":{\"foo\":{\"Bar\":{}}}}".fromJson[Foo] == Right(Foo.Qux(Foo.Bar)))
    },
    test("Derives and decodes for a union of string-based literals") {
      case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonDecoder

      assertTrue("""{"aOrB": "A", "optA": "A"}""".fromJson[Foo] == Right(Foo("A", Some("A")))) &&
      assertTrue("""{"aOrB": "C"}""".fromJson[Foo] == Left(".aOrB(expected one of: A, B)"))
    },
    test("Derives and decodes for a custom map key string-based union type") {
      case class Foo(aOrB: Map["A" | "B", Int]) derives JsonDecoder

      assertTrue("""{"aOrB": {"A": 1, "B": 2}}""".fromJson[Foo] == Right(Foo(Map("A" -> 1, "B" -> 2)))) &&
      assertTrue("""{"aOrB": {"C": 1}}""".fromJson[Foo] == Left(".aOrB.C((expected one of: A, B))"))
    },
  )
}
