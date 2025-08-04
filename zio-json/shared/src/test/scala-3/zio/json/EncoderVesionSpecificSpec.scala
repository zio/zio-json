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
        },
        test("IArray") {
          assert(IArray.empty[Int].toJson)(equalTo("[]")) &&
          assert(IArray(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(IArray.empty[String].toJsonPretty)(equalTo("[]")) &&
          assert(IArray("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]"))
        },
        test("Derives for a product type") {
          case class Foo(bar: String) derives JsonEncoder

          val json = Foo("bar").toJson
          assertTrue(json == """{"bar":"bar"}""")
        },
        test("Derives for a sum enum Enumeration type") {
          enum Foo derives JsonEncoder:
            case Bar
            case Baz
            case Qux

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """"Qux"""")
        },
        test("Derives for a sum enum Enumeration type with enumValuesAsStrings = false") {
          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(enumValuesAsStrings = false)

          enum Foo derives JsonEncoder:
            case Bar
            case Baz
            case Qux

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """{"Qux":{}}""")
        },
        test("Derives for a sum enum Enumeration type with discriminator") {
          @jsonDiscriminator("$type")
          enum Foo derives JsonEncoder:
            case Bar
            case Baz
            case Qux

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """{"$type":"Qux"}""")
        },
        test("Derives for a sum sealed trait Enumeration type") {
          sealed trait Foo derives JsonEncoder
          object Foo:
            case object Bar extends Foo
            case object Baz extends Foo
            case object Qux extends Foo

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """"Qux"""")
        },
        test("Derives for a sum ADT type") {
          enum Foo derives JsonEncoder:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          val json = (Foo.Qux(Foo.Bar): Foo).toJson
          assertTrue(json == """{"Qux":{"foo":{"Bar":{}}}}""")
        },
        test("Derives and encodes for a union of string-based literals") {
          case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonEncoder

          assertTrue(Foo("A", Some("A")).toJson == """{"aOrB":"A","optA":"A"}""")
        },
        test("Derives and encodes for a custom map key string-based union type") {
          case class Foo(aOrB: Map["A" | "B", Int]) derives JsonEncoder

          assertTrue(Foo(Map("A" -> 1, "B" -> 2)).toJson == """{"aOrB":{"A":1,"B":2}}""")
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
