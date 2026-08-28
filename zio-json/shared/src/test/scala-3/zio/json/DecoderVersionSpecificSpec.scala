package zio.json

import zio.Chunk
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

// Test types for Bug 1: overloaded apply
case class OverloadedApplyWrapper(values: Chunk[Int])
object OverloadedApplyWrapper {
  def apply(single: Int): OverloadedApplyWrapper = new OverloadedApplyWrapper(Chunk(single))
}

// Test types for Bug 2: sealed trait with constructor params and case objects
sealed trait StatusWithCode(val code: Int)
object StatusWithCode {
  case object Active                                      extends StatusWithCode(1)
  case object Inactive                                    extends StatusWithCode(0)
  case class Custom(name: String, override val code: Int) extends StatusWithCode(code)
}

// Test types for @jsonExplicitEmptyCollections annotation
@jsonExplicitEmptyCollections()
case class WithExplicitEmpty(items: List[Int], name: String)
object WithExplicitEmpty {
  implicit val codec: JsonCodec[WithExplicitEmpty] = DeriveJsonCodec.gen[WithExplicitEmpty]
}

@jsonExplicitEmptyCollections(encoding = false)
case class WithExplicitEmptyEncodingOff(items: List[Int], name: String)
object WithExplicitEmptyEncodingOff {
  implicit val codec: JsonCodec[WithExplicitEmptyEncodingOff] = DeriveJsonCodec.gen[WithExplicitEmptyEncodingOff]
}

@jsonExplicitEmptyCollections(decoding = false)
case class WithExplicitEmptyDecodingOff(items: List[Int], name: String)
object WithExplicitEmptyDecodingOff {
  implicit val codec: JsonCodec[WithExplicitEmptyDecodingOff] = DeriveJsonCodec.gen[WithExplicitEmptyDecodingOff]
}

case class WithoutExplicitEmpty(items: List[Int], name: String)

// Test types for exhaustivity warning fix
sealed trait Tag
object Tag {
  case object Open               extends Tag
  case class Named(name: String) extends Tag

  implicit val codec: JsonCodec[Tag] = DeriveJsonCodec.gen[Tag]
}

// Test types for multi-level sealed hierarchy bug fix
sealed trait Animal
sealed trait Pet               extends Animal
case class Dog(name: String)   extends Pet
case class Cat(name: String)   extends Pet
case class Fish(color: String) extends Animal

// Test types for parametric sealed type (Bug 1: Either-like)
sealed trait SimpleEither[+A, +B]
case class SimpleLeft[+A, +B](value: A)  extends SimpleEither[A, B]
case class SimpleRight[+A, +B](value: B) extends SimpleEither[A, B]

// Test types for parametric case class (Bug 2)
case class SeqApiResult[A](results: Seq[A], total: Int)

// Test types for type alias with private constructor (Bug 3)
class MaquetteImpl private (val name: String, val version: Int)
object MaquetteImpl {
  def apply(name: String, version: Int): MaquetteImpl = new MaquetteImpl(name, version)
}

object DecoderVersionSpecificSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("DecoderVersionSpecific")(
      suite("fromJson")(
        test("ArraySeq") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.ArraySeq("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[immutable.ArraySeq[String]])(isRight(equalTo(expected)))
        },
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

          assertTrue("\"qux\"".fromJson[Foo] == Right(Foo.Qux)) &&
          assertTrue("\"bar\"".fromJson[Foo] == Right(Foo.Bar))
        },
        test("Derives for a sum enum Enumeration type with enumValuesAsStrings = false") {
          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(enumValuesAsStrings = false)

          enum Foo derives JsonDecoder:
            case Bar
            case Baz
            case Qux

          assertTrue("{\"Qux\":{}}".fromJson[Foo] == Right(Foo.Qux)) &&
          assertTrue("{\"Bar\":{}}".fromJson[Foo] == Right(Foo.Bar))
        },
        test("Derives for a sum sealed trait Enumeration type") {
          sealed trait Foo derives JsonDecoder
          object Foo:
            @jsonHint("Barrr")
            case object Bar extends Foo
            case object Baz extends Foo
            case object Qux extends Foo

          assertTrue("\"Qux\"".fromJson[Foo] == Right(Foo.Qux)) &&
          assertTrue("\"Barrr\"".fromJson[Foo] == Right(Foo.Bar))
        },
        test("Derives for a sum sealed trait Enumeration type with enumValuesAsStrings = false") {
          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(enumValuesAsStrings = false)

          sealed trait Foo derives JsonDecoder
          object Foo:
            @jsonHint("Barrr")
            case object Bar extends Foo
            case object Baz extends Foo
            case object Qux extends Foo

          assertTrue("{\"Qux\":{}}".fromJson[Foo] == Right(Foo.Qux)) &&
          assertTrue("{\"Barrr\":{}}".fromJson[Foo] == Right(Foo.Bar))
        },
        test("Derives for a sum sealed trait Enumeration type with discriminator") {
          @jsonDiscriminator("$type")
          sealed trait Foo derives JsonDecoder
          object Foo:
            @jsonHint("Barrr")
            case object Bar extends Foo
            case object Baz extends Foo
            case object Qux extends Foo

          assertTrue("""{"$type":"Qux"}""".fromJson[Foo] == Right(Foo.Qux)) &&
          assertTrue("""{"$type":"Barrr"}""".fromJson[Foo] == Right(Foo.Bar))
        },
        test("Derives for a sum sealed trait Enumeration type with computed jsonHint") {
          @jsonDiscriminator("$type")
          sealed trait Foo
          object Foo:
            @jsonHint("Bar" + "rr")
            case object Bar extends Foo
            case object Baz extends Foo

            given JsonCodec[Foo] = DeriveJsonCodec.gen[Foo]

          assertTrue((Foo.Bar: Foo).toJson == """{"$type":"Barrr"}""") &&
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
          assertTrue("""{"aOrB": {"C": 1}}""".fromJson[Foo] == Left(".aOrB.C(expected one of: A, B)"))
        },
        test("derives codec for case class with overloaded apply in companion") {
          implicit val codec: JsonCodec[OverloadedApplyWrapper] = DeriveJsonCodec.gen[OverloadedApplyWrapper]

          val json = """{"values":[1,2,3]}"""
          assertTrue(json.fromJson[OverloadedApplyWrapper] == Right(new OverloadedApplyWrapper(Chunk(1, 2, 3))))
        },
        test("derives codec for sealed trait with constructor params and case objects") {
          implicit val codec: JsonCodec[StatusWithCode] = DeriveJsonCodec.gen[StatusWithCode]

          assertTrue("""{"Active":{}}""".fromJson[StatusWithCode] == Right(StatusWithCode.Active)) &&
          assertTrue(
            """{"Custom":{"name":"test","code":42}}""".fromJson[StatusWithCode] == Right(
              StatusWithCode.Custom("test", 42)
            )
          )
        },
        test("@jsonExplicitEmptyCollections decodes explicit empty collections") {
          import WithExplicitEmpty._
          val result = """{"items":[],"name":"test"}""".fromJson[WithExplicitEmpty]
          assertTrue(result == Right(WithExplicitEmpty(List.empty, "test")))
        },
        test("@jsonExplicitEmptyCollections requires explicit empty collections in JSON") {
          import WithExplicitEmpty._
          // With default annotation (decoding=true), missing "items" field should FAIL
          val result = """{"name":"test"}""".fromJson[WithExplicitEmpty]
          assertTrue(result == Left(".items(missing)"))
        },
        test("@jsonExplicitEmptyCollections(decoding = false) allows missing empty collections") {
          import WithExplicitEmptyDecodingOff._
          // With decoding=false, missing "items" should succeed with empty List
          val result = """{"name":"test"}""".fromJson[WithExplicitEmptyDecodingOff]
          assertTrue(result == Right(WithExplicitEmptyDecodingOff(List.empty, "test")))
        },
        test("without annotation, missing empty collections fail with default config") {
          implicit val codec: JsonCodec[WithoutExplicitEmpty] = DeriveJsonCodec.gen[WithoutExplicitEmpty]
          // Default config has explicitEmptyCollections = ExplicitEmptyCollections(encoding=true, decoding=true)
          // So without annotation, missing "items" should FAIL (same as default config)
          val result = """{"name":"test"}""".fromJson[WithoutExplicitEmpty]
          assertTrue(result == Left(".items(missing)"))
        },
        test("derives codec for sealed trait with case objects (no exhaustivity warning)") {
          import Tag._
          assertTrue("""{"Open":{}}""".fromJson[Tag] == Right(Tag.Open)) &&
          assertTrue("""{"Named":{"name":"test"}}""".fromJson[Tag] == Right(Tag.Named("test")))
        },
        test("derives codec for multi-level sealed trait hierarchy") {
          implicit val codec: JsonCodec[Animal] = DeriveJsonCodec.gen[Animal]
          assertTrue("""{"Dog":{"name":"Rex"}}""".fromJson[Animal] == Right(Dog("Rex"))) &&
          assertTrue("""{"Cat":{"name":"Whiskers"}}""".fromJson[Animal] == Right(Cat("Whiskers"))) &&
          assertTrue("""{"Fish":{"color":"gold"}}""".fromJson[Animal] == Right(Fish("gold"))) &&
          assertTrue((Dog("Rex"): Animal).toJson == """{"Dog":{"name":"Rex"}}""") &&
          assertTrue((Cat("Whiskers"): Animal).toJson == """{"Cat":{"name":"Whiskers"}}""") &&
          assertTrue((Fish("gold"): Animal).toJson == """{"Fish":{"color":"gold"}}""")
        },
        test("derives codec for parametric sealed trait with concrete type args") {
          implicit val codec: JsonCodec[SimpleEither[String, Int]] = DeriveJsonCodec.gen[SimpleEither[String, Int]]
          assertTrue(
            """{"SimpleLeft":{"value":"hello"}}""".fromJson[SimpleEither[String, Int]] == Right(SimpleLeft("hello"))
          ) &&
          assertTrue(
            (SimpleLeft("hello"): SimpleEither[String, Int]).toJson == """{"SimpleLeft":{"value":"hello"}}"""
          ) &&
          assertTrue(
            """{"SimpleRight":{"value":42}}""".fromJson[SimpleEither[String, Int]] == Right(SimpleRight(42))
          ) &&
          assertTrue((SimpleRight(42): SimpleEither[String, Int]).toJson == """{"SimpleRight":{"value":42}}""")
        },
        test("derives codec for parametric sealed trait") {
          implicit def codec[A: JsonEncoder: JsonDecoder]: JsonCodec[SimpleEither[A, A]] =
            DeriveJsonCodec.gen[SimpleEither[A, A]]
          assertTrue(
            """{"SimpleLeft":{"value":42}}""".fromJson[SimpleEither[Int, Int]] == Right(SimpleLeft(42))
          ) &&
          assertTrue((SimpleLeft(42): SimpleEither[Int, Int]).toJson == """{"SimpleLeft":{"value":42}}""")
        },
        test("derives codec for parametric case class") {
          case class Container[A](items: Seq[A], label: String)
          given [A: JsonEncoder: JsonDecoder]: JsonCodec[Container[A]] = DeriveJsonCodec.gen[Container[A]]
          assertTrue(
            """{"items":[1,2],"label":"test"}""".fromJson[Container[Int]] == Right(Container(Seq(1, 2), "test"))
          )
        },
        test("derives codec for parametric case class with defaults") {
          case class WithDefaults[A](items: Seq[A], extras: Seq[String] = Nil)
          given [A: JsonEncoder: JsonDecoder]: JsonCodec[WithDefaults[A]] = DeriveJsonCodec.gen[WithDefaults[A]]
          assertTrue("""{"items":[1]}""".fromJson[WithDefaults[Int]] == Right(WithDefaults(Seq(1), Nil)))
        },
        test("alias on field decodes correctly via alternate name") {
          case class WithAlias(@jsonAliases("alt") @jsonField("primary") name: String, value: Int)
          implicit val codec: JsonCodec[WithAlias] = DeriveJsonCodec.gen[WithAlias]
          assertTrue("""{"primary":"hello","value":42}""".fromJson[WithAlias] == Right(WithAlias("hello", 42))) &&
          assertTrue("""{"alt":"hello","value":42}""".fromJson[WithAlias] == Right(WithAlias("hello", 42)))
        }
      ),
      suite("fromJsonAST")(
        test("ArraySeq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.ArraySeq("5XL", "2XL", "XL")
          assert(json.as[Seq[String]])(isRight(equalTo(expected)))
        }
      )
    )
}
