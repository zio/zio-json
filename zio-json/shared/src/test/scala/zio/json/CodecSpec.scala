package zio.json

import zio._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect.jvmOnly
import zio.test._

import scala.collection.immutable

object CodecSpec extends ZIOSpecDefault {

  case class RecursiveOption(i: Int, d: Double, s: List[String], o: Option[RecursiveOption])
  object RecursiveOption {
    implicit lazy val codec: JsonCodec[RecursiveOption] = DeriveJsonCodec.gen[RecursiveOption]
  }

  val spec: Spec[Environment, Any] =
    suite("CodecSpec")(
      suite("Codec regressions")(
        test("option in recursive structure") {
          val expected = RecursiveOption(100, 0.25, List("a", "b"), Some(RecursiveOption(200, 0, Nil, None)))

          val decoded = JsonCodec[RecursiveOption].decoder
            .decodeJson("""{"i":100,"d":0.25,"s":["a","b"],"o":{"i":200,"d":0,"s":[]}}""")

          assertTrue(decoded.right.get == expected)
        }
      ),
      suite("Decoding")(
        test("empty") {
          import exampleempty._
          assert("{}".fromJson[Empty])(
            isRight(equalTo(Empty(None)))
          )
        },
        test("primitives") {
          val exampleBDString = "234234.234"
          // this big integer consumes more than 256 bits
          assert(
            "170141183460469231731687303715884105728489465165484668486513574864654818964653168465316546851"
              .fromJson[java.math.BigInteger]
          )(
            isLeft(equalTo("(expected a 256 bit BigInteger)"))
          )
        },
        test("java.util.Currency") {
          val exampleValue = "\"USD\""
          assert(exampleValue.fromJson[java.util.Currency])(isRight(equalTo(java.util.Currency.getInstance("USD"))))
        } @@ jvmOnly,
        test("eithers") {
          val bernies = List("""{"a":1}""", """{"left":1}""", """{"Left":1}""")
          val trumps  = List("""{"b":2}""", """{"right":2}""", """{"Right":2}""")

          assert(bernies.map(_.fromJson[Either[Int, Int]]))(
            forall(isRight(isLeft(equalTo(1))))
          ) && assert(trumps.map(_.fromJson[Either[Int, Int]]))(
            forall(isRight(isRight(equalTo(2))))
          )
        },
        test("parameterless products") {
          import exampleproducts._

          // actually anything works... consider this a canary test because if only
          // the empty object is supported that's fine.
          assert("""{}""".fromJson[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert("""null""".fromJson[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert("""{"field":"value"}""".fromJson[Parameterless])(isRight(equalTo(Parameterless())))
        },
        test("no extra fields") {
          import exampleproducts._

          assert("""{"s":""}""".fromJson[OnlyString])(isRight(equalTo(OnlyString("")))) &&
          assert("""{"s":"","t":""}""".fromJson[OnlyString])(isLeft(equalTo("(invalid extra field)")))
        },
        test("sum encoding") {
          import examplesum._

          assert("""{"Child1":{}}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"Child2":{}}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"type":"Child1"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)")))
        },
        test("sum encoding with hint names") {
          import examplesumhintnames._

          assert("""{"child1":{}}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"child2":{}}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"type":"child1"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert("""{"hint":"Cain"}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"hint":"Abel"}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"hint":"Samson"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert("""{"Cain":{}}""".fromJson[Parent])(isLeft(equalTo("(missing hint 'hint')")))
        },
        test("sum alternative encoding with hint names") {
          import examplealtsumhintnames._

          assert("""{"hint":"child1"}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"hint":"Abel"}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"hint":"Child1"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert("""{"child1":{}}""".fromJson[Parent])(isLeft(equalTo("(missing hint 'hint')")))
        },
        test("key transformation") {
          import exampletransformkeys._
          val kebabed       = """{"shish123-kebab":""}"""
          val snaked        = """{"indiana123_jones":""}"""
          val pascaled      = """{"Anders123Hejlsberg":""}"""
          val cameled       = """{"small123Talk":""}"""
          val overrides     = """{"not_modified":"","but-this-should-be":0}"""
          val kebabedLegacy = """{"shish-123-kebab":""}"""
          val snakedLegacy  = """{"indiana_123_jones":""}"""

          assert(kebabed.fromJson[Kebabed])(isRight(equalTo(Kebabed("")))) &&
          assert(kebabedLegacy.fromJson[legacy.Kebabed])(isRight(equalTo(legacy.Kebabed("")))) &&
          assert(snaked.fromJson[Snaked])(isRight(equalTo(Snaked("")))) &&
          assert(snakedLegacy.fromJson[legacy.Snaked])(isRight(equalTo(legacy.Snaked("")))) &&
          assert(pascaled.fromJson[Pascaled])(isRight(equalTo(Pascaled("")))) &&
          assert(cameled.fromJson[Cameled])(isRight(equalTo(Cameled("")))) &&
          assert(overrides.fromJson[OverridesAlsoWork])(isRight(equalTo(OverridesAlsoWork("", 0)))) &&
          assertTrue(Kebabed("").toJson == kebabed) &&
          assertTrue(Kebabed("").toJsonAST.toOption.get == kebabed.fromJson[Json].toOption.get) &&
          assertTrue(legacy.Kebabed("").toJson == kebabedLegacy) &&
          assertTrue(legacy.Kebabed("").toJsonAST.toOption.get == kebabedLegacy.fromJson[Json].toOption.get) &&
          assertTrue(Snaked("").toJson == snaked) &&
          assertTrue(Snaked("").toJsonAST.toOption.get == snaked.fromJson[Json].toOption.get) &&
          assertTrue(legacy.Snaked("").toJson == snakedLegacy) &&
          assertTrue(legacy.Snaked("").toJsonAST.toOption.get == snakedLegacy.fromJson[Json].toOption.get) &&
          assertTrue(Pascaled("").toJson == pascaled) &&
          assertTrue(Pascaled("").toJsonAST.toOption.get == pascaled.fromJson[Json].toOption.get) &&
          assertTrue(Cameled("").toJson == cameled) &&
          assertTrue(Cameled("").toJsonAST.toOption.get == cameled.fromJson[Json].toOption.get) &&
          assertTrue(OverridesAlsoWork("", 0).toJson == overrides)
        },
        test("key transformation - except native") {
          // Problem in scala-native seems to be with implementation of package scala.scalanative.regex.Parser, in particular method `parsePerlFlags`.
          // It should be fixed from there. Until then, better not to use regex.

          import exampletransformkeys._
          val indianaJones = """{"wHATcASEiStHIS":""}"""

          assert(indianaJones.fromJson[Custom])(isRight(equalTo(Custom("")))) &&
          assertTrue(Custom("").toJson == indianaJones) &&
          assertTrue(Custom("").toJsonAST.toOption.get == indianaJones.fromJson[Json].toOption.get)
        } @@ TestAspect.exceptNative,
        test("unicode") {
          assert(""""€🐵🥰"""".fromJson[String])(isRight(equalTo("€🐵🥰")))
        },
        test("Seq") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Seq("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Seq[String]])(isRight(equalTo(expected)))
        },
        test("Vector") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Vector("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Vector[String]])(isRight(equalTo(expected)))
        },
        test("SortedSet") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        },
        test("HashSet") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[immutable.HashSet[String]])(isRight(equalTo(expected)))
        },
        test("Set") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Set("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Set[String]])(isRight(equalTo(expected)))
        },
        test("Map") {
          val jsonStr  = """{"5XL":3,"2XL":14,"XL":159}"""
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(jsonStr.fromJson[Map[String, Int]])(isRight(equalTo(expected)))
        },
        test("ListMap") {
          val jsonStr  = """{"5XL":3,"2XL":14,"XL":159}"""
          val expected = collection.immutable.ListMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(jsonStr.fromJson[collection.immutable.ListMap[String, Int]])(isRight(equalTo(expected)))
        },
        test("zio.Chunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Chunk[String]])(isRight(equalTo(expected)))
        }
      ),
      suite("Encode -> Decode")(
        suite("control chars")(
          test("tab") {
            assert(encodeDecode(JsonCodec.char, '\t'))(isRight(equalTo('\t')))
          },
          test("carriage return") {
            assert(encodeDecode(JsonCodec.char, '\r'))(isRight(equalTo('\r')))
          },
          test("newline") {
            assert(encodeDecode(JsonCodec.char, '\n'))(isRight(equalTo('\n')))
          },
          test("form feed") {
            assert(encodeDecode(JsonCodec.char, '\f'))(isRight(equalTo('\f')))
          },
          test("backspace") {
            assert(encodeDecode(JsonCodec.char, '\b'))(isRight(equalTo('\b')))
          },
          test("escape") {
            assert(encodeDecode(JsonCodec.char, '\\'))(isRight(equalTo('\\')))
          },
          test("quote") {
            assert(encodeDecode(JsonCodec.char, '"'))(isRight(equalTo('"')))
          }
        )
      )
    )

  private def encodeDecode[A](codec: JsonCodec[A], value: A): Either[String, A] =
    codec.decodeJson(
      codec.encodeJson(value, None)
    )

  object exampleproducts {
    case class Parameterless()

    object Parameterless {
      implicit val codec: JsonCodec[Parameterless] = DeriveJsonCodec.gen[Parameterless]
    }

    @jsonNoExtraFields
    case class OnlyString(s: String)

    object OnlyString {
      implicit val codec: JsonCodec[OnlyString] = DeriveJsonCodec.gen[OnlyString]
    }
  }

  object examplesum {
    sealed abstract class Parent

    object Parent {
      implicit val codec: JsonCodec[Parent] = DeriveJsonCodec.gen[Parent]
    }
    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object examplesumhintnames {
    @jsonHintNames(SnakeCase)
    sealed abstract class Parent

    object Parent {
      implicit val codec: JsonCodec[Parent] = DeriveJsonCodec.gen[Parent]
    }
    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object exampleempty {
    case class Empty(a: Option[String])

    object Empty {
      implicit val codec: JsonCodec[Empty] = DeriveJsonCodec.gen[Empty]
    }
  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    @jsonHintNames(SnakeCase)
    sealed abstract class Parent

    object Parent {
      implicit val codec: JsonCodec[Parent] = DeriveJsonCodec.gen[Parent]
    }

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent
  }

  object examplealtsumhintnames {

    @jsonDiscriminator("hint")
    @jsonHintNames(SnakeCase)
    sealed abstract class Parent

    object Parent {
      implicit val codec: JsonCodec[Parent] = DeriveJsonCodec.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent
  }

  object exampletransformkeys {
    @jsonMemberNames(KebabCase)
    case class Kebabed(shish123Kebab: String)
    object Kebabed {
      implicit val codec: JsonCodec[Kebabed] = DeriveJsonCodec.gen[Kebabed]
    }

    @jsonMemberNames(SnakeCase)
    case class Snaked(indiana123Jones: String)
    object Snaked {
      implicit val codec: JsonCodec[Snaked] = DeriveJsonCodec.gen[Snaked]
    }

    @jsonMemberNames(PascalCase)
    case class Pascaled(anders123Hejlsberg: String)
    object Pascaled {
      implicit val codec: JsonCodec[Pascaled] = DeriveJsonCodec.gen[Pascaled]
    }

    @jsonMemberNames(CamelCase)
    case class Cameled(small123_talk: String)
    object Cameled {
      implicit val codec: JsonCodec[Cameled] = DeriveJsonCodec.gen[Cameled]
    }

    @jsonMemberNames(CustomCase(Custom.indianaJonesCase))
    case class Custom(whatCaseIsThis: String)
    object Custom {
      def indianaJonesCase(str: String): String =
        str
          .split("(?=\\p{Upper})")
          .map(part => s"${part.head.toLower}${part.substring(1, part.length).toUpperCase}")
          .mkString

      implicit val codec: JsonCodec[Custom] = DeriveJsonCodec.gen[Custom]
    }

    @jsonMemberNames(KebabCase)
    case class OverridesAlsoWork(@jsonField("not_modified") notModified: String, butThisShouldBe: Int)
    object OverridesAlsoWork {
      implicit val codec: JsonCodec[OverridesAlsoWork] = DeriveJsonCodec.gen[OverridesAlsoWork]
    }

    object legacy {
      @jsonMemberNames(ziojson_03.KebabCase)
      case class Kebabed(shish123Kebab: String)

      object Kebabed {
        implicit val codec: JsonCodec[Kebabed] = DeriveJsonCodec.gen[Kebabed]
      }

      @jsonMemberNames(ziojson_03.SnakeCase)
      case class Snaked(indiana123Jones: String)

      object Snaked {
        implicit val codec: JsonCodec[Snaked] = DeriveJsonCodec.gen[Snaked]
      }

    }
  }

  object logEvent {
    case class Event(at: Long, message: String, a: Seq[String] = Nil)
    implicit val codec: JsonCodec[Event] = DeriveJsonCodec.gen[Event]
  }
}
