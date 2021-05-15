package testzio.json

import java.util.UUID

import scala.collection.{ immutable, mutable }

import zio._
import zio.json._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._
import zio.test.environment.Live

object DecoderSpec extends DefaultRunnableSpec {

  def spec: Spec[ZEnv with Live, TestFailure[Any], TestSuccess] =
    suite("Decoder")(
      suite("fromJson")(
        test("BigDecimal") {
          assert("123".fromJson[BigDecimal])(isRight(equalTo(BigDecimal(123))))
        },
        test("BigInteger too large") {
          // this big integer consumes more than 128 bits
          assert("170141183460469231731687303715884105728".fromJson[java.math.BigInteger])(
            isLeft(equalTo("(expected a 128 bit BigInteger)"))
          )
        },
        test("collections") {
          val arr = """[1, 2, 3]"""
          val obj = """{ "a": 1 }"""

          assert(arr.fromJson[Array[Int]])(isRight(equalTo(Array(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.IndexedSeq[Int]])(isRight(equalTo(immutable.IndexedSeq(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.LinearSeq[Int]])(isRight(equalTo(immutable.LinearSeq(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.ListSet[Int]])(isRight(equalTo(immutable.ListSet(1, 2, 3)))) &&
          assert(arr.fromJson[immutable.TreeSet[Int]])(isRight(equalTo(immutable.TreeSet(1, 2, 3)))) &&
          assert(obj.fromJson[mutable.Map[String, Int]])(isRight(equalTo(mutable.Map("a" -> 1))))
        },
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
        test("default field value") {
          import exampleproducts._

          assert("""{}""".fromJson[DefaultString])(isRight(equalTo(DefaultString("")))) &&
          assert("""{"s": null}""".fromJson[DefaultString])(isRight(equalTo(DefaultString(""))))
        },
        test("sum encoding") {
          import examplesum._

          assert("""{"Child1":{}}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"Child2":{}}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"type":"Child1"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert("""{"hint":"Cain"}""".fromJson[Parent])(isRight(equalTo(Child1()))) &&
          assert("""{"hint":"Abel"}""".fromJson[Parent])(isRight(equalTo(Child2()))) &&
          assert("""{"hint":"Samson"}""".fromJson[Parent])(isLeft(equalTo("(invalid disambiguator)"))) &&
          assert("""{"Cain":{}}""".fromJson[Parent])(isLeft(equalTo("(missing hint 'hint')")))
        },
        test("unicode") {
          assert(""""€🐵🥰"""".fromJson[String])(isRight(equalTo("€🐵🥰")))
        },
        test("Option: .map on derived JsonDecoder with missing value") {
          // More information about use case here https://github.com/zio/zio-json/issues/198
          // User wants to derive an alternative encoding of optionality
          sealed trait Assumed[+A]

          object Assumed {
            case object MissingAssumed       extends Assumed[Nothing]
            case class FoundAssumed[A](v: A) extends Assumed[A]

            implicit def decoder[A](implicit decoding: JsonDecoder[A]): JsonDecoder[Assumed[A]] =
              JsonDecoder.option[A].map {
                case None    => Assumed.MissingAssumed
                case Some(v) => Assumed.FoundAssumed[A](v)
              }
          }

          case class Example(a: Assumed[Boolean])
          implicit val exampleDecoder: JsonDecoder[Example] = DeriveJsonDecoder.gen[Example]

          assert("""{ "a": null }""".fromJson[Example])(isRight(equalTo(Example(Assumed.MissingAssumed)))) &&
          assert("""{ "a": true }""".fromJson[Example])(isRight(equalTo(Example(Assumed.FoundAssumed(true))))) &&
          assert("""{ "a": false }""".fromJson[Example])(isRight(equalTo(Example(Assumed.FoundAssumed(false))))) &&
          assert("""{ }""".fromJson[Example])(isRight(equalTo(Example(Assumed.MissingAssumed))))
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
        test("Map with unicode keys") {
          val expected = Map(new String(Array('\u0007', '\n')) -> "value")
          val jsonStr  = JsonEncoder[Map[String, String]].encodeJson(expected, None)
          assert(jsonStr.fromJson[Map[String, String]])(isRight(equalTo(expected)))
        },
        test("zio.Chunk") {
          val jsonStr  = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assert(jsonStr.fromJson[Chunk[String]])(isRight(equalTo(expected)))
        },
        test("java.util.UUID") {
          val ok  = """"64d7c38d-2afd-4004-9832-4e700fe400f8""""
          val bad = """"""""

          assert(ok.fromJson[UUID])(isRight(equalTo(UUID.fromString("64d7c38d-2afd-4004-9832-4e700fe400f8")))) &&
          assert(bad.fromJson[UUID])(isLeft(containsString("Invalid UUID")))
        }
      ),
      suite("fromJsonAST")(
        test("BigDecimal") {
          assert(Json.Num(123).as[BigDecimal])(isRight(equalTo(BigDecimal(123))))
        },
        test("eithers") {
          val bernies =
            List(Json.Obj("a" -> Json.Num(1)), Json.Obj("left" -> Json.Num(1)), Json.Obj("Left" -> Json.Num(1)))
          val trumps =
            List(Json.Obj("b" -> Json.Num(2)), Json.Obj("right" -> Json.Num(2)), Json.Obj("Right" -> Json.Num(2)))

          assert(bernies.map(_.as[Either[Int, Int]]))(
            forall(isRight(isLeft(equalTo(1))))
          ) && assert(trumps.map(_.as[Either[Int, Int]]))(
            forall(isRight(isRight(equalTo(2))))
          )
        },
        test("parameterless products") {
          import exampleproducts._
          assert(Json.Obj().as[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert(Json.Null.as[Parameterless])(isRight(equalTo(Parameterless()))) &&
          assert(Json.Obj("field" -> Json.Str("value")).as[Parameterless])(isRight(equalTo(Parameterless())))
        },
        test("no extra fields") {
          import exampleproducts._

          assert(Json.Obj("s" -> Json.Str("")).as[OnlyString])(isRight(equalTo(OnlyString("")))) &&
          assert(Json.Obj("s" -> Json.Str(""), "t" -> Json.Str("")).as[OnlyString])(
            isLeft(equalTo("Invalid extra field"))
          )
        },
        test("default field value") {
          import exampleproducts._

          assert(Json.Obj().as[DefaultString])(isRight(equalTo(DefaultString("")))) &&
          assert(Json.Obj("s" -> Json.Null).as[DefaultString])(isRight(equalTo(DefaultString(""))))
        },
        test("sum encoding") {
          import examplesum._

          assert(Json.Obj("Child1" -> Json.Obj()).as[Parent])(isRight(equalTo(Child1()))) &&
          assert(Json.Obj("Child2" -> Json.Obj()).as[Parent])(isRight(equalTo(Child2()))) &&
          assert(Json.Obj("type" -> Json.Str("Child1")).as[Parent])(isLeft(equalTo("Invalid disambiguator")))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert(Json.Obj("hint" -> Json.Str("Cain")).as[Parent])(isRight(equalTo(Child1()))) &&
          assert(Json.Obj("hint" -> Json.Str("Abel")).as[Parent])(isRight(equalTo(Child2()))) &&
          assert(Json.Obj("hint" -> Json.Str("Samson")).as[Parent])(isLeft(equalTo("Invalid disambiguator"))) &&
          assert(Json.Obj("Cain" -> Json.Obj()).as[Parent])(isLeft(equalTo("Missing hint 'hint'")))
        },
        test("Seq") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Seq("5XL", "2XL", "XL")

          assert(json.as[Seq[String]])(isRight(equalTo(expected)))
        },
        test("Vector") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Vector("5XL", "2XL", "XL")

          assert(json.as[Vector[String]])(isRight(equalTo(expected)))
        },
        test("SortedSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assert(json.as[immutable.SortedSet[String]])(isRight(equalTo(expected)))
        },
        test("HashSet") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assert(json.as[immutable.HashSet[String]])(isRight(equalTo(expected)))
        },
        test("Set") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Set("5XL", "2XL", "XL")

          assert(json.as[Set[String]])(isRight(equalTo(expected)))
        },
        test("Map") {
          val json     = Json.Obj("5XL" -> Json.Num(3), "2XL" -> Json.Num(14), "XL" -> Json.Num(159))
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assert(json.as[Map[String, Int]])(isRight(equalTo(expected)))
        },
        test("Map, custom keys") {
          val json     = Json.Obj("1" -> Json.Str("a"), "2" -> Json.Str("b"))
          val expected = Map(1 -> "a", 2 -> "b")

          assert(json.as[Map[Int, String]])(isRight(equalTo(expected)))
        },
        test("zio.Chunk") {
          val json     = Json.Arr(Json.Str("5XL"), Json.Str("2XL"), Json.Str("XL"))
          val expected = Chunk("5XL", "2XL", "XL")

          assert(json.as[Chunk[String]])(isRight(equalTo(expected)))
        },
        test("java.util.UUID") {
          val ok  = Json.Str("64d7c38d-2afd-4004-9832-4e700fe400f8")
          val bad = Json.Str("")

          assert(ok.as[UUID])(isRight(equalTo(UUID.fromString("64d7c38d-2afd-4004-9832-4e700fe400f8")))) &&
          assert(bad.as[UUID])(isLeft(containsString("Invalid UUID")))
        }
      ),
      suite("decode enums")(
        test("simple enum") {
          import exampleenums._

          val jsonStr  = """"Yellow""""
          val expected = Yellow

          assert(jsonStr.fromJson[Color])(isRight(equalTo(expected)))
        },
        test("case class with enum") {

          import exampleenums._

          val jsonStr  = """{"color": "Yellow", "name": "yellowStyle"}"""
          val expected = Style(name = "yellowStyle", color = Yellow)

          assert(jsonStr.fromJson[Style])(isRight(equalTo(expected)))
        }
      )
    )

  object exampleproducts {

    case class Parameterless()

    object Parameterless {

      implicit val decoder: JsonDecoder[Parameterless] =
        DeriveJsonDecoder.gen[Parameterless]
    }

    @jsonNoExtraFields
    case class OnlyString(s: String)

    object OnlyString {

      implicit val decoder: JsonDecoder[OnlyString] =
        DeriveJsonDecoder.gen[OnlyString]
    }

    case class DefaultString(s: String = "")

    object DefaultString {

      implicit val decoder: JsonDecoder[DefaultString] =
        DeriveJsonDecoder.gen[DefaultString]
    }

  }

  object examplesum {

    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    case class Child1() extends Parent

    case class Child2() extends Parent

  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    sealed abstract class Parent

    object Parent {
      implicit val decoder: JsonDecoder[Parent] = DeriveJsonDecoder.gen[Parent]
    }

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent

  }

  object logEvent {

    case class Event(at: Long, message: String)

    implicit val eventDecoder: JsonDecoder[Event] = DeriveJsonDecoder.gen[Event]
    implicit val eventEncoder: JsonEncoder[Event] = DeriveJsonEncoder.gen[Event]
  }

  object exampleenums {

    sealed trait Color
    case object Green  extends Color
    case object Yellow extends Color

    case class Style(name: String, color: Color)

    object Style {
      implicit val decoder: JsonDecoder[Style] = DeriveJsonDecoder.gen[Style]
    }
    object Color {
      implicit val decoder: JsonDecoder[Color] = DeriveJsonDecoder.gen[Color]
    }

  }
}
