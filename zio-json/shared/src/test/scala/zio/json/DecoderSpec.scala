package testzio.json

import scala.collection.immutable

import zio.json._
import zio.test.Assertion._
import zio.test.environment.Live
import zio.test.{ DefaultRunnableSpec, _ }
import zio.{ test => _, _ }

object DecoderSpec extends DefaultRunnableSpec {
  def spec: Spec[ZEnv with Live, TestFailure[Any], TestSuccess] =
    suite("Decoder")(
      test("primitives") {
        // this big integer consumes more than 128 bits
        assert("170141183460469231731687303715884105728".fromJson[java.math.BigInteger])(
          isLeft(equalTo("(expected a 128 bit BigInteger)"))
        )
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
      test("zio.Chunk") {
        val jsonStr  = """["5XL","2XL","XL"]"""
        val expected = Chunk("5XL", "2XL", "XL")

        assert(jsonStr.fromJson[Chunk[String]])(isRight(equalTo(expected)))
      }
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
}
