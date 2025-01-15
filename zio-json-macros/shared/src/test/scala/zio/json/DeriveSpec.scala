package zio.json

import zio.test.Assertion._
import zio.test._

object DeriveSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("DeriveCodec")(
      suite("Decoding")(
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
        }
      ) @@ TestAspect.flaky // flaky only for Scala Native
    )

  object exampleproducts {
    @jsonDerive
    case class Parameterless()

    @jsonDerive
    @jsonNoExtraFields
    case class OnlyString(s: String)
  }

  object examplesum {
    @jsonDerive
    sealed abstract class Parent

    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object examplesumhintnames {
    @jsonDerive
    @jsonHintNames(SnakeCase)
    sealed abstract class Parent

    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object exampleempty {
    @jsonDerive
    case class Empty(a: Option[String])

  }

  object examplealtsum {

    @jsonDerive
    @jsonDiscriminator("hint")
    sealed abstract class Parent

    @jsonHint("Cain")
    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent
  }

  object examplealtsumhintnames {

    @jsonDerive
    @jsonDiscriminator("hint")
    @jsonHintNames(SnakeCase)
    sealed abstract class Parent

    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2() extends Parent
  }

  object logEvent {
    @jsonDerive(JsonDeriveConfig.Decoder)
    case class Event(at: Long, message: String, a: Seq[String] = Nil)
  }

}
