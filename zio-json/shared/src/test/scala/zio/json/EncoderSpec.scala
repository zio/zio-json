package testzio.json

import zio.Chunk
import zio.json._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.util.UUID
import scala.collection.{ immutable, mutable }

// zioJsonJVM/testOnly testzio.json.EncoderSpec
object EncoderSpec extends DefaultRunnableSpec {

  def spec: Spec[Annotations, TestFailure[Any], TestSuccess] =
    suite("Encoder")(
      suite("toJson")(
        suite("primitives")(
          test("strings") {
            assert("hello world".toJson)(equalTo("\"hello world\"")) &&
            assert("hello\nworld".toJson)(equalTo("\"hello\\nworld\"")) &&
            assert("hello\rworld".toJson)(equalTo("\"hello\\rworld\"")) &&
            assert("hello\u0000world".toJson)(equalTo("\"hello\\u0000world\""))
          },
          test("boolean") {
            assert(true.toJson)(equalTo("true")) &&
            assert(false.toJson)(equalTo("false"))
          },
          test("char") {
            assert('c'.toJson)(equalTo("\"c\"")) &&
            assert(Symbol("c").toJson)(equalTo("\"c\""))
          },
          test("numerics") {
            val exampleBigIntStr     = "170141183460469231731687303715884105728"
            val exampleBigDecimalStr = "170141183460469231731687303715884105728.4433"
            assert((1: Byte).toJson)(equalTo("1")) &&
            assert((1: Short).toJson)(equalTo("1")) &&
            assert((1: Int).toJson)(equalTo("1")) &&
            assert(1L.toJson)(equalTo("1")) &&
            assert(new java.math.BigInteger("1").toJson)(equalTo("1")) &&
            assert(new java.math.BigInteger(exampleBigIntStr).toJson)(equalTo(exampleBigIntStr)) &&
            assert(BigInt(exampleBigIntStr).toJson)(equalTo(exampleBigIntStr)) &&
            assert(BigDecimal(exampleBigDecimalStr).toJson)(equalTo(exampleBigDecimalStr)) &&
            assert(1.0f.toJson)(equalTo("1.0")) &&
            assert(1.0d.toJson)(equalTo("1.0"))
          } @@ jvmOnly,
          test("NaN / Infinity") {
            assert(Float.NaN.toJson)(equalTo("\"NaN\"")) &&
            assert(Float.PositiveInfinity.toJson)(equalTo("\"Infinity\"")) &&
            assert(Float.NegativeInfinity.toJson)(equalTo("\"-Infinity\"")) &&
            assert(Double.NaN.toJson)(equalTo("\"NaN\"")) &&
            assert(Double.PositiveInfinity.toJson)(equalTo("\"Infinity\"")) &&
            assert(Double.NegativeInfinity.toJson)(equalTo("\"-Infinity\""))
          }
        ),
        test("options") {
          assert((None: Option[Int]).toJson)(equalTo("null")) &&
          assert((Some(1): Option[Int]).toJson)(equalTo("1"))
        },
        test("eithers") {
          assert((Left(1): Either[Int, Int]).toJson)(equalTo("""{"Left":1}""")) &&
          assert((Right(1): Either[Int, Int]).toJson)(equalTo("""{"Right":1}""")) &&
          assert((Left(1): Either[Int, Int]).toJsonPretty)(equalTo("{\n  \"Left\" : 1\n}")) &&
          assert((Right(1): Either[Int, Int]).toJsonPretty)(equalTo("{\n  \"Right\" : 1\n}"))
        },
        test("collections") {
          assert(List[Int]().toJson)(equalTo("[]")) &&
          assert(List(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Vector[Int]().toJson)(equalTo("[]")) &&
          assert(Vector(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Seq[Int]().toJson)(equalTo("[]")) &&
          assert(Seq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(IndexedSeq[Int]().toJson)(equalTo("[]")) &&
          assert(IndexedSeq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.LinearSeq[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.LinearSeq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.ListSet[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.ListSet(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.TreeSet[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.TreeSet(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Array[Int]().toJson)(equalTo("[]")) &&
          assert(Array(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(Map[String, String]().toJson)(equalTo("{}")) &&
          assert(Map("hello" -> "world").toJson)(equalTo("""{"hello":"world"}""")) &&
          assert(mutable.Map("hello" -> "world").toJson)(equalTo("""{"hello":"world"}""")) &&
          assert(Map("hello" -> Some("world"), "goodbye" -> None).toJson)(equalTo("""{"hello":"world"}""")) &&
          assert(List[Int]().toJsonPretty)(equalTo("[]")) &&
          assert(List(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
          assert(Vector[Int]().toJsonPretty)(equalTo("[]")) &&
          assert(Vector(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
          assert(Seq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(Seq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(IndexedSeq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(IndexedSeq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(immutable.LinearSeq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.LinearSeq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(immutable.ListSet[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.ListSet("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]")) &&
          assert(immutable.TreeSet[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.TreeSet("bar", "foo").toJsonPretty)(equalTo("[\n  \"bar\",\n  \"foo\"\n]")) &&
          assert(Array[Int]().toJsonPretty)(equalTo("[]")) &&
          assert(Array(1, 2, 3).toJsonPretty)(equalTo("[\n  1,\n  2,\n  3\n]")) &&
          assert(Map[String, String]().toJsonPretty)(equalTo("{}")) &&
          assert(Map("hello" -> "world").toJsonPretty)(equalTo("{\n  \"hello\" : \"world\"\n}")) &&
          assert(Map("hello" -> Some("world"), "goodbye" -> None).toJsonPretty)(
            equalTo("{\n  \"hello\" : \"world\"\n}")
          )
        },
        test("Map, custom keys") {
          assert(Map(1 -> "a").toJson)(equalTo("""{"1":"a"}"""))
        },
        test("java.util.UUID") {
          assert(UUID.fromString("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd").toJson)(
            equalTo(""""e142f1aa-6e9e-4352-adfe-7e6eb9814ccd"""")
          )
        },
        test("parameterless products") {
          import exampleproducts._

          assert(Parameterless().toJson)(equalTo("{}")) &&
          assert(Parameterless().toJsonPretty)(equalTo("{}"))
        },
        test("tuples") {
          assert(("hello", "world").toJson)(equalTo("""["hello","world"]""")) &&
          assert(("hello", "world").toJsonPretty)(equalTo("""["hello", "world"]"""))
        },
        test("products") {
          import exampleproducts._

          assert(OnlyString("foo").toJson)(equalTo("""{"s":"foo"}""")) &&
          assert(CoupleOfThings(-1, Some(10.0f), false).toJson)(equalTo("""{"j":-1,"f":10.0,"b":false}""")) &&
          assert(CoupleOfThings(0, None, true).toJson)(equalTo("""{"j":0,"b":true}""")) &&
          assert(OnlyString("foo").toJsonPretty)(equalTo("{\n  \"s\" : \"foo\"\n}")) &&
          assert(CoupleOfThings(-1, Some(10.0f), false).toJsonPretty)(
            equalTo("{\n  \"j\" : -1,\n  \"f\" : 10.0,\n  \"b\" : false\n}")
          ) &&
          assert(CoupleOfThings(0, None, true).toJsonPretty)(equalTo("{\n  \"j\" : 0,\n  \"b\" : true\n}")) &&
          assert(OptionalAndRequired(None, "foo").toJson)(equalTo("""{"s":"foo"}"""))
        } @@ jvmOnly,
        test("sum encoding") {
          import examplesum._

          assert((Child1(): Parent).toJson)(equalTo("""{"Child1":{}}""")) &&
          assert((Child2(): Parent).toJson)(equalTo("""{"Cain":{}}""")) &&
          assert((Child1(): Parent).toJsonPretty)(equalTo("{\n  \"Child1\" : {}\n}")) &&
          assert((Child2(): Parent).toJsonPretty)(equalTo("{\n  \"Cain\" : {}\n}"))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          // note lack of whitespace on last line
          assert((Child1(): Parent).toJson)(equalTo("""{"hint":"Child1"}""")) &&
          assert((Child2(None): Parent).toJson)(equalTo("""{"hint":"Abel"}""")) &&
          assert((Child2(Some("hello")): Parent).toJson)(equalTo("""{"hint":"Abel","s":"hello"}""")) &&
          assert((Child1(): Parent).toJsonPretty)(equalTo("{\n  \"hint\" : \"Child1\"}")) &&
          assert((Child2(None): Parent).toJsonPretty)(equalTo("{\n  \"hint\" : \"Abel\"}")) &&
          assert((Child2(Some("hello")): Parent).toJsonPretty)(
            equalTo("{\n  \"hint\" : \"Abel\",\n  \"s\" : \"hello\"\n}")
          )
        },
        test("exclude fields") {
          import exampleexcludefield._
          assert(Person("Peter", 20).toJson)(equalTo("""{"name":"Peter"}"""))
        }
      ),
      suite("toJsonAST")(
        suite("primitives")(
          test("strings") {
            assert("hello world".toJsonAST)(isRight(equalTo(Json.Str("hello world"))))
          },
          test("boolean") {
            assert(true.toJsonAST)(isRight(equalTo(Json.Bool(true)))) &&
            assert(false.toJsonAST)(isRight(equalTo(Json.Bool(false))))
          },
          test("char") {
            assert('c'.toJsonAST)(isRight(equalTo(Json.Str("c"))))
          },
          test("numerics") {
            assert((1: Byte).toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert((1: Short).toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert((1: Int).toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(1L.toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(new java.math.BigInteger("1").toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(1.0f.toJsonAST)(isRight(equalTo(Json.Num(1)))) &&
            assert(1.0d.toJsonAST)(isRight(equalTo(Json.Num(1))))
          } @@ jvmOnly
        ),
        test("options") {
          assert((None: Option[Int]).toJsonAST)(isRight(equalTo(Json.Null))) &&
          assert((Some(1): Option[Int]).toJsonAST)(isRight(equalTo(Json.Num(1))))
        },
        test("eithers") {
          assert((Left(1): Either[Int, Int]).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Left" -> Json.Num(1)))))) &&
          assert((Right(1): Either[Int, Int]).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Right" -> Json.Num(1))))))
        },
        test("collections") {
          val arrEmpty = Json.Arr()
          val arr123   = Json.Arr(Json.Num(1), Json.Num(2), Json.Num(3))
          val objEmpty = Json.Obj()
          val objHW    = Json.Obj("hello" -> Json.Str("world"))

          assert(List[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(List(1, 2, 3).toJsonAST)(isRight(equalTo(arr123))) &&
          assert(Vector[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(Vector(1, 2, 3).toJsonAST)(isRight(equalTo(arr123))) &&
          assert(Map[String, String]().toJsonAST)(isRight(equalTo(objEmpty))) &&
          assert(Map("hello" -> "world").toJsonAST)(isRight(equalTo(objHW))) &&
          assert(Map("hello" -> Some("world"), "goodbye" -> None).toJsonAST)(isRight(equalTo(objHW)))
        },
        test("java.util.UUID") {
          assert(UUID.fromString("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd").toJsonAST)(
            isRight(equalTo(Json.Str("e142f1aa-6e9e-4352-adfe-7e6eb9814ccd")))
          )
        },
        test("parameterless products") {
          import exampleproducts._

          assert(Parameterless().toJsonAST)(isRight(equalTo(Json.Obj())))
        },
        test("tuples") {
          assert(("hello", "world").toJsonAST)(isRight(equalTo(Json.Arr(Json.Str("hello"), Json.Str("world")))))
        },
        test("products") {
          import exampleproducts._

          assert(OnlyString("foo").toJsonAST)(isRight(equalTo(Json.Obj("s" -> Json.Str("foo"))))) &&
          assert(CoupleOfThings(-1, Some(10.0f), false).toJsonAST)(
            isRight(equalTo(Json.Obj("j" -> Json.Num(-1), "f" -> Json.Num(10), "b" -> Json.Bool(false))))
          ) &&
          assert(CoupleOfThings(0, None, true).toJsonAST)(
            isRight(equalTo(Json.Obj("j" -> Json.Num(0), "b" -> Json.Bool(true))))
          ) &&
          assert(OptionalAndRequired(None, "foo").toJsonAST)(isRight(equalTo(Json.Obj("s" -> Json.Str("foo")))))
        } @@ jvmOnly,
        test("sum encoding") {
          import examplesum._

          assert((Child1(): Parent).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Child1" -> Json.Obj()))))) &&
          assert((Child2(): Parent).toJsonAST)(isRight(equalTo(Json.Obj(Chunk("Cain" -> Json.Obj())))))
        },
        test("sum alternative encoding") {
          import examplealtsum._

          assert((Child1(): Parent).toJsonAST)(isRight(equalTo(Json.Obj("hint" -> Json.Str("Child1"))))) &&
          assert((Child2(None): Parent).toJsonAST)(isRight(equalTo(Json.Obj("hint" -> Json.Str("Abel"))))) &&
          assert((Child2(Some("hello")): Parent).toJsonAST)(
            (isRight(equalTo(Json.Obj("s" -> Json.Str("hello"), "hint" -> Json.Str("Abel")))))
          )
        }
      )
    )

  object exampleproducts {

    case class Parameterless()

    object Parameterless {

      implicit val encoder: JsonEncoder[Parameterless] =
        DeriveJsonEncoder.gen[Parameterless]
    }

    case class OnlyString(s: String)

    object OnlyString {

      implicit val encoder: JsonEncoder[OnlyString] =
        DeriveJsonEncoder.gen[OnlyString]
    }

    case class CoupleOfThings(@jsonField("j") i: Int, f: Option[Float], b: Boolean)

    object CoupleOfThings {

      implicit val encoder: JsonEncoder[CoupleOfThings] =
        DeriveJsonEncoder.gen[CoupleOfThings]
    }

    case class OptionalAndRequired(i: Option[Int], s: String)

    object OptionalAndRequired {

      implicit val encoder: JsonEncoder[OptionalAndRequired] =
        DeriveJsonEncoder.gen[OptionalAndRequired]
    }

  }

  object exampleexcludefield {

    case class Person(name: String, @jsonExclude age: Int)

    object Person {
      implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
    }

  }

  object examplesum {

    sealed abstract class Parent

    object Parent {
      implicit val encoder: JsonEncoder[Parent] = DeriveJsonEncoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Cain")
    case class Child2() extends Parent

  }

  object examplealtsum {

    @jsonDiscriminator("hint")
    sealed abstract class Parent

    object Parent {
      implicit val encoder: JsonEncoder[Parent] = DeriveJsonEncoder.gen[Parent]
    }

    case class Child1() extends Parent

    @jsonHint("Abel")
    case class Child2(s: Option[String]) extends Parent

  }

}
