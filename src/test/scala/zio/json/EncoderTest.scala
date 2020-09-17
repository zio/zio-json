package zio.json

import scala.collection.immutable

import io.circe
import zio.json
import zio.json._
import TestUtils._
import scalaprops._
import Property.{ implies, prop, property }
import scala.collection.mutable

import utest._
import zio.json.data.googlemaps._
import zio.json.data.twitter._

// testOnly *EncoderTest
object EncoderTest extends TestSuite {

  object exampleproducts {
    case class Parameterless()
    object Parameterless {
      implicit val encoder: json.JsonEncoder[Parameterless] =
        json.DeriveJsonEncoder.gen[Parameterless]
    }

    case class OnlyString(s: String)
    object OnlyString {
      implicit val encoder: json.JsonEncoder[OnlyString] =
        json.DeriveJsonEncoder.gen[OnlyString]
    }

    case class CoupleOfThings(@json.field("j") i: Int, f: Option[Float], b: Boolean)
    object CoupleOfThings {
      implicit val encoder: json.JsonEncoder[CoupleOfThings] =
        json.DeriveJsonEncoder.gen[CoupleOfThings]
    }
  }

  object examplesum {

    sealed abstract class Parent
    object Parent {
      implicit val encoder: json.JsonEncoder[Parent] = json.DeriveJsonEncoder.gen[Parent]
    }
    case class Child1() extends Parent
    @json.hint("Cain")
    case class Child2() extends Parent
  }

  object examplealtsum {

    @json.discriminator("hint")
    sealed abstract class Parent
    object Parent {
      implicit val encoder: json.JsonEncoder[Parent] = json.DeriveJsonEncoder.gen[Parent]
    }
    case class Child1() extends Parent
    @json.hint("Abel")
    case class Child2(s: Option[String]) extends Parent
  }

  val tests = Tests {
    test("primitives") {
      "hello world".toJson ==> "\"hello world\""
      "hello\nworld".toJson ==> "\"hello\\nworld\""
      "hello\rworld".toJson ==> "\"hello\\rworld\""
      "hello\u0000world".toJson ==> "\"hello\\u0000world\""

      true.toJson ==> "true"
      false.toJson ==> "false"
      'c'.toJson ==> "\"c\""
      Symbol("c").toJson ==> "\"c\""

      (1: Byte).toJson ==> "1"
      (1: Short).toJson ==> "1"
      (1: Int).toJson ==> "1"
      (1L).toJson ==> "1"
      (new java.math.BigInteger("1")).toJson ==> "1"
      (new java.math.BigInteger("170141183460469231731687303715884105728").toJson ==> "170141183460469231731687303715884105728")

      (1.0f).toJson ==> "1.0"
      (1.0d).toJson ==> "1.0"

      Float.NaN.toJson ==> "\"NaN\""
      Float.PositiveInfinity.toJson ==> "\"Infinity\""
      Float.NegativeInfinity.toJson ==> "\"-Infinity\""

      Double.NaN.toJson ==> "\"NaN\""
      Double.PositiveInfinity.toJson ==> "\"Infinity\""
      Double.NegativeInfinity.toJson ==> "\"-Infinity\""
    }

    test("options") {
      (None: Option[Int]).toJson ==> "null"
      (Some(1): Option[Int]).toJson ==> "1"
    }

    test("eithers") {
      (Left(1): Either[Int, Int]).toJson ==> """{"Left":1}"""
      (Right(1): Either[Int, Int]).toJson ==> """{"Right":1}"""

      (Left(1): Either[Int, Int]).toJsonPretty ==> "{\n  \"Left\" : 1\n}"
      (Right(1): Either[Int, Int]).toJsonPretty ==> "{\n  \"Right\" : 1\n}"
    }

    test("collections") {
      List[Int]().toJson ==> "[]"
      List(1, 2, 3).toJson ==> "[1,2,3]"
      Vector[Int]().toJson ==> "[]"
      Vector(1, 2, 3).toJson ==> "[1,2,3]"

      Map[String, String]().toJson ==> "{}"
      Map("hello" -> "world").toJson ==> """{"hello":"world"}"""
      Map("hello" -> Some("world"), "goodbye" -> None).toJson ==> """{"hello":"world"}"""

      List[Int]().toJsonPretty ==> "[]"
      List(1, 2, 3).toJsonPretty ==> "[1, 2, 3]"
      Vector[Int]().toJsonPretty ==> "[]"
      Vector(1, 2, 3).toJsonPretty ==> "[1, 2, 3]"

      Map[String, String]().toJsonPretty ==> "{}"
      Map("hello" -> "world").toJsonPretty ==> "{\n  \"hello\" : \"world\"\n}"
      Map("hello" -> Some("world"), "goodbye" -> None).toJsonPretty ==> "{\n  \"hello\" : \"world\"\n}"
    }

    test("parameterless products") {
      import exampleproducts._

      Parameterless().toJson ==> "{}"

      Parameterless().toJsonPretty ==> "{}"
    }

    test("tuples") {
      ("hello", "world").toJson ==> """["hello","world"]"""

      ("hello", "world").toJsonPretty ==> """["hello", "world"]"""
    }

    test("products") {
      import exampleproducts._

      OnlyString("foo").toJson ==> """{"s":"foo"}"""

      CoupleOfThings(-1, Some(10.0f), false).toJson ==> """{"j":-1,"f":10.0,"b":false}"""
      CoupleOfThings(0, None, true).toJson ==> """{"j":0,"b":true}"""

      OnlyString("foo").toJsonPretty ==> "{\n  \"s\" : \"foo\"\n}"

      CoupleOfThings(-1, Some(10.0f), false).toJsonPretty ==> "{\n  \"j\" : -1,\n  \"f\" : 10.0,\n  \"b\" : false\n}"
      CoupleOfThings(0, None, true).toJsonPretty ==> "{\n  \"j\" : 0,\n  \"b\" : true\n}"
    }

    test("sum encoding") {
      import examplesum._

      (Child1(): Parent).toJson ==> """{"Child1":{}}"""
      (Child2(): Parent).toJson ==> """{"Cain":{}}"""

      (Child1(): Parent).toJsonPretty ==> "{\n  \"Child1\" : {}\n}"
      (Child2(): Parent).toJsonPretty ==> "{\n  \"Cain\" : {}\n}"
    }

    test("sum alternative encoding") {
      import examplealtsum._

      (Child1(): Parent).toJson ==> """{"hint":"Child1"}"""
      (Child2(None): Parent).toJson ==> """{"hint":"Abel"}"""
      (Child2(Some("hello")): Parent).toJson ==> """{"hint":"Abel","s":"hello"}"""

      // note lack of whitespace on last line
      (Child1(): Parent).toJsonPretty ==> "{\n  \"hint\" : \"Child1\"}"
      (Child2(None): Parent).toJsonPretty ==> "{\n  \"hint\" : \"Abel\"}"
      (Child2(Some("hello")): Parent).toJsonPretty ==> "{\n  \"hint\" : \"Abel\",\n  \"s\" : \"hello\"\n}"
    }

    // using circe to avoid entwining this test on zio.json.JsonDecoder
    def testRoundtrip[A: circe.Decoder: JsonEncoder](res: String) = {
      val jsonString = getResourceAsString(res)
      val decoded    = circe.parser.decode[A](jsonString)
      val recoded    = decoded.toOption.get.toJson
      circe.parser.decode[A](recoded) ==> decoded

      val recodedPretty = decoded.toOption.get.toJson
      circe.parser.decode[A](recodedPretty) ==> decoded
    }

    test("Google Maps") {
      testRoundtrip[DistanceMatrix]("google_maps_api_response.json")
    }

    test("Twitter") {
      testRoundtrip[List[Tweet]]("twitter_api_response.json")
    }

    test("GeoJSON") {
      import zio.json.data.geojson.generated._

      testRoundtrip[GeoJSON]("che.geo.json")
    }
  }

}
