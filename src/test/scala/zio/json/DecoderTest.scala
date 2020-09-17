package zio.json

import scala.collection.immutable

import zio.Chunk
import zio.json
import zio.json.ast._
import io.circe
import TestUtils._
import scalaprops._
import Property.{ implies, prop, property }
import org.typelevel.jawn.{ ast => jawn }
import scala.collection.mutable

import utest._
import zio.json.data.googlemaps._
import zio.json.data.twitter._

// testOnly *DecoderTest
object DecoderTest extends TestSuite {

  object exampleproducts {
    case class Parameterless()
    object Parameterless {
      implicit val decoder: json.JsonDecoder[Parameterless] =
        json.DeriveJsonDecoder.gen[Parameterless]
    }

    @json.no_extra_fields
    case class OnlyString(s: String)
    object OnlyString {
      implicit val decoder: json.JsonDecoder[OnlyString] =
        json.DeriveJsonDecoder.gen[OnlyString]
    }
  }

  object examplesum {

    sealed abstract class Parent
    object Parent {
      implicit val decoder: json.JsonDecoder[Parent] = json.DeriveJsonDecoder.gen[Parent]
    }
    case class Child1() extends Parent
    case class Child2() extends Parent
  }

  object examplealtsum {

    @json.discriminator("hint")
    sealed abstract class Parent
    object Parent {
      implicit val decoder: json.JsonDecoder[Parent] = json.DeriveJsonDecoder.gen[Parent]
    }
    @json.hint("Cain")
    case class Child1() extends Parent
    @json.hint("Abel")
    case class Child2() extends Parent
  }

  val tests = Tests {
    test("primitives") {
      // this big integer consumes more than 128 bits
      json.parser.decode[java.math.BigInteger]("170141183460469231731687303715884105728") ==> Left(
        "(expected a 128 bit BigInteger)"
      )
    }

    test("eithers") {
      val bernies = List("""{"a":1}""", """{"left":1}""", """{"Left":1}""")
      val trumps  = List("""{"b":2}""", """{"right":2}""", """{"Right":2}""")

      bernies.foreach(s => json.parser.decode[Either[Int, Int]](s) ==> Right(Left(1)))

      trumps.foreach(s => json.parser.decode[Either[Int, Int]](s) ==> Right(Right(2)))

    }

    test("parameterless products") {
      import exampleproducts._
      json.parser.decode[Parameterless]("""{}""") ==> Right(Parameterless())

      // actually anything works... consider this a canary test because if only
      // the empty object is supported that's fine.
      json.parser.decode[Parameterless]("""null""") ==> Right(Parameterless())
      json.parser.decode[Parameterless]("""{"field":"value"}""") ==> Right(
        Parameterless()
      )
    }

    test("no extra fields") {
      import exampleproducts._

      json.parser.decode[OnlyString]("""{"s":""}""") ==> Right(OnlyString(""))

      json.parser.decode[OnlyString]("""{"s":"","t":""}""") ==> Left(
        "(invalid extra field)"
      )
    }

    test("sum encoding") {
      import examplesum._
      json.parser.decode[Parent]("""{"Child1":{}}""") ==> Right(Child1())
      json.parser.decode[Parent]("""{"Child2":{}}""") ==> Right(Child2())
      json.parser.decode[Parent]("""{"type":"Child1"}""") ==> Left(
        "(invalid disambiguator)"
      )
    }

    test("sum alternative encoding") {
      import examplealtsum._

      json.parser.decode[Parent]("""{"hint":"Cain"}""") ==> Right(Child1())
      json.parser.decode[Parent]("""{"hint":"Abel"}""") ==> Right(Child2())
      json.parser.decode[Parent]("""{"hint":"Samson"}""") ==> Left(
        "(invalid disambiguator)"
      )
      json.parser.decode[Parent]("""{"Cain":{}}""") ==> Left(
        "(missing hint 'hint')"
      )
    }

    test("googleMapsNormal") {
      val jsonString = getResourceAsString("google_maps_api_response.json")
      parser.decode[DistanceMatrix](jsonString) ==>
        circe.parser.decode[DistanceMatrix](jsonString)
    }

    test("googleMapsCompact") {
      val jsonStringCompact =
        getResourceAsString("google_maps_api_compact_response.json")
      parser.decode[DistanceMatrix](jsonStringCompact) ==>
        circe.parser.decode[DistanceMatrix](jsonStringCompact)
    }

    test("googleMapsExtra") {
      val jsonStringExtra = getResourceAsString("google_maps_api_extra.json")
      parser.decode[DistanceMatrix](jsonStringExtra) ==>
        circe.parser.decode[DistanceMatrix](jsonStringExtra)
    }

    test("googleMapsError") {
      val jsonStringErr =
        getResourceAsString("google_maps_api_error_response.json")
      parser.decode[DistanceMatrix](jsonStringErr) ==
        Left(".rows[0].elements[0].distance.value(missing)")
    }

    test("googleMapsAst") {
      parser.decode[Json](
        getResourceAsString("google_maps_api_response.json")
      ) ==>
        parser.decode[Json](
          getResourceAsString("google_maps_api_compact_response.json")
        )
    }

    test("twitter") {
      val input = getResourceAsString("twitter_api_response.json")
      val expected =
        circe.parser.decode[List[Tweet]](input)
      val got = json.parser.decode[List[Tweet]](input)
      got ==> expected
    }

    test("geojson1") {
      import zio.json.data.geojson.generated._
      val input    = getResourceAsString("che.geo.json")
      val expected = circe.parser.decode[GeoJSON](input)
      val got      = json.parser.decode[GeoJSON](input)
      got ==> expected
    }

    test("geojson1 alt") {
      import zio.json.data.geojson.handrolled._
      val input    = getResourceAsString("che.geo.json")
      val expected = circe.parser.decode[GeoJSON](input)
      val got      = json.parser.decode[GeoJSON](input)
      got ==> expected
    }

    test("geojson2") {
      import zio.json.data.geojson.generated._
      val input    = getResourceAsString("che-2.geo.json")
      val expected = circe.parser.decode[GeoJSON](input)
      val got      = json.parser.decode[GeoJSON](input)
      got ==> expected
    }

    test("geojson2 lowlevel") {
      import zio.json.data.geojson.generated._
      // this uses a lower level Reader to ensure that the more general recorder
      // impl is covered by the tests
      val expected =
        circe.parser.decode[GeoJSON](getResourceAsString("che-2.geo.json"))
      val input = getResourceAsReader("che-2.geo.json")
      val got   = json.JsonDecoder[GeoJSON].unsafeDecode(Chunk.empty, input)
      input.close()
      Right(got) ==> expected
    }

    test("unicode") {
      json.parser.decode[String](""""€🐵🥰"""") ==> Right("€🐵🥰")
    }

    // collections tests contributed by Piotr Paradziński
    test("Seq") {
      val jsonStr  = """["5XL","2XL","XL"]"""
      val expected = Seq("5XL", "2XL", "XL")
      json.parser.decode[Seq[String]](jsonStr) ==> Right(expected)
    }

    test("Vector") {
      val jsonStr  = """["5XL","2XL","XL"]"""
      val expected = Vector("5XL", "2XL", "XL")
      json.parser.decode[Vector[String]](jsonStr) ==> Right(expected)
    }

    test("SortedSet") {
      val jsonStr  = """["5XL","2XL","XL"]"""
      val expected = immutable.SortedSet("5XL", "2XL", "XL")
      json.parser.decode[immutable.SortedSet[String]](jsonStr) ==> Right(expected)
    }

    test("HashSet") {
      val jsonStr  = """["5XL","2XL","XL"]"""
      val expected = immutable.HashSet("5XL", "2XL", "XL")
      json.parser.decode[immutable.HashSet[String]](jsonStr) ==> Right(expected)
    }

    test("Set") {
      val jsonStr  = """["5XL","2XL","XL"]"""
      val expected = Set("5XL", "2XL", "XL")
      json.parser.decode[Set[String]](jsonStr) ==> Right(expected)
    }

    test("Map") {
      val jsonStr  = """{"5XL":3,"2XL":14,"XL":159}"""
      val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)
      json.parser.decode[Map[String, Int]](jsonStr) ==> Right(expected)
    }

    test("jawn test data: bar") {
      testAst("bar")
    }

    test("jawn test data: bla25") {
      testAst("bla25")
    }

    test("jawn test data: bla2") {
      testAst("bla2")
    }

    test("jawn test data: countries.geo") {
      testAst("countries.geo")
    }

    test("jawn test data: dkw-sample") {
      testAst("dkw-sample")
    }

    test("jawn test data: foo") {
      testAst("foo")
    }

    test("jawn test data: qux1") {
      testAst("qux1")
    }

    test("jawn test data: qux2") {
      testAst("qux2")
    }

    test("jawn test data: ugh10k") {
      testAst("ugh10k")
    }

    // TODO it would be good to test with https://github.com/nst/JSONTestSuite
  }

  def testAst(name: String) = {
    val input     = getResourceAsString(s"jawn/${name}.json")
    val expected  = jawn.JParser.parseFromString(input).toEither.map(fromJawn)
    val got       = parser.decode[Json](input).map(normalize)
    val gotf      = s"${name}-got.json"
    val expectedf = s"${name}-expected.json"

    def e2s[A, B](e: Either[A, B]) =
      e match {
        case Left(left)   => left.toString
        case Right(right) => right.toString
      }
    if (expected != got) {
      writeFile(gotf, e2s(got))
      writeFile(expectedf, e2s(expected))
    }
    scala.Predef.assert(
      got == expected,
      s"dumped .json files, use `cmp <(jq . ${expectedf}) <(jq . ${gotf})`"
    ) // errors are too big
  }

  // reorder objects to match jawn's lossy AST (and dedupe)
  def normalize(ast: Json): Json =
    ast match {
      case Json.Obj(values) =>
        Json.Obj(
          values
            .distinctBy(_._1)
            .map { case (k, v) => (k, normalize(v)) }
            .sortBy(_._1)
        )
      case Json.Arr(values) => Json.Arr(values.map(normalize(_)))
      case other           => other
    }

  def fromJawn(ast: jawn.JValue): Json =
    ast match {
      case jawn.JNull      => Json.Null
      case jawn.JTrue      => Json.Bool(true)
      case jawn.JFalse     => Json.Bool(false)
      case jawn.JString(s) => Json.Str(s)
      case jawn.LongNum(i) =>
        Json.Num(new java.math.BigDecimal(java.math.BigInteger.valueOf(i)))
      case jawn.DoubleNum(d) => Json.Num(new java.math.BigDecimal(d))
      case jawn.DeferLong(i) =>
        Json.Num(new java.math.BigDecimal(new java.math.BigInteger(i)))
      case jawn.DeferNum(n) => Json.Num(new java.math.BigDecimal(n))
      case jawn.JArray(vs)  => Json.Arr(Chunk.fromArray(vs).map(fromJawn))
      case jawn.JObject(es) =>
        Json.Obj(Chunk.fromIterable(es).sortBy(_._1).map { case (k, v) => (k, fromJawn(v)) })
    }

}
