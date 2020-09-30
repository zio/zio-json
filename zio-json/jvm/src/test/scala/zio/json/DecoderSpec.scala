package testzio.json

import scala.collection.immutable

import io.circe
import org.typelevel.jawn.{ ast => jawn }
import testzio.json.TestUtils._
import testzio.json.data.googlemaps._
import testzio.json.data.twitter._

import zio.blocking._
import zio.duration._
import zio.json._
import zio.json.ast._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.environment.{ Live }
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
      testM("googleMapsNormal") {
        getResourceAsStringM("google_maps_api_response.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(matchesCirceDecoded[DistanceMatrix](str))
        }
      },
      testM("googleMapsCompact") {
        getResourceAsStringM("google_maps_api_compact_response.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(matchesCirceDecoded[DistanceMatrix](str))
        }
      },
      testM("googleMapsExtra") {
        getResourceAsStringM("google_maps_api_extra.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(matchesCirceDecoded[DistanceMatrix](str))
        }
      },
      testM("googleMapsError") {
        getResourceAsStringM("google_maps_api_error_response.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(isLeft(equalTo(".rows[0].elements[0].distance.value(missing)")))
        }
      },
      testM("googleMapsAst") {
        val response = getResourceAsStringM("google_maps_api_response.json")
        val compact  = getResourceAsStringM("google_maps_api_compact_response.json")

        (response <&> compact).map { case (response, compact) =>
          assert(response.fromJson[Json])(equalTo(compact.fromJson[Json]))
        }
      },
      testM("twitter") {
        getResourceAsStringM("twitter_api_response.json").map { str =>
          assert(str.fromJson[List[Tweet]])(matchesCirceDecoded[List[Tweet]](str))
        }
      },
      testM("geojson1") {
        import testzio.json.data.geojson.generated._

        getResourceAsStringM("che.geo.json").map { str =>
          assert(str.fromJson[GeoJSON])(matchesCirceDecoded[GeoJSON](str))
        }
      },
      testM("geojson1 alt") {
        import testzio.json.data.geojson.handrolled._

        getResourceAsStringM("che.geo.json").map { str =>
          assert(str.fromJson[GeoJSON])(matchesCirceDecoded[GeoJSON](str))
        }
      },
      testM("geojson2") {
        import testzio.json.data.geojson.generated._

        getResourceAsStringM("che-2.geo.json").map { str =>
          assert(str.fromJson[GeoJSON])(matchesCirceDecoded[GeoJSON](str))
        }
      },
      testM("geojson2 lowlevel") {
        import testzio.json.data.geojson.generated._
        // this uses a lower level Reader to ensure that the more general recorder
        // impl is covered by the tests

        getResourceAsStringM("che-2.geo.json").flatMap { str =>
          ZManaged.fromAutoCloseable(Task(getResourceAsReader("che-2.geo.json"))).use { reader =>
            for {
              circe <- ZIO.fromEither(circe.parser.decode[GeoJSON](str))
              got   <- effectBlocking(JsonDecoder[GeoJSON].unsafeDecode(Nil, reader))
            } yield {
              assert(got)(equalTo(circe))
            }
          }
        }
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
      },
      suite("jawn")(
        testAst("bar"),
        testAst("bla25"),
        testAst("bla2"),
        testAst("countries.geo"),
        testAst("dkw-sample"),
        testAst("foo"),
        testAst("qux1"),
        testAst("qux2"),
        testAst("ugh10k")
      ),
      suite("ZIO Streams integration")(
        testM("decodes a stream of chars") {
          for {
            int <- JsonDecoder[Int].decodeJsonStream(ZStream('1', '2', '3'))
          } yield {
            assert(int)(equalTo(123))
          }
        },
        suite("decodeJsonTransducer")(
          suite("Newline delimited")(
            testM("decodes single elements") {
              ZStream
                .fromIterable("1001".toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001)))
                }
            },
            testM("decodes multiple elements") {
              ZStream
                .fromIterable("1001\n1002".toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            testM("decodes multiple elements when fed in smaller chunks") {
              ZStream
                .fromIterable("1001\n1002".toSeq)
                .chunkN(1)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            testM("accepts trailing NL") {
              ZStream
                .fromIterable("1001\n1002\n".toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            testM("errors") {
              ZStream
                .fromIterable("1\nfalse\n3".toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Newline))
                .runDrain
                .run
                .map { exit =>
                  assert(exit)(fails(anything))
                }
            },
            testM("is interruptible") {
              (ZStream.fromIterable("1\n2\n3\n4") ++ ZStream.fromEffect(ZIO.interrupt))
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Newline))
                .runDrain
                .run
                .map { exit =>
                  assert(exit)(isInterrupted)
                }
            } @@ timeout(2.seconds)
          ),
          suite("Array delimited")(
            testM("decodes single elements") {
              ZStream
                .fromIterable("[1001]".toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001)))
                }
            },
            testM("decodes multiple elements") {
              ZStream
                .fromIterable("[ 1001, 1002, 1003 ]".toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002, 1003)))
                }
            },
            testM("handles whitespace leniently") {
              val in =
                """[
                  1001, 1002,
                  1003
                ]"""

              ZStream
                .fromIterable(in.toSeq)
                .transduce(JsonDecoder[Int].decodeJsonTransducer(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002, 1003)))
                }
            }
          )
        )
      )
    )

  def testAst(label: String): ZSpec[Blocking with zio.console.Console, Throwable] =
    testM(label) {
      getResourceAsStringM(s"jawn/$label.json").flatMap { input =>
        val expected = jawn.JParser.parseFromString(input).toEither.map(fromJawn)
        val got      = input.fromJson[Json].map(normalize)

        def e2s[A, B](e: Either[A, B]) =
          e match {
            case Left(left)   => left.toString
            case Right(right) => right.toString
          }

        if (expected != got) {
          val gotf      = s"${label}-got.json"
          val expectedf = s"${label}-expected.json"

          for {
            _ <- effectBlocking(writeFile(gotf, e2s(got)))
            _ <- effectBlocking(writeFile(expectedf, e2s(expected)))
            _ <- console.putStrLn(s"dumped .json files, use `cmp <(jq . ${expectedf}) <(jq . ${gotf})`")
          } yield {
            assert(got)(equalTo(expected.left.map(_.getMessage)))
          }
        } else ZIO.effectTotal(assertCompletes)
      }
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

  // reorder objects to match jawn's lossy AST (and dedupe)
  def normalize(ast: Json): Json =
    ast match {
      case Json.Obj(values) =>
        Json.Obj(
          Chunk
            .fromIterable(
              values
                .groupBy(_._1)
                .map(_._2.head)
            )
            .map { case (k, v) => (k, normalize(v)) }
            .sortBy(_._1)
        )
      case Json.Arr(values) => Json.Arr(values.map(normalize(_)))
      case other            => other
    }

  // Helper function because Circe and Zio-JSON’s Left differ, making tests unnecessarly verbose
  def matchesCirceDecoded[A](
    expected: String
  )(implicit cDecoder: circe.Decoder[A], eq: Eql[A, A]): Assertion[Either[String, A]] = {
    import zio.test.Assertion.Render._

    val cDecoded = circe.parser.decode(expected).left.map(_.toString)

    Assertion.assertion("matchesCirceDecoded")(param(cDecoded))(actual => actual == cDecoded)
  }

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
}
