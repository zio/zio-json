package zio.json

import io.circe
import org.typelevel.jawn.{ ast => jawn }
import zio._
import zio.json.TestUtils._
import zio.json.ast._
import zio.json.data.googlemaps._
import zio.json.data.twitter._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.Paths

object DecoderPlatformSpecificSpec extends ZIOSpecDefault {

  val spec =
    suite("Decoder")(
      test("excessively nested structures") {
        // JVM specific: getResourceAsString not yet supported
        val testFile = "json_test_suite/n_structure_open_array_object.json"

        for {
          s <- getResourceAsStringM(testFile)
          r <- ZIO.fromEither(s.fromJson[Json]).exit
        } yield {
          assert(r)(fails(equalTo("Unexpected structure")))
        }
      },
      test("googleMapsNormal") {
        getResourceAsStringM("google_maps_api_response.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(matchesCirceDecoded[DistanceMatrix](str))
        }
      },
      test("googleMapsCompact") {
        getResourceAsStringM("google_maps_api_compact_response.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(matchesCirceDecoded[DistanceMatrix](str))
        }
      },
      test("googleMapsExtra") {
        getResourceAsStringM("google_maps_api_extra.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(matchesCirceDecoded[DistanceMatrix](str))
        }
      },
      test("googleMapsError") {
        getResourceAsStringM("google_maps_api_error_response.json").map { str =>
          assert(str.fromJson[DistanceMatrix])(isLeft(equalTo(".rows[0].elements[0].distance.value(missing)")))
        }
      },
      test("googleMapsAst") {
        val response = getResourceAsStringM("google_maps_api_response.json")
        val compact  = getResourceAsStringM("google_maps_api_compact_response.json")

        (response <&> compact).map { case (response, compact) =>
          assert(response.fromJson[Json])(equalTo(compact.fromJson[Json]))
        }
      },
      test("twitter") {
        getResourceAsStringM("twitter_api_response.json").map { str =>
          assert(str.fromJson[List[Tweet]])(matchesCirceDecoded[List[Tweet]](str))
        }
      },
      test("geojson1") {
        import zio.json.data.geojson.generated._

        getResourceAsStringM("che.geo.json").map { str =>
          assert(str.fromJson[GeoJSON])(matchesCirceDecoded[GeoJSON](str))
        }
      },
      test("geojson1 alt") {
        import zio.json.data.geojson.handrolled._

        getResourceAsStringM("che.geo.json").map { str =>
          assert(str.fromJson[GeoJSON])(matchesCirceDecoded[GeoJSON](str))
        }
      },
      test("geojson2") {
        import zio.json.data.geojson.generated._

        getResourceAsStringM("che-2.geo.json").map { str =>
          assert(str.fromJson[GeoJSON])(matchesCirceDecoded[GeoJSON](str))
        }
      },
      test("geojson2 lowlevel") {
        import zio.json.data.geojson.generated._
        // this uses a lower level Reader to ensure that the more general recorder
        // impl is covered by the tests

        getResourceAsStringM("che-2.geo.json").flatMap { str =>
          ZIO.scoped[TestEnvironment] {
            ZIO.fromAutoCloseable(ZIO.attempt(getResourceAsReader("che-2.geo.json"))).flatMap { reader =>
              for {
                circe <- ZIO.fromEither(circe.parser.decode[GeoJSON](str))
                got   <- ZIO.attemptBlocking(JsonDecoder[GeoJSON].unsafeDecode(Nil, reader))
              } yield {
                assert(got)(equalTo(circe))
              }
            }
          }
        }
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
        test("decodes a stream of chars") {
          for {
            int <- JsonDecoder[Int].decodeJsonStream(ZStream('1', '2', '3'))
          } yield {
            assert(int)(equalTo(123))
          }
        },
        test("decodes an encoded stream of bytes") {
          for {
            int <- JsonDecoder[Int].decodeJsonStreamInput(ZStream.fromIterable("123".getBytes(StandardCharsets.UTF_8)))
          } yield assert(int)(equalTo(123))
        },
        suite("decodeJsonPipeline")(
          suite("Newline delimited")(
            test("decodes single elements") {
              ZStream
                .fromIterable("1001".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001)))
                }
            },
            test("decodes multiple elements") {
              ZStream
                .fromIterable("1001\n1002".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            test("decodes multiple elements when fed in smaller chunks") {
              ZStream
                .fromIterable("1001\n1002".toSeq)
                .rechunk(1)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            test("accepts trailing NL") {
              ZStream
                .fromIterable("1001\n1002\n".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            test("errors") {
              ZStream
                .fromIterable("1\nfalse\n3".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runDrain
                .exit
                .map { exit =>
                  assert(exit)(fails(anything))
                }
            },
            test("is interruptible") {
              (ZStream.fromIterable("1\n2\n3\n4") ++ ZStream.fromZIO(ZIO.interrupt))
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runDrain
                .exit
                .map { exit =>
                  assert(exit)(isInterrupted)
                }
            } @@ timeout(7.seconds)
          ),
          suite("Array delimited")(
            test("decodes single elements") {
              ZStream
                .fromIterable("[1001]".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001)))
                }
            },
            test("empty array") {
              ZStream
                .fromIterable("[]".toSeq)
                .via(JsonDecoder[String].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(isEmpty)
                }
            },
            test("decodes multiple elements") {
              ZStream
                .fromIterable("[ 1001, 1002, 1003 ]".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002, 1003)))
                }
            },
            test("handles whitespace leniently") {
              val in =
                """[
                  1001, 1002,
                  1003
                ]"""

              ZStream
                .fromIterable(in.toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002, 1003)))
                }
            }
          )
        ),
        suite("helpers in zio.json")(
          test("readJsonLines reads from files") {
            import logEvent._

            for {
              lines <- readJsonLinesAs[Event](Paths.get("zio-json/jvm/src/test/resources/log.jsonlines")).runCollect
            } yield {
              assert(lines(0))(equalTo(Event(1603669875, "hello"))) &&
              assert(lines(1))(equalTo(Event(1603669876, "world")))
            }
          },
          test("readJsonLines reads from URLs") {
            import logEvent._

            val url = this.getClass.getClassLoader.getResource("log.jsonlines")

            for {
              lines <- readJsonLinesAs[Event](url).runCollect
            } yield {
              assert(lines(0))(equalTo(Event(1603669875, "hello"))) &&
              assert(lines(1))(equalTo(Event(1603669876, "world")))
            }
          }
        ),
        suite("combinators")(
          test("test JsonDecoder.orElse") {
            val decoder = JsonDecoder[Int].widen[AnyVal].orElse(JsonDecoder[Boolean].widen[AnyVal])
            assert(decoder.decodeJson("true"))(equalTo(Right(true.asInstanceOf[AnyVal])))
          },
          test("test hand-coded alternative in `orElse` comment") {
            val decoder: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
              case 't' | 'f' => JsonDecoder[Boolean].widen
              case c         => JsonDecoder[Int].widen
            }
            assert(decoder.decodeJson("true"))(equalTo(Right(true.asInstanceOf[AnyVal]))) &&
            assert(decoder.decodeJson("42"))(equalTo(Right(42.asInstanceOf[AnyVal]))) &&
            assert(decoder.decodeJson("\"a string\""))(equalTo(Left("(expected a number, got a)")))
          }
        )
      )
    )

  def testAst(label: String) =
    test(label) {
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
            _ <- ZIO.attemptBlocking(writeFile(gotf, e2s(got)))
            _ <- ZIO.attemptBlocking(writeFile(expectedf, e2s(expected)))
            _ <- Console.printLine(s"dumped .json files, use `cmp <(jq . ${expectedf}) <(jq . ${gotf})`")
          } yield {
            assert(got)(equalTo(expected.left.map(_.getMessage)))
          }
        } else ZIO.succeed(assertCompletes)
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

  // Helper function because Circe and Zio-JSON’s Left differ, making tests unnecessary verbose
  def matchesCirceDecoded[A](
    expected: String
  )(implicit cDecoder: circe.Decoder[A]): Assertion[Either[String, A]] = {

    val cDecoded = circe.parser.decode(expected).left.map(_.toString)

    Assertion.assertion("matchesCirceDecoded")(actual => actual == cDecoded)
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

  object logEvent {
    case class Event(at: Long, message: String)

    implicit val eventDecoder: JsonDecoder[Event] = DeriveJsonDecoder.gen[Event]
    implicit val eventEncoder: JsonEncoder[Event] = DeriveJsonEncoder.gen[Event]
  }
}
