package zio.json

import java.io.IOException
import java.nio.file.Files

import io.circe
import testzio.json.TestUtils._
import testzio.json.data.geojson.generated._
import testzio.json.data.googlemaps._
import testzio.json.data.twitter._

import zio.Chunk
import zio.blocking.Blocking
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, assert, _ }

object EncoderPlatformSpecificSpec extends DefaultRunnableSpec {
  import testzio.json.DecoderSpec.logEvent._

  def spec: Spec[TestEnvironment, TestFailure[Any], TestSuccess] =
    suite("Encoder")(
      suite("roundtrip")(
        testRoundTrip[DistanceMatrix]("google_maps_api_response"),
        testRoundTrip[List[Tweet]]("twitter_api_response"),
        testRoundTrip[GeoJSON]("che.geo")
      ),
      suite("ZIO Streams integration")(
        testM("encodes into a ZStream of Char") {
          val intEncoder = JsonEncoder[Int]
          val value      = 1234

          for {
            chars <- intEncoder.encodeJsonStream(value, indent = None).runCollect
          } yield {
            assert(chars.mkString)(equalTo("1234"))
          }
        },
        testM("encodes values that yield a result of length > DefaultChunkSize") {
          val longString = List.fill(ZStream.DefaultChunkSize * 2)('x').mkString

          for {
            chars <- JsonEncoder[String].encodeJsonStream(longString, indent = None).runCollect
          } yield {
            assert(chars)(hasSize(equalTo(ZStream.DefaultChunkSize * 2 + 2))) &&
            assert(chars.mkString(""))(equalTo("\"" ++ longString ++ "\""))
          }
        },
        testM("encodeJsonLinesTransducer") {
          val ints = ZStream(1, 2, 3, 4)

          for {
            xs <- ints.transduce(JsonEncoder[Int].encodeJsonLinesTransducer).runCollect
          } yield {
            assert(xs.mkString)(equalTo("1\n2\n3\n4\n"))
          }
        },
        testM("encodeJsonLinesTransducer handles elements which take up > DefaultChunkSize to encode") {
          val longString = List.fill(5000)('x').mkString

          val ints    = ZStream(longString, longString)
          val encoder = JsonEncoder[String]

          for {
            xs <- ints.transduce(encoder.encodeJsonLinesTransducer).runCollect
          } yield {
            // leading `"`, trailing `"` and `\n` = 3
            assert(xs.size)(equalTo((5000 + 3) * 2))
          }
        },
        testM("encodeJsonArrayTransducer") {
          val ints = ZStream(1, 2, 3).map(n => Json.Obj(Chunk("id" -> Json.Num(BigDecimal(n).bigDecimal))))

          for {
            xs <- ints.transduce(JsonEncoder[Json].encodeJsonArrayTransducer).runCollect
          } yield {
            assert(xs.mkString)(equalTo("""[{"id":1},{"id":2},{"id":3}]"""))
          }
        }
      ),
      suite("helpers in zio.json")(
        testM("writeJsonLines writes JSON lines") {
          val path = Files.createTempFile("log", "json")
          val events = Chunk(
            Event(1603669876, "hello"),
            Event(1603669875, "world")
          )

          for {
            _  <- writeJsonLinesAs(path, ZStream.fromIterable(events))
            xs <- readJsonLinesAs[Event](path).runCollect
          } yield {
            assert(xs)(equalTo(events))
          }
        }
      )
    )

  def testRoundTrip[A: circe.Decoder: JsonEncoder](label: String): ZSpec[Blocking, IOException] =
    testM(label) {
      getResourceAsStringM(s"$label.json").map { input =>
        val circeDecoded  = circe.parser.decode[A](input)
        val circeRecoded  = circeDecoded.toOption.get.toJson
        val recodedPretty = circeDecoded.toOption.get.toJson

        assert(circe.parser.decode[A](circeRecoded))(equalTo(circeDecoded)) &&
        assert(circe.parser.decode[A](recodedPretty))(equalTo(circeDecoded))
      }
    }
}
