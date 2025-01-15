package zio.json

import io.circe
import zio.Chunk
import zio.json.TestUtils._
import zio.json.ast.Json
import zio.json.data.geojson.generated._
import zio.json.data.googlemaps._
import zio.json.data.twitter._
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test.{ ZIOSpecDefault, assert, _ }

import java.io.IOException
import java.nio.file.Files

object EncoderPlatformSpecificSpec extends ZIOSpecDefault {
  import zio.json.DecoderSpec.logEvent._

  val spec =
    suite("Encoder")(
      suite("roundtrip")(
        testRoundTrip[DistanceMatrix]("google_maps_api_response"),
        testRoundTrip[List[Tweet]]("twitter_api_response"),
        testRoundTrip[GeoJSON]("che.geo")
      ),
      suite("ZIO Streams integration")(
        test("encodes into a ZStream of Char") {
          val intEncoder = JsonEncoder[Int]
          val value      = 1234

          for {
            chars <- intEncoder.encodeJsonStream(value).runCollect
          } yield {
            assert(chars.mkString)(equalTo("1234"))
          }
        },
        test("encodes values that yield a result of length > DefaultChunkSize") {
          val longString = List.fill(ZStream.DefaultChunkSize * 2)('x').mkString

          for {
            chars <- JsonEncoder[String].encodeJsonStream(longString).runCollect
          } yield {
            assert(chars)(hasSize(equalTo(ZStream.DefaultChunkSize * 2 + 2))) &&
            assert(chars.mkString(""))(equalTo("\"" ++ longString ++ "\""))
          }
        },
        test("encodeJsonLinesPipeline") {
          val ints = ZStream(1, 2, 3, 4)

          for {
            xs <- ints.via(JsonEncoder[Int].encodeJsonLinesPipeline).runCollect
          } yield {
            assert(xs.mkString)(equalTo("1\n2\n3\n4\n"))
          }
        },
        test("encodeJsonLinesPipeline handles elements which take up > DefaultChunkSize to encode") {
          val longString = List.fill(5000)('x').mkString

          val ints    = ZStream(longString, longString)
          val encoder = JsonEncoder[String]

          for {
            xs <- ints.via(encoder.encodeJsonLinesPipeline).runCollect
          } yield {
            // leading `"`, trailing `"` and `\n` = 3
            assert(xs.size)(equalTo((5000 + 3) * 2))
          }
        },
        test("encodeJsonArrayPipeline XYZ") {
          val ints = ZStream(1, 2, 3).map(n => Json.Obj(Chunk("id" -> Json.Num(BigDecimal(n).bigDecimal))))

          for {
            xs <- ints.via(JsonEncoder[Json].encodeJsonArrayPipeline).runCollect
          } yield {
            assert(xs.mkString)(equalTo("""[{"id":1},{"id":2},{"id":3}]"""))
          }
        },
        test("encodeJsonArrayPipeline, empty stream") {
          val emptyArray = ZStream
            .from(List())
            .via(JsonEncoder[String].encodeJsonArrayPipeline)
            .run(ZSink.mkString)

          assertZIO(emptyArray)(equalTo("[]"))
        }
      ),
      suite("helpers in zio.json")(
        test("writeJsonLines writes JSON lines") {
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

  def testRoundTrip[A: circe.Decoder: JsonEncoder](label: String): Spec[Any, IOException] =
    test(label) {
      getResourceAsStringM(s"$label.json").map { input =>
        val circeDecoded  = circe.parser.decode[A](input)
        val circeRecoded  = circeDecoded.toOption.get.toJson
        val recodedPretty = circeDecoded.toOption.get.toJson

        assert(circe.parser.decode[A](circeRecoded))(equalTo(circeDecoded)) &&
        assert(circe.parser.decode[A](recodedPretty))(equalTo(circeDecoded))
      }
    }
}
