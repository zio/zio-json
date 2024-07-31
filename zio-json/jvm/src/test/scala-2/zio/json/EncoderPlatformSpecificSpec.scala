package zio.json

import io.circe
import testzio.json.TestUtils._
import testzio.json.data.geojson.generated._
import testzio.json.data.googlemaps._
import testzio.json.data.twitter._
import zio.Chunk
import zio.json.ast.Json
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test.{ ZIOSpecDefault, assert, _ }

import java.io.IOException
import java.nio.file.Files

object EncoderPlatformSpecificSpec extends ZIOSpecDefault {
  import testzio.json.DecoderSpec.logEvent._

  val spec =
    suite("Encoder")(
      suite("roundtrip")(
        testRoundTrip[DistanceMatrix]("google_maps_api_response"),
        testRoundTrip[List[Tweet]]("twitter_api_response"),
        testRoundTrip[GeoJSON]("che.geo")
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
