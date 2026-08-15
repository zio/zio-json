package zio.json

import zio.ZIO
import zio.json.TestUtils._
import zio.json.ast.Json
import zio.json.data.geojson.generated._
import zio.json.data.googlemaps._
import zio.json.data.twitter._
import zio.test._

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Parity of `toJsonBytes`/`toJsonBytesArray` against `toJson.getBytes(UTF_8)`, encoding real-world fixtures decoded
 * through the derived decoders -- nested objects, arrays, non-ASCII content and string escaping all at once, as opposed
 * to `ChunkEncoderSpec`'s narrower, targeted cases.
 *
 * There is no encode-side analogue of the decoder corpus sweep (`ChunkDecoderCorpusSpec`): every valid Scala value
 * encodes to something, there is no "malformed input" to defend against here, only agreement between the two output
 * paths for the same value.
 */
object ChunkEncoderCorpusSpec extends ZIOSpecDefault {

  private def parity[A: JsonDecoder: JsonEncoder](path: String): ZIO[Any, IOException, TestResult] =
    getResourceAsStringM(path).map { input =>
      val a        = input.fromJson[A].fold(e => throw new RuntimeException(e), identity)
      val expected = a.toJson.getBytes(UTF_8)

      assertTrue(
        a.toJsonBytesArray.sameElements(expected),
        a.toJsonBytes.toArray.sameElements(expected)
      )
    }

  val spec: Spec[Environment, IOException] =
    suite("ChunkEncoderCorpus")(
      test("google maps API response")(parity[DistanceMatrix]("google_maps_api_response.json")),
      test("compact google maps API response")(parity[DistanceMatrix]("google_maps_api_compact_response.json")),
      test("twitter API response")(parity[List[Tweet]]("twitter_api_response.json")),
      test("compact twitter API response")(parity[List[Tweet]]("twitter_api_compact_response.json")),
      test("GeoJSON")(parity[GeoJSON]("che.geo.json")),
      test("as the untyped AST")(parity[Json]("google_maps_api_response.json"))
    )
}
