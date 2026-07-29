package zio.json

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import org.openjdk.jmh.annotations._
import zio.json.ChunkEncoderBenchmarks._
import zio.json.TestUtils._
import zio.json.data.googlemaps._

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

/**
 * Encoding a value that is about to leave the process as bytes, which is the normal case for an HTTP response body.
 *
 * `encodeZioViaString` is what users had to write before `toJsonBytes`/`encodeJsonBytes` existed. `encodeJsoniter` is
 * there as a reference point, not as a like-for-like comparison.
 *
 * {{{
 * jmh:run -i 5 -wi 5 -f 1 ChunkEncoder.*
 * jmh:run -i 3 -wi 3 -f 1 -prof gc ChunkEncoder.*
 * }}}
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class ChunkEncoderBenchmarks {
  var decoded: DistanceMatrix = _

  @Setup
  def setup(): Unit = {
    decoded = getResourceAsString("google_maps_api_response.json").fromJson[DistanceMatrix].fold(sys.error, identity)

    assert(java.util.Arrays.equals(encodeZioBytes(), encodeZioViaString()))
    assert(java.util.Arrays.equals(encodeZioBytes(), encodeZioChunk().toArray))
  }

  /** Baseline: build a `String`, discard it immediately -- most of an app's callers do this today. */
  @Benchmark
  def encodeZioString(): CharSequence =
    JsonEncoder[DistanceMatrix].encodeJson(decoded, None)

  /** The status quo for getting bytes out: build the `String`, then re-encode it. */
  @Benchmark
  def encodeZioViaString(): Array[Byte] =
    JsonEncoder[DistanceMatrix].encodeJson(decoded, None).toString.getBytes(UTF_8)

  @Benchmark
  def encodeZioBytes(): Array[Byte] =
    decoded.toJsonBytesArray

  @Benchmark
  def encodeZioChunk(): zio.Chunk[Byte] =
    decoded.toJsonBytes

  @Benchmark
  def encodeJsoniter(): Array[Byte] =
    writeToArray(decoded)
}

object ChunkEncoderBenchmarks {
  implicit val codec: JsonValueCodec[DistanceMatrix] = JsonCodecMaker.make
}
