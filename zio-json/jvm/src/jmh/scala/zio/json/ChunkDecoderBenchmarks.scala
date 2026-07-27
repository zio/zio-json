package zio.json

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import org.openjdk.jmh.annotations._
import zio.json.ChunkDecoderBenchmarks._
import zio.json.TestUtils._
import zio.json.data.googlemaps._
import zio.stream.ZStream
import zio.{ Chunk, Runtime, Unsafe }

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

/**
 * Decoding a payload that arrived as bytes, which is the normal case for an HTTP response body.
 *
 * `decodeZioChunkViaString` is what users had to write before `decodeJson(Chunk[Byte])` existed, and
 * `decodeZioStreamInput` is the only allocation-bounded option that was available. `decodeJsoniterBytes` is there as a
 * reference point, not as a like-for-like comparison.
 *
 * jmh:run -i 5 -wi 5 -f1 ChunkDecoder.*
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class ChunkDecoderBenchmarks {
  var jsonString: String       = _
  var jsonBytes: Array[Byte]   = _
  var chunk: Chunk[Byte]       = _
  var chunkConcat: Chunk[Byte] = _

  private[this] val runtime = Runtime.default

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("google_maps_api_response.json")
    jsonBytes = jsonString.getBytes(UTF_8)
    chunk = Chunk.fromArray(jsonBytes)
    // what a body assembled from several network reads looks like: a Chunk.Concat, not a single array
    chunkConcat = jsonBytes.grouped(math.max(jsonBytes.length / 8, 1)).foldLeft(Chunk.empty[Byte]) { (acc, bs) =>
      acc ++ Chunk.fromArray(bs)
    }

    assert(decodeZioChunk() == decodeZioString())
    assert(decodeZioChunkConcat() == decodeZioString())
    assert(decodeZioChunkViaString() == decodeZioString())
    assert(decodeZioStreamInput() == decodeZioString())
  }

  /** Baseline: the payload is already a `String`, no byte handling involved. */
  @Benchmark
  def decodeZioString(): Either[String, DistanceMatrix] =
    jsonString.fromJson[DistanceMatrix]

  /** The status quo: copy the bytes into a `String`, then parse that. */
  @Benchmark
  def decodeZioChunkViaString(): Either[String, DistanceMatrix] =
    new String(chunk.toArray, UTF_8).fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioChunk(): Either[String, DistanceMatrix] =
    chunk.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioChunkConcat(): Either[String, DistanceMatrix] =
    chunkConcat.fromJson[DistanceMatrix]

  /**
   * The other allocation-bounded option, via `InputStream` + `InputStreamReader`. The score includes a
   * `Runtime.unsafe.run` round trip, so read it as the cost of the whole call, not of the parse alone.
   */
  @Benchmark
  def decodeZioStreamInput(): Either[String, DistanceMatrix] =
    Unsafe.unsafe { implicit u: Unsafe =>
      runtime.unsafe
        .run(JsonDecoder[DistanceMatrix].decodeJsonStreamInput(ZStream.fromChunk(chunk)).either)
        .getOrThrow()
        .left
        .map(_.getMessage)
    }

  @Benchmark
  def decodeJsoniterBytes(): DistanceMatrix =
    readFromArray[DistanceMatrix](jsonBytes)
}

object ChunkDecoderBenchmarks {
  implicit val codec: JsonValueCodec[DistanceMatrix] = JsonCodecMaker.make
}
