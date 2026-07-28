package zio.json

import org.openjdk.jmh.annotations._

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

/**
 * Reproduction for the allocation discrepancy reported on #1649 (comparing `Array[Boolean]` decoded from a `String`
 * against the same bytes decoded via `decodeJson(Array[Byte])`), matching jsoniter-scala's `ArrayOfBooleansReading`
 * shape as closely as possible: a flat JSON array of booleans, decoded straight into an `Array[Boolean]`.
 *
 * The allocation difference between the two paths turns out not to be a stable ratio -- it is a cliff that appears at a
 * different array size for each reader, verified three independent ways (JFR allocation sampling, raw JVM GC logs, and
 * `ThreadMXBean` allocated-byte counters): below the cliff the per-element error-trace allocation in `array[A]`'s
 * decode loop (`new JsonError.ArrayAccess(i) :: trace`, unconditional, unread on the success path) gets scalar-replaced
 * away by escape analysis; above it, escape analysis gives up and the full cost is paid. Which reader crosses its cliff
 * at which size depends on how much per-character work that reader does, which is why the two paths disagree on
 * direction depending where in the sweep you look -- around size 512 bytes reads as much worse, but at size 1000+ (both
 * readers well past their cliffs) bytes reads as slightly better. See the writeup on issue #1651, which independently
 * identified the same unconditional trace allocation from a completely different (wide, shallow object) workload.
 *
 * {{{
 * jmh:run -i 5 -wi 5 -f 1 -prof gc BooleanArrayRepro.*
 * }}}
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class BooleanArrayReproBenchmarks {
  // 400/512 straddle the byte reader's cliff, 200/300 straddle the string reader's, 1000 is past both
  @Param(Array("200", "300", "400", "512", "1000"))
  var size: Int = _

  var jsonBytes: Array[Byte] = _

  @Setup
  def setup(): Unit = {
    val obj = (1 to size).map(i => ((i * 1498724053) & 0x1) == 0).toArray
    jsonBytes = obj.mkString("[", ",", "]").getBytes(UTF_8)

    assert(viaString().sameElements(viaBytes()))
  }

  @Benchmark
  def viaString(): Array[Boolean] =
    new String(jsonBytes, UTF_8).fromJson[Array[Boolean]].fold(sys.error, identity)

  @Benchmark
  def viaBytes(): Array[Boolean] =
    jsonBytes.fromJson[Array[Boolean]].fold(sys.error, identity)
}
