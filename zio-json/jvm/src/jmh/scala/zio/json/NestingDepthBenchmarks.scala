package zio.json

import org.openjdk.jmh.annotations._
import zio.json.NestingDepthBenchmarks._

import java.util.concurrent.TimeUnit

/**
 * Decoder throughput as a function of case class nesting depth alone -- field count and payload size are fixed at one
 * field, one leaf value, so this isolates depth from the width effects `ProductConstructionBenchmarks` measures.
 *
 * This exists because a try/catch based fix for issue #1651 (attach the error-trace span while unwinding a failed
 * decode, instead of consing it on every successful one) was measured to regress recursively nested case classes
 * specifically, starting around depth 3 and peaking around depth 5-7, with no `orElse` or failure involved at all -- a
 * materially bigger problem than the `orElse`-fails cost the issue anticipated, since nested case classes are far more
 * common. See the d1..d8 numbers in the class doc for the two variants compared to date.
 *
 * {{{
 * jmh:run -i 5 -wi 5 -f 2 NestingDepth.*
 * }}}
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 2)
class NestingDepthBenchmarks {
  var j1, j2, j3, j4, j5, j6, j7, j8: String = _

  @Setup
  def setup(): Unit = {
    j1 = nest(1)
    j2 = nest(2)
    j3 = nest(3)
    j4 = nest(4)
    j5 = nest(5)
    j6 = nest(6)
    j7 = nest(7)
    j8 = nest(8)

    assert(j1.fromJson[Any](d1dec).isRight)
    assert(j8.fromJson[Any](d8dec).isRight)
  }

  /** `{"w":{"w":...{"w":1}...}}`, `depth` levels deep. */
  private def nest(depth: Int): String =
    (1 until depth).foldLeft("""{"w":1}""")((acc, _) => s"""{"w":$acc}""")

  @Benchmark def d1(): Either[String, Any] = j1.fromJson[Any](d1dec)
  @Benchmark def d2(): Either[String, Any] = j2.fromJson[Any](d2dec)
  @Benchmark def d3(): Either[String, Any] = j3.fromJson[Any](d3dec)
  @Benchmark def d4(): Either[String, Any] = j4.fromJson[Any](d4dec)
  @Benchmark def d5(): Either[String, Any] = j5.fromJson[Any](d5dec)
  @Benchmark def d6(): Either[String, Any] = j6.fromJson[Any](d6dec)
  @Benchmark def d7(): Either[String, Any] = j7.fromJson[Any](d7dec)
  @Benchmark def d8(): Either[String, Any] = j8.fromJson[Any](d8dec)
}

object NestingDepthBenchmarks {
  final case class W[A](w: A)

  // one decoder per depth, each a val, derived once -- see OrElseDecoderBenchmarks for why that matters
  private implicit val i1: JsonDecoder[W[Int]]                      = DeriveJsonDecoder.gen
  private implicit val i2: JsonDecoder[W[W[Int]]]                   = DeriveJsonDecoder.gen
  private implicit val i3: JsonDecoder[W[W[W[Int]]]]                = DeriveJsonDecoder.gen
  private implicit val i4: JsonDecoder[W[W[W[W[Int]]]]]             = DeriveJsonDecoder.gen
  private implicit val i5: JsonDecoder[W[W[W[W[W[Int]]]]]]          = DeriveJsonDecoder.gen
  private implicit val i6: JsonDecoder[W[W[W[W[W[W[Int]]]]]]]       = DeriveJsonDecoder.gen
  private implicit val i7: JsonDecoder[W[W[W[W[W[W[W[Int]]]]]]]]    = DeriveJsonDecoder.gen
  private implicit val i8: JsonDecoder[W[W[W[W[W[W[W[W[Int]]]]]]]]] = DeriveJsonDecoder.gen

  val d1dec: JsonDecoder[Any] = i1.widen[Any]
  val d2dec: JsonDecoder[Any] = i2.widen[Any]
  val d3dec: JsonDecoder[Any] = i3.widen[Any]
  val d4dec: JsonDecoder[Any] = i4.widen[Any]
  val d5dec: JsonDecoder[Any] = i5.widen[Any]
  val d6dec: JsonDecoder[Any] = i6.widen[Any]
  val d7dec: JsonDecoder[Any] = i7.widen[Any]
  val d8dec: JsonDecoder[Any] = i8.widen[Any]
}
