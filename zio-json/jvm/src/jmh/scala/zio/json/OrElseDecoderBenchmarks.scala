package zio.json

import org.openjdk.jmh.annotations._
import zio.json.OrElseDecoderBenchmarks._

import java.util.concurrent.TimeUnit

/**
 * Decoding through `JsonDecoder#orElse`, which uses a thrown `UnsafeJson` as ordinary control flow: when the first
 * alternative fails, the exception unwinds to the `orElse` and the second alternative is tried.
 *
 * Today that unwind is free — the frames it passes through do no work. Any change to how the error trace is carried
 * (see issue #1651: the trace is currently built on the success path, which is the decoder's single largest allocation)
 * risks making each of those frames do work on the way out, so this measures the path before such a change lands. The
 * `Fails` benchmarks are the ones at risk; `Succeeds` and `Direct` are the controls that isolate the fallback from the
 * cost of `orElse` itself.
 *
 * Depth matters: a scheme that rebuilds the trace while unwinding is quadratic in nesting depth, so a regression may be
 * invisible at depth 1 and obvious at depth 8. Compare `Shallow` against `Deep` rather than reading either alone.
 *
 * {{{
 * jmh:run -i 5 -wi 5 -f 1 OrElseDecoder.*
 * jmh:run -i 3 -wi 3 -f 1 -prof gc OrElseDecoder.*
 * }}}
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class OrElseDecoderBenchmarks {
  var shallowInt, shallowStr, deepInt, deepStr: String = _

  @Setup
  def setup(): Unit = {
    shallowInt = """{"w":1}"""
    shallowStr = """{"w":"x"}"""
    deepInt = nest(""""w":1""")
    deepStr = nest(""""w":"x"""")

    // These only measure the fallback if the first alternative really does fail and the second really does succeed,
    // so pin that here rather than trusting the shape of the JSON.
    assert(deep1.decodeJson(deepStr).isLeft, "the deep first alternative should fail")
    assert(deep1.decodeJson(deepInt).isRight, "the deep first alternative should succeed on its own payload")
    assert(shallow1.decodeJson(shallowStr).isLeft, "the shallow first alternative should fail")
    assert(decodeDeepFails().isRight)
    assert(decodeShallowFails().isRight)
    assert(decodeDeepSucceeds().isRight)
    assert(decodeDeepDirect().isRight)
  }

  /** Nesting depth 8: the first alternative descends the whole way before discovering the leaf is the wrong type. */
  private def nest(leaf: String): String = (1 until Depth).foldLeft("{" + leaf + "}")((acc, _) => s"""{"w":$acc}""")

  /** First alternative fails at the leaf, after descending one level. */
  @Benchmark
  def decodeShallowFails(): Either[String, Any] = shallow.decodeJson(shallowStr)

  /** First alternative fails at the leaf, after descending eight levels — the quadratic case. */
  @Benchmark
  def decodeDeepFails(): Either[String, Any] = deep.decodeJson(deepStr)

  /** Control: the same `orElse` and depth, but the first alternative succeeds, so nothing is thrown. */
  @Benchmark
  def decodeDeepSucceeds(): Either[String, Any] = deep.decodeJson(deepInt)

  /** Control: the same payload and depth with no `orElse` in the way at all. */
  @Benchmark
  def decodeDeepDirect(): Either[String, Any] = deep1.decodeJson(deepInt)
}

object OrElseDecoderBenchmarks {
  final val Depth = 8

  /** A wrapper that nests to any depth, so one pair of types covers every depth measured. */
  final case class W[A](w: A)

  // Every decoder is a val, derived once. An implicit def would re-derive the whole nest on every invocation and the
  // benchmarks would measure derivation rather than decoding — which is exactly what an earlier version of this file
  // did, reporting the unwrapped control as 13x slower than the wrapped one.
  private implicit val i1: JsonDecoder[W[Int]]                      = DeriveJsonDecoder.gen
  private implicit val i2: JsonDecoder[W[W[Int]]]                   = DeriveJsonDecoder.gen
  private implicit val i3: JsonDecoder[W[W[W[Int]]]]                = DeriveJsonDecoder.gen
  private implicit val i4: JsonDecoder[W[W[W[W[Int]]]]]             = DeriveJsonDecoder.gen
  private implicit val i5: JsonDecoder[W[W[W[W[W[Int]]]]]]          = DeriveJsonDecoder.gen
  private implicit val i6: JsonDecoder[W[W[W[W[W[W[Int]]]]]]]       = DeriveJsonDecoder.gen
  private implicit val i7: JsonDecoder[W[W[W[W[W[W[W[Int]]]]]]]]    = DeriveJsonDecoder.gen
  private implicit val i8: JsonDecoder[W[W[W[W[W[W[W[W[Int]]]]]]]]] = DeriveJsonDecoder.gen

  private implicit val s1: JsonDecoder[W[String]]                      = DeriveJsonDecoder.gen
  private implicit val s2: JsonDecoder[W[W[String]]]                   = DeriveJsonDecoder.gen
  private implicit val s3: JsonDecoder[W[W[W[String]]]]                = DeriveJsonDecoder.gen
  private implicit val s4: JsonDecoder[W[W[W[W[String]]]]]             = DeriveJsonDecoder.gen
  private implicit val s5: JsonDecoder[W[W[W[W[W[String]]]]]]          = DeriveJsonDecoder.gen
  private implicit val s6: JsonDecoder[W[W[W[W[W[W[String]]]]]]]       = DeriveJsonDecoder.gen
  private implicit val s7: JsonDecoder[W[W[W[W[W[W[W[String]]]]]]]]    = DeriveJsonDecoder.gen
  private implicit val s8: JsonDecoder[W[W[W[W[W[W[W[W[String]]]]]]]]] = DeriveJsonDecoder.gen

  // widened to Any so the two alternatives can be combined without a common supertype
  val shallow1: JsonDecoder[Any] = i1.widen[Any]
  val deep1: JsonDecoder[Any]    = i8.widen[Any]

  val shallow: JsonDecoder[Any] = shallow1.orElse(s1.widen[Any])
  val deep: JsonDecoder[Any]    = deep1.orElse(s8.widen[Any])
}
