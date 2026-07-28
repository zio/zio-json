package zio.json

import org.openjdk.jmh.annotations._
import zio.json.NestingDepthBenchmarks.{ W, d1dec, d3dec, d5dec, d8dec }
import zio.json.SpanStackBenchmarks._
import zio.json.internal.{ Lexer, RetractReader, StringMatrix }

import java.util.concurrent.TimeUnit

/**
 * Prototype for issue #1651's A1 Option 2: replace the per-field `spans(idx) :: trace` cons-cell with a push/pop
 * against a mutable counter -- no `try`/`catch` anywhere on the success path -- and check whether that avoids both the
 * allocation Option 1 was meant to remove *and* the nesting-depth throughput regression that disqualified Option 1 (see
 * `NestingDepthBenchmarks`), at the exact same depths and JSON shape.
 *
 * `pushed` hand-writes what a macro-emitted Option 2 decoder would do for `W[A]`: increment a depth counter before
 * decoding the field, decrement after, unconditionally, on the plain success path. If the inner decode throws, the
 * decrement is skipped -- deliberately: the whole point of Option 2 is that nothing needs to run on the way back up
 * through a failure, because the decode is aborting all the way out regardless (to `orElse`'s catch or the top-level
 * `decodeJson` boundary), not returning through this frame normally.
 *
 * The counter lives in a `ThreadLocal[Array[Int]]` single cell here, standing in for "wherever the real stack would
 * live" -- the open design question from #1651. A `ThreadLocal` has a known correctness hole for a real implementation:
 * nothing resets it at every possible entry point other than `decodeJson` (`unsafeDecode` is public and can be called
 * directly), so a stray direct call on the same thread would inherit whatever depth a prior decode left behind. Hanging
 * the stack off the reader instead (constructed fresh per top-level decode, so naturally scoped with no reset needed)
 * is the safer answer for a real implementation, at the cost of touching the sealed `RetractReader` hierarchy. This
 * prototype uses the simpler `ThreadLocal` because it only ever runs one decode per thread per benchmark invocation, so
 * the hole it has does not affect what is being measured here.
 *
 * What this does NOT prototype: `Lexer.error`'s signature would need to gain access to the stack to attach spans at
 * throw time (it only takes `trace` today, not the reader), so on a malformed or missing field, `pushed`'s error
 * message is missing the intermediate `.w` spans a real implementation would include. Only successful decodes are
 * measured or asserted here, same as `NestingDepthBenchmarks`. `unsafeFromJsonAST`'s parallel trace-prepend mechanism
 * is untouched entirely -- it has no reader to hang a stack off, so it needs its own design if ever tackled, separate
 * from this one.
 *
 * {{{
 * jmh:run -i 5 -wi 5 -f 2 SpanStack.*
 * jmh:run -i 3 -wi 3 -f 1 -prof gc SpanStack.*
 * }}}
 */
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 2)
class SpanStackBenchmarks {
  var j1, j3, j5, j8: String = _

  @Setup
  def setup(): Unit = {
    j1 = nest(1)
    j3 = nest(3)
    j5 = nest(5)
    j8 = nest(8)

    assert(j1.fromJson[Any](d1dec).isRight)
    assert(j1.fromJson[Any](d1dec) == j1.fromJson[Any](pushed1))
    assert(j3.fromJson[Any](d3dec) == j3.fromJson[Any](pushed3))
    assert(j5.fromJson[Any](d5dec) == j5.fromJson[Any](pushed5))
    assert(j8.fromJson[Any](d8dec) == j8.fromJson[Any](pushed8))
  }

  /** `{"w":{"w":...{"w":1}...}}`, `depth` levels deep -- identical to `NestingDepthBenchmarks.nest`. */
  private def nest(depth: Int): String =
    (1 until depth).foldLeft("""{"w":1}""")((acc, _) => s"""{"w":$acc}""")

  @Benchmark def derivedD1(): Either[String, Any] = j1.fromJson[Any](d1dec)
  @Benchmark def derivedD3(): Either[String, Any] = j3.fromJson[Any](d3dec)
  @Benchmark def derivedD5(): Either[String, Any] = j5.fromJson[Any](d5dec)
  @Benchmark def derivedD8(): Either[String, Any] = j8.fromJson[Any](d8dec)

  @Benchmark def pushedD1(): Either[String, Any] = j1.fromJson[Any](pushed1)
  @Benchmark def pushedD3(): Either[String, Any] = j3.fromJson[Any](pushed3)
  @Benchmark def pushedD5(): Either[String, Any] = j5.fromJson[Any](pushed5)
  @Benchmark def pushedD8(): Either[String, Any] = j8.fromJson[Any](pushed8)
}

object SpanStackBenchmarks {

  // stands in for "wherever the real span stack would live" -- see the class doc for why this has a known hole
  // that doesn't matter for what's being measured here
  private val depthTL = new ThreadLocal[Array[Int]] {
    override def initialValue(): Array[Int] = new Array(1)
  }

  /** What a macro-emitted Option 2 decoder would do: push/pop a depth counter, never cons the trace. */
  final class PushedWDecoder[A](inner: JsonDecoder[A]) extends JsonDecoder[W[A]] {
    private[this] val matrix = new StringMatrix(Array("w"))

    def unsafeDecode(trace: List[JsonError], in: RetractReader): W[A] = {
      Lexer.char(trace, in, '{')
      var w    = null.asInstanceOf[A]
      var seen = false
      if (Lexer.firstField(trace, in))
        while ({
          val idx = Lexer.field(trace, in, matrix)
          if (idx == 0) {
            seen = true
            val d = depthTL.get
            d(0) += 1
            w = inner.unsafeDecode(trace, in) // same trace, unchanged: no allocation
            d(0) -= 1                         // plain and unconditional -- skipped on a throw, which is fine, see the class doc
          } else Lexer.skipValue(trace, in)
          Lexer.nextField(trace, in)
        }) ()
      if (!seen) Lexer.error("missing", JsonError.ObjectAccess("w") :: trace)
      new W(w)
    }
  }

  private val p1 = new PushedWDecoder(JsonDecoder[Int])
  private val p2 = new PushedWDecoder[W[Int]](p1)
  private val p3 = new PushedWDecoder[W[W[Int]]](p2)
  private val p4 = new PushedWDecoder[W[W[W[Int]]]](p3)
  private val p5 = new PushedWDecoder[W[W[W[W[Int]]]]](p4)
  private val p6 = new PushedWDecoder[W[W[W[W[W[Int]]]]]](p5)
  private val p7 = new PushedWDecoder[W[W[W[W[W[W[Int]]]]]]](p6)
  private val p8 = new PushedWDecoder[W[W[W[W[W[W[W[Int]]]]]]]](p7)

  val pushed1: JsonDecoder[Any] = p1.widen[Any]
  val pushed3: JsonDecoder[Any] = p3.widen[Any]
  val pushed5: JsonDecoder[Any] = p5.widen[Any]
  val pushed8: JsonDecoder[Any] = p8.widen[Any]
}
