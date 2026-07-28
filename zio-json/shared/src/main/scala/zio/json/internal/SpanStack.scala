package zio.json.internal

import zio.json.JsonError

/**
 * The path of `JsonError` spans from the top of the current decode down to the frame being decoded right now.
 *
 * Decoders used to record that path by consing onto the `trace` they were handed —
 * `child.unsafeDecode(spans(idx) :: trace, in)` — which allocates a cons cell per object field and per array element on
 * every *successful* decode, purely so that a failure *could* render `.rows[0].elements[0].value`. Issue #1651 measures
 * that at 28% of everything the decoder allocates.
 *
 * Instead each frame pushes its span before descending and pops it after, and the path is turned into the
 * `List[JsonError]` that [[zio.json.JsonError.render]] expects only when an error is actually raised, in
 * [[Lexer.error]]. The success path allocates nothing.
 *
 * A pop is deliberately *not* run when the frame below throws: the decode is unwinding all the way out to either
 * `orElse`'s catch or the top-level `decodeJson` boundary, so nothing between here and there will read the stack again,
 * and both of those restore the depth themselves ([[mark]]/[[release]]). Skipping the pop is what keeps this off the
 * `try`/`catch` machinery that made #1651's Option 1 regress nested case classes.
 *
 * Being thread-local, one instance is shared by every decode on a thread — but only one decode is ever in flight on a
 * thread at a time, and [[release]] at each top-level entry point makes a decode independent of whatever the previous
 * one left behind.
 */
private[zio] final class SpanStack {
  private[this] var spans: Array[JsonError] = new Array(32)
  private[this] var depth: Int              = 0

  /** The current depth, to be handed back to [[release]] once this subtree is done or has been abandoned. */
  @inline def mark(): Int = depth

  /** Discards everything pushed since [[mark]], whether or not it was popped. */
  @inline def release(m: Int): Unit = depth = m

  @inline def push(span: JsonError): Unit = {
    var ss = spans
    val d  = depth
    if (d == ss.length) {
      ss = java.util.Arrays.copyOf(ss, d << 1)
      spans = ss
    }
    ss(d) = span
    depth = d + 1
  }

  @inline def pop(): Unit = depth -= 1

  /**
   * The spans from here up to the top, innermost first, prepended onto `base` — the shape `JsonError.render` folds
   * over. Only ever called while raising an error.
   */
  def pathTo(base: List[JsonError]): List[JsonError] = {
    var out = base
    var i   = 0
    val d   = depth
    val ss  = spans
    while (i < d) { // spans(0) is outermost, so prepending in order leaves the innermost at the head
      out = ss(i) :: out
      i += 1
    }
    out
  }
}

private[zio] object SpanStack {
  private[this] val stacks = new ThreadLocal[SpanStack] {
    override def initialValue(): SpanStack = new SpanStack
  }

  @inline def get: SpanStack = stacks.get
}
