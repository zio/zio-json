package zio.json.javatime

import java.time.{ Duration => JDuration }
import java.util.regex.Pattern

// The code in DurationParser is a more Scala-friendly port of the Duration.parse method from JDK 16 to cope with
// a bug resulting in incorrect durations in JDK 8 (see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978)
private[json] object DurationParser {
  // We are not allowed to instantiate java.time.format Exceptions as we cross-compile for JavaScript
  final case class DurationParseException(message: String, parsed: CharSequence, errorIndex: Int)
      extends RuntimeException(message)

  private val DurationPattern: Pattern =
    "([-+]?)P(?:([-+]?[0-9]+)D)?(T(?:([-+]?[0-9]+)H)?(?:([-+]?[0-9]+)M)?(?:([-+]?[0-9]+)(?:[.,]([0-9]{0,9}))?S)?)?".r.pattern

  private val HOURS_PER_DAY      = 24
  private val MINUTES_PER_HOUR   = 60
  private val SECONDS_PER_MINUTE = 60
  private val SECONDS_PER_HOUR   = SECONDS_PER_MINUTE * MINUTES_PER_HOUR
  private val SECONDS_PER_DAY    = SECONDS_PER_HOUR * HOURS_PER_DAY
  private val NANOS_PER_SECOND   = 1000000000L

  /**
   * Takes a text duration and turns it into a java.time.Duration
   * WARNING: unsafeParse throws exceptions
   *
   * @param text is the text input representation of a Duration
   * @return java.time.Duration
   */
  def unsafeParse(text: CharSequence): JDuration =
    if (text == null) throw new NullPointerException("text is null")
    else {
      val matcher = DurationPattern.matcher(text)
      if (matcher.matches()) {
        val negate = charMatch(text, matcher.start(1), matcher.end(1), '-')

        val dayStart = matcher.start(2)
        val dayEnd   = matcher.end(2)

        val hourStart = matcher.start(4)
        val hourEnd   = matcher.end(4)

        val minuteStart = matcher.start(5)
        val minuteEnd   = matcher.end(5)

        val secondStart = matcher.start(6)
        val secondEnd   = matcher.end(6)

        val fractionStart = matcher.start(7)
        val fractionEnd   = matcher.end(7)

        if (dayStart >= 0 || hourStart >= 0 || minuteStart >= 0 || secondStart >= 0) {
          val daysAsSecs   = parseNumber(text, dayStart, dayEnd, SECONDS_PER_DAY, "days")
          val hoursAsSecs  = parseNumber(text, hourStart, hourEnd, SECONDS_PER_HOUR, "hours")
          val minsAsSecs   = parseNumber(text, minuteStart, minuteEnd, SECONDS_PER_MINUTE, "minutes")
          val seconds      = parseNumber(text, secondStart, secondEnd, 1, "seconds")
          val negativeSecs = secondStart >= 0 && text.charAt(secondStart) == '-'
          val nanos        = parseFraction(text, fractionStart, fractionEnd, if (negativeSecs) -1 else 1)
          createDuration(negate, daysAsSecs, hoursAsSecs, minsAsSecs, seconds, nanos)
        } else
          throw DurationParseException(
            "text matched pattern for Duration but day/hour/minute/second fraction was less than 0",
            text,
            0
          )
      } else throw DurationParseException("text cannot be parsed to a Duration", text, 0)
    }

  private def charMatch(text: CharSequence, start: Int, end: Int, c: Char): Boolean =
    start >= 0 && end == start + 1 && text.charAt(start) == c

  private def parseNumber(
    text: CharSequence,
    start: Int,
    end: Int,
    multiplier: Int,
    errorText: String
  ): Long =
    // regex limits to [-+]?[0-9]+
    if (start < 0 || end < 0) 0L
    else
      try {
        val long = java.lang.Long.parseLong(text.subSequence(start, end).toString, 10)
        Math.multiplyExact(long, multiplier)
      } catch {
        case _: NumberFormatException =>
          throw DurationParseException(
            s"text cannot be parsed to a number whilst parsing a Duration: " + errorText,
            text,
            0
          )

        case _: ArithmeticException =>
          throw DurationParseException(
            s"text cannot be parsed to a number whilst parsing a Duration: " + errorText,
            text,
            0
          )
      }

  private def parseFraction(text: CharSequence, start: Int, end: Int, negate: Int): Int =
    // regex limits to [0-9]{0,9}
    if (start < 0 || end < 0 || end - start == 0) 0
    else
      try {
        var fraction = text.subSequence(start, end).toString.toInt
        // for number strings smaller than 9 digits, interpret as if there were trailing zeros
        (end - start).until(9).foreach(_ => fraction *= 10)
        fraction * negate
      } catch {
        case _: NumberFormatException =>
          throw DurationParseException("Text cannot be parsed to a Duration: fraction", text, 0)

        case _: ArithmeticException =>
          throw DurationParseException("Text cannot be parsed to a Duration: fraction", text, 0)
      }

  private def createDuration(
    negate: Boolean,
    daysAsSecs: Long,
    hoursAsSecs: Long,
    minsAsSecs: Long,
    secs: Long,
    nanos: Long
  ): JDuration = {
    val seconds  = Math.addExact(daysAsSecs, Math.addExact(hoursAsSecs, Math.addExact(minsAsSecs, secs)))
    val duration = ofSecondsAndNanos(seconds, nanos)
    if (negate) duration.negated()
    else duration
  }

  private def ofSecondsAndNanos(seconds: Long, nanoAdjustment: Long): JDuration = {
    val secs = Math.addExact(seconds, Math.floorDiv(nanoAdjustment, NANOS_PER_SECOND))
    val nos  = Math.floorMod(nanoAdjustment, NANOS_PER_SECOND).toInt
    if ((secs | nos) == 0) JDuration.ZERO
    else JDuration.ofSeconds(seconds, nanoAdjustment)
  }
}
