package zio.json.javatime

import java.time.format.DateTimeFormatter
import java.time.{ OffsetDateTime, ZoneId, ZonedDateTime }

// ZonedDateTimeParser copes with a bug resulting in incorrect ZoneDateTimes in JDK 8
// (see https://bugs.openjdk.java.net/browse/JDK-8066982)
private[json] object ZonedDateTimeParser {
  def unsafeParse(input: String): ZonedDateTime =
    if (hasZone(input)) {
      val zoneId = extractZone(input)
      val odt    = OffsetDateTime.parse(input, DateTimeFormatter.ISO_ZONED_DATE_TIME)
      odt.atZoneSameInstant(zoneId)
    } else ZonedDateTime.parse(input)

  private def hasZone(isoTime: String): Boolean =
    isoTime.lastIndexOf("]") != -1

  private def extractZone(isoTime: String): ZoneId = {
    val begin = isoTime.lastIndexOf("[")
    ZoneId.of(isoTime.substring(begin + 1, isoTime.length - 1))
  }
}
