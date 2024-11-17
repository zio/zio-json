package testzio.json

import zio.test.Gen

import java.math.BigInteger
import java.time.{ ZoneId, _ }
import scala.jdk.CollectionConverters._
import scala.util.Try

object Gens {
  val genBigInteger: Gen[Any, BigInteger] =
    Gen
      .bigInt((BigInt(2).pow(128) - 1) * -1, BigInt(2).pow(128) - 1)
      .map(_.bigInteger)
      .filter(_.bitLength < 128)

  val genBigDecimal: Gen[Any, java.math.BigDecimal] =
    Gen
      .bigDecimal((BigDecimal(2).pow(128) - 1) * -1, BigDecimal(2).pow(128) - 1)
      .map(_.bigDecimal)
      .filter(_.toBigInteger.bitLength < 128)

  val genUsAsciiString: Gen[Any, String] =
    Gen.string(Gen.oneOf(Gen.char('!', '~')))

  val genAlphaLowerString: Gen[Any, String] =
    Gen.string(Gen.oneOf(Gen.char('a', 'z')))

  val genYear: Gen[Any, Year] =
    Gen.oneOf(Gen.int(-9999, 9999), Gen.int(-999999999, 999999999)).map(Year.of)

  val genLocalDate: Gen[Any, LocalDate] = for {
    year  <- genYear
    month <- Gen.int(1, 12)
    day   <- Gen.int(1, Month.of(month).length(year.isLeap))
  } yield LocalDate.of(year.getValue, month, day)

  val genLocalTime: Gen[Any, LocalTime] = for {
    hour   <- Gen.int(0, 23)
    minute <- Gen.int(0, 59)
    second <- Gen.int(0, 59)
    nano   <- Gen.int(0, 999999999)
  } yield LocalTime.of(hour, minute, second, nano)

  val genInstant: Gen[Any, Instant] = for {
    epochSecond     <- Gen.long(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
    nanoAdjustment  <- Gen.long(Long.MinValue, Long.MaxValue)
    fallbackInstant <- Gen.elements(Instant.MIN, Instant.EPOCH, Instant.MAX)
  } yield Try(Instant.ofEpochSecond(epochSecond, nanoAdjustment)).getOrElse(fallbackInstant)

  val genZoneOffset: Gen[Any, ZoneOffset] = Gen.oneOf(
    Gen.int(-18, 18).map(ZoneOffset.ofHours),
    Gen.int(-18 * 60, 18 * 60).map(x => ZoneOffset.ofHoursMinutes(x / 60, x % 60)),
    Gen.int(-18 * 60 * 60, 18 * 60 * 60).map(ZoneOffset.ofTotalSeconds)
  )

  val genZoneId: Gen[Any, ZoneId] = Gen.oneOf(
    genZoneOffset,
    genZoneOffset.map(zo => ZoneId.ofOffset("UT", zo)),
    genZoneOffset.map(zo => ZoneId.ofOffset("UTC", zo)),
    genZoneOffset.map(zo => ZoneId.ofOffset("GMT", zo)),
    Gen.elements(ZoneId.getAvailableZoneIds.asScala.toSeq: _*).map(ZoneId.of),
    Gen.elements(ZoneId.SHORT_IDS.values().asScala.toSeq: _*).map(ZoneId.of)
  )

  val genLocalDateTime: Gen[Any, LocalDateTime] = for {
    localDate <- genLocalDate
    localTime <- genLocalTime
  } yield LocalDateTime.of(localDate, localTime)

  val genZonedDateTime: Gen[Any, ZonedDateTime] = for {
    localDateTime <- genLocalDateTime
    zoneId        <- genZoneId
  } yield ZonedDateTime.of(localDateTime, zoneId)

  val genDuration: Gen[Any, Duration] = Gen.oneOf(
    Gen.long(Long.MinValue / 86400, Long.MaxValue / 86400).map(Duration.ofDays),
    Gen.long(Long.MinValue / 3600, Long.MaxValue / 3600).map(Duration.ofHours),
    Gen.long(Long.MinValue / 60, Long.MaxValue / 60).map(Duration.ofMinutes),
    Gen.long(Long.MinValue, Long.MaxValue).map(Duration.ofSeconds),
    Gen.long(Int.MinValue, Int.MaxValue.toLong).map(Duration.ofMillis),
    Gen.long(Int.MinValue, Int.MaxValue.toLong).map(Duration.ofNanos)
  )

  val genMonthDay: Gen[Any, MonthDay] = for {
    month <- Gen.int(1, 12)
    day   <- Gen.int(1, 29)
  } yield MonthDay.of(month, day)

  val genOffsetDateTime: Gen[Any, OffsetDateTime] = for {
    localDateTime <- genLocalDateTime
    zoneOffset    <- genZoneOffset
  } yield OffsetDateTime.of(localDateTime, zoneOffset)

  val genOffsetTime: Gen[Any, OffsetTime] = for {
    localTime  <- genLocalTime
    zoneOffset <- genZoneOffset
  } yield OffsetTime.of(localTime, zoneOffset)

  val genPeriod: Gen[Any, Period] = for {
    year  <- Gen.int
    month <- Gen.int
    day   <- Gen.int
  } yield Period.of(year, month, day)

  val genYearMonth: Gen[Any, YearMonth] = for {
    year  <- genYear
    month <- Gen.int(1, 12)
  } yield YearMonth.of(year.getValue, month)

  val genDayOfWeek: Gen[Any, DayOfWeek] = Gen.int(1, 7).map(DayOfWeek.of)

  val genMonth: Gen[Any, Month] = Gen.int(1, 12).map(Month.of)
}
