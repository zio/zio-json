package testzio.json

import java.time._
import java.time.format.DateTimeFormatter

import zio.blocking._
import zio.json._
import zio.test.Assertion._
import zio.test._

// zioJsonJVM/testOnly testzio.json.JavaTimeSpec
object JavaTimeSpec extends DefaultRunnableSpec {
  private def stringify(s: Any): String = s""" "${s.toString}" """

  private def equalToStringified(expected: String) = equalTo(s""""$expected"""")

  def spec: ZSpec[Blocking, Any] =
    suite("java.time")(
      suite("Encoder")(
        test("DayOfWeek") {
          assert(DayOfWeek.MONDAY.toJson)(equalToStringified("MONDAY")) &&
          assert(DayOfWeek.TUESDAY.toJson)(equalToStringified("TUESDAY")) &&
          assert(DayOfWeek.WEDNESDAY.toJson)(equalToStringified("WEDNESDAY")) &&
          assert(DayOfWeek.THURSDAY.toJson)(equalToStringified("THURSDAY")) &&
          assert(DayOfWeek.FRIDAY.toJson)(equalToStringified("FRIDAY")) &&
          assert(DayOfWeek.SATURDAY.toJson)(equalToStringified("SATURDAY")) &&
          assert(DayOfWeek.SUNDAY.toJson)(equalToStringified("SUNDAY"))
        },
        test("Duration") {
          assert(Duration.ofDays(1).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofHours(24).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofMinutes(1440).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofSeconds(Long.MaxValue, 999999999L).toJson)(
            equalToStringified("PT2562047788015215H30M7.999999999S")
          )
        },
        test("Instant") {
          val n = Instant.now()
          assert(Instant.EPOCH.toJson)(equalToStringified("1970-01-01T00:00:00Z")) &&
          assert(n.toJson)(equalToStringified(n.toString))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2020, 1, 1)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_DATE))) &&
          assert(p.toJson)(equalToStringified("2020-01-01"))
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))) &&
          assert(p.toJson)(equalToStringified("2020-01-01T12:36:00"))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_TIME))) &&
          assert(p.toJson)(equalToStringified("12:36:00"))
        },
        test("Month") {
          assert(Month.JANUARY.toJson)(equalToStringified("JANUARY")) &&
          assert(Month.FEBRUARY.toJson)(equalToStringified("FEBRUARY")) &&
          assert(Month.MARCH.toJson)(equalToStringified("MARCH")) &&
          assert(Month.APRIL.toJson)(equalToStringified("APRIL")) &&
          assert(Month.MAY.toJson)(equalToStringified("MAY")) &&
          assert(Month.JUNE.toJson)(equalToStringified("JUNE")) &&
          assert(Month.JULY.toJson)(equalToStringified("JULY")) &&
          assert(Month.AUGUST.toJson)(equalToStringified("AUGUST")) &&
          assert(Month.SEPTEMBER.toJson)(equalToStringified("SEPTEMBER")) &&
          assert(Month.OCTOBER.toJson)(equalToStringified("OCTOBER")) &&
          assert(Month.NOVEMBER.toJson)(equalToStringified("NOVEMBER")) &&
          assert(Month.DECEMBER.toJson)(equalToStringified("DECEMBER"))
        },
        test("MonthDay") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(p.toJson)(equalToStringified("--01-01"))
        },
        test("OffsetDateTime") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))) &&
          assert(p.toJson)(equalToStringified("2020-01-01T12:36:12Z"))
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_OFFSET_TIME))) &&
          assert(p.toJson)(equalToStringified("12:36:12-04:00"))
        },
        test("Period") {
          assert(Period.ZERO.toJson)(equalToStringified("P0D")) &&
          assert(Period.ofDays(1).toJson)(equalToStringified("P1D")) &&
          assert(Period.ofMonths(2).toJson)(equalToStringified("P2M")) &&
          assert(Period.ofWeeks(52).toJson)(equalToStringified("P364D")) &&
          assert(Period.ofYears(10).toJson)(equalToStringified("P10Y"))
        },
        test("Year") {
          val n = Year.now()
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(Year.of(1999).toJson)(equalToStringified("1999")) &&
          assert(Year.of(10000).toJson)(equalToStringified("+10000"))
        },
        test("YearMonth") {
          val n = YearMonth.now()
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(YearMonth.of(1999, 12).toJson)(equalToStringified("1999-12")) &&
          assert(YearMonth.of(1999, 1).toJson)(equalToStringified("1999-01"))
        },
        test("ZonedDateTime") {
          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))) &&
          assert(est.toJson)(equalToStringified("2020-01-01T12:36:00-05:00[America/New_York]")) &&
          assert(utc.toJson)(equalToStringified("2020-01-01T12:36:00Z[Etc/UTC]"))
        },
        test("ZoneId") {
          assert(ZoneId.of("America/New_York").toJson)(equalToStringified("America/New_York")) &&
          assert(ZoneId.of("Etc/UTC").toJson)(equalToStringified("Etc/UTC")) &&
          assert(ZoneId.of("Pacific/Auckland").toJson)(equalToStringified("Pacific/Auckland")) &&
          assert(ZoneId.of("Asia/Shanghai").toJson)(equalToStringified("Asia/Shanghai")) &&
          assert(ZoneId.of("Africa/Cairo").toJson)(equalToStringified("Africa/Cairo"))
        },
        test("ZoneOffset") {
          assert(ZoneOffset.UTC.toJson)(equalToStringified("Z")) &&
          assert(ZoneOffset.ofHours(5).toJson)(equalToStringified("+05:00")) &&
          assert(ZoneOffset.ofHours(-5).toJson)(equalToStringified("-05:00"))
        }
      ),
      suite("Decoder")(
        test("DayOfWeek") {
          assert(stringify("MONDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("TUESDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.TUESDAY))) &&
          assert(stringify("WEDNESDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.WEDNESDAY))) &&
          assert(stringify("THURSDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.THURSDAY))) &&
          assert(stringify("FRIDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.FRIDAY))) &&
          assert(stringify("SATURDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.SATURDAY))) &&
          assert(stringify("SUNDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.SUNDAY))) &&
          assert(stringify("monday").fromJson[DayOfWeek])(
            isRight(equalTo(DayOfWeek.MONDAY))
          ) &&
          assert(stringify("MonDay").fromJson[DayOfWeek])(
            isRight(equalTo(DayOfWeek.MONDAY))
          )
        },
        test("Duration") {
          assert(stringify("PT24H").fromJson[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("PT2562047788015215H30M7.999999999S").fromJson[Duration])(
            isRight(equalTo(Duration.ofSeconds(Long.MaxValue, 999999999L)))
          )
        },
        test("Instant") {
          val n = Instant.now()
          assert(stringify("1970-01-01T00:00:00Z").fromJson[Instant])(isRight(equalTo(Instant.EPOCH))) &&
          assert(stringify(n).fromJson[Instant])(isRight(equalTo(n)))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2020, 1, 1)
          assert(stringify(n).fromJson[LocalDate])(isRight(equalTo(n))) &&
          assert(stringify(p).fromJson[LocalDate])(isRight(equalTo(p)))
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(stringify(n).fromJson[LocalDateTime])(isRight(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36").fromJson[LocalDateTime])(isRight(equalTo(p)))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(stringify(n).fromJson[LocalTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36").fromJson[LocalTime])(isRight(equalTo(p)))
        },
        test("Month") {
          assert(stringify("JANUARY").fromJson[Month])(isRight(equalTo(Month.JANUARY))) &&
          assert(stringify("FEBRUARY").fromJson[Month])(isRight(equalTo(Month.FEBRUARY))) &&
          assert(stringify("MARCH").fromJson[Month])(isRight(equalTo(Month.MARCH))) &&
          assert(stringify("APRIL").fromJson[Month])(isRight(equalTo(Month.APRIL))) &&
          assert(stringify("MAY").fromJson[Month])(isRight(equalTo(Month.MAY))) &&
          assert(stringify("JUNE").fromJson[Month])(isRight(equalTo(Month.JUNE))) &&
          assert(stringify("JULY").fromJson[Month])(isRight(equalTo(Month.JULY))) &&
          assert(stringify("AUGUST").fromJson[Month])(isRight(equalTo(Month.AUGUST))) &&
          assert(stringify("SEPTEMBER").fromJson[Month])(isRight(equalTo(Month.SEPTEMBER))) &&
          assert(stringify("OCTOBER").fromJson[Month])(isRight(equalTo(Month.OCTOBER))) &&
          assert(stringify("NOVEMBER").fromJson[Month])(isRight(equalTo(Month.NOVEMBER))) &&
          assert(stringify("DECEMBER").fromJson[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(stringify("december").fromJson[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(stringify("December").fromJson[Month])(isRight(equalTo(Month.DECEMBER)))
        },
        test("MonthDay") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(stringify(n).fromJson[MonthDay])(isRight(equalTo(n))) &&
          assert(stringify("--01-01").fromJson[MonthDay])(isRight(equalTo(p)))
        },
        test("OffsetDateTime") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(stringify(n).fromJson[OffsetDateTime])(isRight(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36:12Z").fromJson[OffsetDateTime])(isRight(equalTo(p)))
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(stringify(n).fromJson[OffsetTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36:12-04:00").fromJson[OffsetTime])(isRight(equalTo(p)))
        },
        test("Period") {
          assert(stringify("P0D").fromJson[Period])(isRight(equalTo(Period.ZERO))) &&
          assert(stringify("P1D").fromJson[Period])(isRight(equalTo(Period.ofDays(1)))) &&
          assert(stringify("P2M").fromJson[Period])(isRight(equalTo(Period.ofMonths(2)))) &&
          assert(stringify("P364D").fromJson[Period])(isRight(equalTo(Period.ofWeeks(52)))) &&
          assert(stringify("P10Y").fromJson[Period])(isRight(equalTo(Period.ofYears(10))))
        },
        test("Year") {
          val n = Year.now()
          assert(stringify(n).fromJson[Year])(isRight(equalTo(n))) &&
          assert(stringify("1999").fromJson[Year])(isRight(equalTo(Year.of(1999)))) &&
          assert(stringify("+10000").fromJson[Year])(isRight(equalTo(Year.of(10000))))
        },
        test("YearMonth") {
          val n = YearMonth.now()
          assert(stringify(n).fromJson[YearMonth])(isRight(equalTo(n))) &&
          assert(stringify("1999-12").fromJson[YearMonth])(isRight(equalTo(YearMonth.of(1999, 12)))) &&
          assert(stringify("1999-01").fromJson[YearMonth])(isRight(equalTo(YearMonth.of(1999, 1))))
        },
        test("ZonedDateTime") {
          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          assert(stringify(n).fromJson[ZonedDateTime])(isRight(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36-05:00[America/New_York]").fromJson[ZonedDateTime])(
            isRight(equalTo(est))
          ) &&
          assert(stringify("2020-01-01T12:36Z[Etc/UTC]").fromJson[ZonedDateTime])(isRight(equalTo(utc)))
        },
        test("ZoneId") {
          assert(stringify("America/New_York").fromJson[ZoneId])(
            isRight(
              equalTo(
                ZoneId.of("America/New_York")
              )
            )
          ) &&
          assert(stringify("Etc/UTC").fromJson[ZoneId])(isRight(equalTo(ZoneId.of("Etc/UTC")))) &&
          assert(stringify("Pacific/Auckland").fromJson[ZoneId])(
            isRight(
              equalTo(
                ZoneId.of("Pacific/Auckland")
              )
            )
          ) &&
          assert(stringify("Asia/Shanghai").fromJson[ZoneId])(
            isRight(equalTo(ZoneId.of("Asia/Shanghai")))
          ) &&
          assert(stringify("Africa/Cairo").fromJson[ZoneId])(isRight(equalTo(ZoneId.of("Africa/Cairo"))))
        },
        test("ZoneOffset") {
          assert(stringify("Z").fromJson[ZoneOffset])(isRight(equalTo(ZoneOffset.UTC))) &&
          assert(stringify("+05:00").fromJson[ZoneOffset])(isRight(equalTo(ZoneOffset.ofHours(5)))) &&
          assert(stringify("-05:00").fromJson[ZoneOffset])(isRight(equalTo(ZoneOffset.ofHours(-5))))
        }
      ),
      suite("Decoder Sad Path")(
        test("DayOfWeek") {
          assert(stringify("foody").fromJson[DayOfWeek])(
            isLeft(equalTo("(No enum constant java.time.DayOfWeek.FOODY)"))
          )
        },
        test("LocalDate") {
          assert(stringify("01/01/2020").fromJson[LocalDate])(
            isLeft(
              equalTo("(01/01/2020 is not a valid ISO-8601 format, Text '01/01/2020' could not be parsed at index 0)")
            )
          )
        },
        test("LocalDateTime") {
          assert(stringify("01-01-2020T12:36").fromJson[LocalDateTime])(
            isLeft(
              equalTo(
                "(01-01-2020T12:36 is not a valid ISO-8601 format, Text '01-01-2020T12:36' could not be parsed at index 0)"
              )
            )
          )
        },
        test("LocalTime") {
          assert(stringify("12:36.000").fromJson[LocalTime])(
            isLeft(
              equalTo(
                "(12:36.000 is not a valid ISO-8601 format, Text '12:36.000' could not be parsed, unparsed text found at index 5)"
              )
            )
          )
        },
        test("Month") {
          assert(stringify("FebTober").fromJson[Month])(
            isLeft(equalTo("(No enum constant java.time.Month.FEBTOBER)"))
          )
        },
        test("MonthDay") {
          assert(stringify("01-01").fromJson[MonthDay])(
            isLeft(equalTo("(01-01 is not a valid ISO-8601 format, Text '01-01' could not be parsed at index 0)"))
          )
        },
        test("OffsetDateTime") {
          assert(stringify("01-01-2020T12:36:12Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(01-01-2020T12:36:12Z is not a valid ISO-8601 format, Text '01-01-2020T12:36:12Z' could not be parsed at index 0)"
              )
            )
          )
        },
        test("OffsetTime") {
          assert(stringify("12:36:12.000").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(12:36:12.000 is not a valid ISO-8601 format, Text '12:36:12.000' could not be parsed at index 12)"
              )
            )
          )
        },
        test("Period") {
          assert(stringify("0D").fromJson[Period])(
            isLeft(equalTo("(0D is not a valid ISO-8601 format, Text cannot be parsed to a Period)"))
          ) &&
          assert(stringify("1D").fromJson[Period])(
            isLeft(equalTo("(1D is not a valid ISO-8601 format, Text cannot be parsed to a Period)"))
          ) &&
          assert(stringify("2M").fromJson[Period])(
            isLeft(equalTo("(2M is not a valid ISO-8601 format, Text cannot be parsed to a Period)"))
          ) &&
          assert(stringify("364D").fromJson[Period])(
            isLeft(equalTo("(364D is not a valid ISO-8601 format, Text cannot be parsed to a Period)"))
          ) &&
          assert(stringify("10Y").fromJson[Period])(
            isLeft(equalTo("(10Y is not a valid ISO-8601 format, Text cannot be parsed to a Period)"))
          )
        },
        test("Year") {
          assert(stringify("1999-").fromJson[Year])(
            isLeft(
              equalTo(
                "(1999- is not a valid ISO-8601 format, Text '1999-' could not be parsed, unparsed text found at index 4)"
              )
            )
          )
        },
        test("YearMonth") {
          assert(stringify("12-1999").fromJson[YearMonth])(
            isLeft(equalTo("(12-1999 is not a valid ISO-8601 format, Text '12-1999' could not be parsed at index 0)"))
          ) &&
          assert(stringify("01-1999").fromJson[YearMonth])(
            isLeft(equalTo("(01-1999 is not a valid ISO-8601 format, Text '01-1999' could not be parsed at index 0)"))
          ) &&
          assert(stringify("1999-1").fromJson[YearMonth])(
            isLeft(equalTo("(1999-1 is not a valid ISO-8601 format, Text '1999-1' could not be parsed at index 5)"))
          )
        },
        test("ZonedDateTime") {
          assert(stringify("01/01-2020T12:36-05:00[America/New_York]").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(01/01-2020T12:36-05:00[America/New_York] is not a valid ISO-8601 format, Text '01/01-2020T12:36-05:00[America/New_York]' could not be parsed at index 0)"
              )
            )
          ) &&
          assert(stringify("01/01-2020T12:36Z[Etc/UTC]").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(01/01-2020T12:36Z[Etc/UTC] is not a valid ISO-8601 format, Text '01/01-2020T12:36Z[Etc/UTC]' could not be parsed at index 0)"
              )
            )
          ) &&
          assert(stringify("01/01-2020T12:36").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(01/01-2020T12:36 is not a valid ISO-8601 format, Text '01/01-2020T12:36' could not be parsed at index 0)"
              )
            )
          )
        },
        test("ZoneId") {
          assert(stringify("America/New York").fromJson[ZoneId])(
            isLeft(
              equalTo(
                "(America/New York is not a valid ISO-8601 format, Invalid ID for region-based ZoneId, invalid format: America/New York)"
              )
            )
          ) &&
          assert(stringify("Solar_System/Mars").fromJson[ZoneId])(
            isLeft(
              equalTo("(Solar_System/Mars is not a valid ISO-8601 format, Unknown time-zone ID: Solar_System/Mars)")
            )
          )
        },
        test("ZoneOffset") {
          assert(stringify("A").fromJson[ZoneOffset])(
            isLeft(equalTo("(A is not a valid ISO-8601 format, Invalid ID for ZoneOffset, invalid format: A)"))
          ) &&
          assert(stringify("+5.0").fromJson[ZoneOffset])(
            isLeft(equalTo("(+5.0 is not a valid ISO-8601 format, Invalid ID for ZoneOffset, invalid format: +5.0)"))
          ) &&
          assert(stringify("-5.0").fromJson[ZoneOffset])(
            isLeft(equalTo("(-5.0 is not a valid ISO-8601 format, Invalid ID for ZoneOffset, invalid format: -5.0)"))
          )
        }
      )
    ) @@ TestAspect.parallel
}
