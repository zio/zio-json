package zio.json

import zio.test.Assertion._
import zio.test._

import java.time._
import java.time.format.DateTimeFormatter

// zioJsonJVM/testOnly zio.json.JavaTimeSpec
object JavaTimeSpec extends ZIOSpecDefault {

  private def stringify(s: Any): String = s""" "${s.toString}" """

  private def equalToStringified(expected: String) = equalTo(s""""$expected"""")

  val spec: Spec[Environment, Any] =
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
          assert(Duration.ofDays(0).toJson)(equalToStringified("PT0S")) &&
          assert(Duration.ofDays(1).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofHours(24).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofMinutes(1440).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofSeconds(Long.MaxValue, 999999999L).toJson)(
            equalToStringified("PT2562047788015215H30M7.999999999S")
          ) &&
          assert(""""PT-0.5S"""".fromJson[Duration].map(_.toString))(isRight(equalTo("PT-0.5S"))) &&
          assert(""""-PT0.5S"""".fromJson[Duration].map(_.toString))(isRight(equalTo("PT-0.5S")))
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
          assert(stringify("monday").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("MonDay").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY)))
        },
        test("Duration") {
          assert(stringify("PT24H").fromJson[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("-PT24H").fromJson[Duration])(isRight(equalTo(Duration.ofHours(-24)))) &&
          assert(stringify("P1D").fromJson[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("P1DT0H").fromJson[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("PT2562047788015215H30M7.999999999S").fromJson[Duration])(
            isRight(equalTo(Duration.ofSeconds(Long.MaxValue, 999999999L)))
          )
        },
        test("Instant") {
          val n = OffsetDateTime.now()
          val p = n.toInstant
          assert(stringify("1970-01-01T00:00:00Z").fromJson[Instant])(isRight(equalTo(Instant.EPOCH))) &&
          assert(stringify("1970-01-01T00:00:00.Z").fromJson[Instant])(isRight(equalTo(Instant.EPOCH))) &&
          assert(stringify(p).fromJson[Instant])(isRight(equalTo(p))) &&
          assert(stringify(n).fromJson[Instant])(isRight(equalTo(p)))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2000, 2, 29)
          assert(stringify(n).fromJson[LocalDate])(isRight(equalTo(n))) &&
          assert(stringify(p).fromJson[LocalDate])(isRight(equalTo(p)))
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(stringify(n).fromJson[LocalDateTime])(isRight(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36").fromJson[LocalDateTime])(isRight(equalTo(p))) &&
          assert(stringify("2020-01-01T12:36:00.").fromJson[LocalDateTime])(isRight(equalTo(p)))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(stringify(n).fromJson[LocalTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36").fromJson[LocalTime])(isRight(equalTo(p))) &&
          assert(stringify("12:36:00.").fromJson[LocalTime])(isRight(equalTo(p)))
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
          assert(stringify("2020-01-01T12:36:12Z").fromJson[OffsetDateTime])(isRight(equalTo(p))) &&
          assert(stringify("2020-01-01T12:36:12.Z").fromJson[OffsetDateTime])(isRight(equalTo(p)))
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(stringify(n).fromJson[OffsetTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36:12-04:00").fromJson[OffsetTime])(isRight(equalTo(p))) &&
          assert(stringify("12:36:12.-04:00").fromJson[OffsetTime])(isRight(equalTo(p)))
        },
        test("Period") {
          assert(stringify("P0D").fromJson[Period])(isRight(equalTo(Period.ZERO))) &&
          assert(stringify("P1D").fromJson[Period])(isRight(equalTo(Period.ofDays(1)))) &&
          assert(stringify("P-1D").fromJson[Period])(isRight(equalTo(Period.ofDays(-1)))) &&
          assert(stringify("-P1D").fromJson[Period])(isRight(equalTo(Period.ofDays(-1)))) &&
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
          def zdtAssert(actual: String, expected: ZonedDateTime): TestResult =
            assert(stringify(actual).fromJson[ZonedDateTime].map(_.toString))(isRight(equalTo(expected.toString)))

          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          val gmt = ZonedDateTime.of(ld, ZoneId.of("+00:00"))

          zdtAssert(n.toString, n) &&
          zdtAssert("2020-01-01T12:36:00-05:00[America/New_York]", est) &&
          zdtAssert("2020-01-01T12:36:00Z[Etc/UTC]", utc) &&
          zdtAssert("2020-01-01T12:36:00.Z[Etc/UTC]", utc) &&
          zdtAssert("2020-01-01T12:36:00+00:00[+00:00]", gmt) &&
          zdtAssert(
            "2018-02-01T00:00Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 2, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "2018-03-01T00:00:00Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 3, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "2018-04-01T00:00:00.000Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 4, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "+999999999-12-31T23:59:59.999999999+18:00",
            ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MAX)
          ) &&
          zdtAssert(
            "+999999999-12-31T23:59:59.999999999-18:00",
            ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MIN)
          ) &&
          zdtAssert("-999999999-01-01T00:00:00+18:00", ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MAX)) &&
          zdtAssert("-999999999-01-01T00:00:00-18:00", ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MIN)) &&
          zdtAssert(
            "2012-10-28T02:00:00+01:00[Europe/Berlin]",
            OffsetDateTime.parse("2012-10-28T02:00:00+01:00").atZoneSameInstant(ZoneId.of("Europe/Berlin"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+01:00[Europe/Warsaw]",
            ZonedDateTime.parse("2018-03-25T02:30+01:00[Europe/Warsaw]")
          ) &&
          zdtAssert(
            "2018-03-25T02:30+00:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+00:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+02:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+02:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+03:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+03:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+00:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+00:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+01:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+01:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+02:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+02:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+03:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+03:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          )
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
            isLeft(
              equalTo("(No enum constant java.time.DayOfWeek.FOODY)") || // JVM
                equalTo("(Unrecognized day of week name: FOODY)") ||     // Scala.js 2.
                equalTo("(enum case not found: FOODY)")                  // Scala.js 3.
            )
          )
        },
        test("Duration") {
          assert("""""""".fromJson[Duration])(
            isLeft(containsString(" is not a valid ISO-8601 format, illegal duration at index 0"))
          ) &&
          assert(""""X"""".fromJson[Duration])(
            isLeft(containsString("X is not a valid ISO-8601 format, expected 'P' or '-' at index 0"))
          ) &&
          assert(""""P"""".fromJson[Duration])(
            isLeft(containsString("P is not a valid ISO-8601 format, illegal duration at index 1"))
          ) &&
          assert(""""-"""".fromJson[Duration])(
            isLeft(containsString("- is not a valid ISO-8601 format, illegal duration at index 1"))
          ) &&
          assert(""""-X"""".fromJson[Duration])(
            isLeft(containsString("-X is not a valid ISO-8601 format, expected 'P' at index 1"))
          ) &&
          assert(""""PXD"""".fromJson[Duration])(
            isLeft(containsString("PXD is not a valid ISO-8601 format, expected '-' or digit at index 1"))
          ) &&
          assert(""""P-"""".fromJson[Duration])(
            isLeft(containsString("P- is not a valid ISO-8601 format, illegal duration at index 2"))
          ) &&
          assert(""""P-XD"""".fromJson[Duration])(
            isLeft(containsString("P-XD is not a valid ISO-8601 format, expected digit at index 2"))
          ) &&
          assert(""""P1XD"""".fromJson[Duration])(
            isLeft(containsString("P1XD is not a valid ISO-8601 format, expected 'D' or digit at index 2"))
          ) &&
          assert(""""PT"""".fromJson[Duration])(
            isLeft(containsString("PT is not a valid ISO-8601 format, illegal duration at index 2"))
          ) &&
          assert(""""PT0SX"""".fromJson[Duration])(
            isLeft(containsString("PT0SX is not a valid ISO-8601 format, illegal duration at index 4"))
          ) &&
          assert(""""P1DT"""".fromJson[Duration])(
            isLeft(containsString("P1DT is not a valid ISO-8601 format, illegal duration at index 4"))
          ) &&
          assert(""""P106751991167301D"""".fromJson[Duration])(
            isLeft(containsString("P106751991167301D is not a valid ISO-8601 format, illegal duration at index 16"))
          ) &&
          assert(""""P1067519911673000D"""".fromJson[Duration])(
            isLeft(containsString("P1067519911673000D is not a valid ISO-8601 format, illegal duration at index 17"))
          ) &&
          assert(""""P-106751991167301D"""".fromJson[Duration])(
            isLeft(containsString("P-106751991167301D is not a valid ISO-8601 format, illegal duration at index 17"))
          ) &&
          assert(""""P1DX1H"""".fromJson[Duration])(
            isLeft(containsString("P1DX1H is not a valid ISO-8601 format, expected 'T' or '\"' at index 3"))
          ) &&
          assert(""""P1DTXH"""".fromJson[Duration])(
            isLeft(containsString("P1DTXH is not a valid ISO-8601 format, expected '-' or digit at index 4"))
          ) &&
          assert(""""P1DT-XH"""".fromJson[Duration])(
            isLeft(containsString("P1DT-XH is not a valid ISO-8601 format, expected digit at index 5"))
          ) &&
          assert(""""P1DT1XH"""".fromJson[Duration])(
            isLeft(
              containsString(
                "P1DT1XH is not a valid ISO-8601 format, expected 'H' or 'M' or 'S or '.' or digit at index 5"
              )
            )
          ) &&
          assert(""""P1DT1H1XM"""".fromJson[Duration])(
            isLeft(
              containsString("P1DT1H1XM is not a valid ISO-8601 format, expected 'M' or 'S or '.' or digit at index 7")
            )
          ) &&
          assert(""""P0DT2562047788015216H"""".fromJson[Duration])(
            isLeft(containsString("P0DT2562047788015216H is not a valid ISO-8601 format, illegal duration at index 20"))
          ) &&
          assert(""""P0DT-2562047788015216H"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT-2562047788015216H is not a valid ISO-8601 format, illegal duration at index 21")
            )
          ) &&
          assert(""""P0DT153722867280912931M"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT153722867280912931M is not a valid ISO-8601 format, illegal duration at index 22")
            )
          ) &&
          assert(""""P0DT-153722867280912931M"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT-153722867280912931M is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT9223372036854775808S"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT9223372036854775808S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT92233720368547758000S"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT92233720368547758000S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT-9223372036854775809S"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT-9223372036854775809S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P1DT1H1MXS"""".fromJson[Duration])(
            isLeft(
              containsString("P1DT1H1MXS is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 8")
            )
          ) &&
          assert(""""P1DT1H1M-XS"""".fromJson[Duration])(
            isLeft(containsString("P1DT1H1M-XS is not a valid ISO-8601 format, expected digit at index 9"))
          ) &&
          assert(""""P1DT1H1M0XS"""".fromJson[Duration])(
            isLeft(containsString("P1DT1H1M0XS is not a valid ISO-8601 format, expected 'S or '.' or digit at index 9"))
          ) &&
          assert(""""P1DT1H1M0.XS"""".fromJson[Duration])(
            isLeft(containsString("P1DT1H1M0.XS is not a valid ISO-8601 format, expected 'S' or digit at index 10"))
          ) &&
          assert(""""P1DT1H1M0.012345678XS"""".fromJson[Duration])(
            isLeft(containsString("P1DT1H1M0.012345678XS is not a valid ISO-8601 format, expected 'S' at index 19"))
          ) &&
          assert(""""P1DT1H1M0.0123456789S"""".fromJson[Duration])(
            isLeft(containsString("P1DT1H1M0.0123456789S is not a valid ISO-8601 format, expected 'S' at index 19"))
          ) &&
          assert(""""P0DT0H0M9223372036854775808S"""".fromJson[Duration])(
            isLeft(
              containsString(
                "P0DT0H0M9223372036854775808S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P0DT0H0M92233720368547758080S"""".fromJson[Duration])(
            isLeft(
              containsString(
                "P0DT0H0M92233720368547758080S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P0DT0H0M-9223372036854775809S"""".fromJson[Duration])(
            isLeft(
              containsString(
                "P0DT0H0M-9223372036854775809S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P106751991167300DT24H"""".fromJson[Duration])(
            isLeft(containsString("P106751991167300DT24H is not a valid ISO-8601 format, illegal duration at index 20"))
          ) &&
          assert(""""P0DT2562047788015215H60M"""".fromJson[Duration])(
            isLeft(
              containsString("P0DT2562047788015215H60M is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT0H153722867280912930M60S"""".fromJson[Duration])(
            isLeft(
              containsString(
                "P0DT0H153722867280912930M60S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          )
        },
        test("Instant") {
          assert(stringify("").fromJson[Instant])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal instant at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJson[Instant])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal instant at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal instant at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal instant at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal instant at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal instant at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal instant at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, illegal instant at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal instant at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, illegal instant at index 23)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal instant at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, illegal instant at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal instant at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJson[Instant])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000001-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+1000000001-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("+3333333333-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(+3333333333-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-1000000001-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(-1000000001-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJson[Instant])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal instant at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJson[Instant])(
            isLeft(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalDate") {
          assert(stringify("").fromJson[LocalDate])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal local date at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal local date at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal local date at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal local date at index 8)")
            )
          ) &&
          assert(stringify("2020-01-012").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01-012 is not a valid ISO-8601 format, illegal local date at index 10)")
            )
          ) &&
          assert(stringify("X020-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(X020-01-01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2X20-01-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(20X0-01-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(202X-01-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020X01-01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-X1-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-0X-01 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01X01 is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01-X1 is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0X").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01-0X is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("+X0000-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+X0000-01-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+1X000-01-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+10X00-01-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+100X0-01-01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+1000X-01-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+10000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+100000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+1000000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(+1000000000-01-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(-1000000000-01-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(-0000-01-01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJson[LocalDate])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal local date at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-00-01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-13-01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01-00 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-01-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-02-30 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-03-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-04-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-05-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-06-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-07-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-08-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-09-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-10-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-11-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32").fromJson[LocalDate])(
            isLeft(
              equalTo("(2020-12-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalDateTime") {
          assert(stringify("").fromJson[LocalDateTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal local date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal local date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal local date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal local date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal local date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal local date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(X020-01-01T01:01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2X20-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(20X0-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(202X-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020X01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-X1-01T01:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-0X-01T01:01 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01X01T01:01 is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-X1T01:01 is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-0XT01:01 is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01X01:01 is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' at index 16)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal local date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1 is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0X").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0X is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:60 is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' at index 19)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01.X is not a valid ISO-8601 format, illegal local date time at index 20)")
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+X0000-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+1X000-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+10X00-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+100X0-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+1000X-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+10000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+100000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+1000000000-01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(-1000000000-01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(-0000-01-01T01:01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal local date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-00-01T01:01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-13-01T01:01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-00T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-02-30T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-03-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-04-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-05-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-06-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-07-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-08-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-09-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-10-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-11-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01").fromJson[LocalDateTime])(
            isLeft(
              equalTo("(2020-12-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalTime") {
          assert(stringify("").fromJson[LocalTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal local time at index 0)")
            )
          ) &&
          assert(stringify("0").fromJson[LocalTime])(
            isLeft(
              equalTo("(0 is not a valid ISO-8601 format, illegal local time at index 0)")
            )
          ) &&
          assert(stringify("01:0").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:0 is not a valid ISO-8601 format, illegal local time at index 3)")
            )
          ) &&
          assert(stringify("X1:01").fromJson[LocalTime])(
            isLeft(
              equalTo("(X1:01 is not a valid ISO-8601 format, expected digit at index 0)")
            )
          ) &&
          assert(stringify("0X:01").fromJson[LocalTime])(
            isLeft(
              equalTo("(0X:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("24:01").fromJson[LocalTime])(
            isLeft(
              equalTo("(24:01 is not a valid ISO-8601 format, illegal hour at index 1)")
            )
          ) &&
          assert(stringify("01X01").fromJson[LocalTime])(
            isLeft(
              equalTo("(01X01 is not a valid ISO-8601 format, expected ':' at index 2)")
            )
          ) &&
          assert(stringify("01:X1").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:X1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("01:0X").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:0X is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("01:60").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:60 is not a valid ISO-8601 format, illegal minute at index 4)")
            )
          ) &&
          assert(stringify("01:01X").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01X is not a valid ISO-8601 format, expected ':' at index 5)")
            )
          ) &&
          assert(stringify("01:01:0").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01:0 is not a valid ISO-8601 format, illegal local time at index 6)")
            )
          ) &&
          assert(stringify("01:01:X1").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01:X1 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("01:01:0X").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01:0X is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("01:01:60").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01:60 is not a valid ISO-8601 format, illegal second at index 7)")
            )
          ) &&
          assert(stringify("01:01:012").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01:012 is not a valid ISO-8601 format, expected '.' at index 8)")
            )
          ) &&
          assert(stringify("01:01:01.X").fromJson[LocalTime])(
            isLeft(
              equalTo("(01:01:01.X is not a valid ISO-8601 format, illegal local time at index 9)")
            )
          )
        },
        test("Month") {
          assert(stringify("FebTober").fromJson[Month])(
            isLeft(
              equalTo("(No enum constant java.time.Month.FEBTOBER)") || // JVM
                equalTo("(Unrecognized month name: FEBTOBER)") ||       // Scala.js 2.
                equalTo("(enum case not found: FEBTOBER)")              // Scala.js 3.
            )
          )
        },
        test("MonthDay") {
          assert(stringify("").fromJson[MonthDay])(
            isLeft(equalTo("( is not a valid ISO-8601 format, illegal month day at index 0)"))
          ) &&
          assert(stringify("X-01-01").fromJson[MonthDay])(
            isLeft(equalTo("(X-01-01 is not a valid ISO-8601 format, expected '-' at index 0)"))
          ) &&
          assert(stringify("-X01-01").fromJson[MonthDay])(
            isLeft(equalTo("(-X01-01 is not a valid ISO-8601 format, expected '-' at index 1)"))
          ) &&
          assert(stringify("--X1-01").fromJson[MonthDay])(
            isLeft(equalTo("(--X1-01 is not a valid ISO-8601 format, expected digit at index 2)"))
          ) &&
          assert(stringify("--0X-01").fromJson[MonthDay])(
            isLeft(equalTo("(--0X-01 is not a valid ISO-8601 format, expected digit at index 3)"))
          ) &&
          assert(stringify("--00-01").fromJson[MonthDay])(
            isLeft(equalTo("(--00-01 is not a valid ISO-8601 format, illegal month at index 3)"))
          ) &&
          assert(stringify("--13-01").fromJson[MonthDay])(
            isLeft(equalTo("(--13-01 is not a valid ISO-8601 format, illegal month at index 3)"))
          ) &&
          assert(stringify("--01X01").fromJson[MonthDay])(
            isLeft(equalTo("(--01X01 is not a valid ISO-8601 format, expected '-' at index 4)"))
          ) &&
          assert(stringify("--01-X1").fromJson[MonthDay])(
            isLeft(equalTo("(--01-X1 is not a valid ISO-8601 format, expected digit at index 5)"))
          ) &&
          assert(stringify("--01-0X").fromJson[MonthDay])(
            isLeft(equalTo("(--01-0X is not a valid ISO-8601 format, expected digit at index 6)"))
          ) &&
          assert(stringify("--01-00").fromJson[MonthDay])(
            isLeft(equalTo("(--01-00 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--01-32").fromJson[MonthDay])(
            isLeft(equalTo("(--01-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--02-30").fromJson[MonthDay])(
            isLeft(equalTo("(--02-30 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--03-32").fromJson[MonthDay])(
            isLeft(equalTo("(--03-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--04-31").fromJson[MonthDay])(
            isLeft(equalTo("(--04-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--05-32").fromJson[MonthDay])(
            isLeft(equalTo("(--05-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--06-31").fromJson[MonthDay])(
            isLeft(equalTo("(--06-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--07-32").fromJson[MonthDay])(
            isLeft(equalTo("(--07-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--08-32").fromJson[MonthDay])(
            isLeft(equalTo("(--08-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--09-31").fromJson[MonthDay])(
            isLeft(equalTo("(--09-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--10-32").fromJson[MonthDay])(
            isLeft(equalTo("(--10-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--11-31").fromJson[MonthDay])(
            isLeft(equalTo("(--11-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--12-32").fromJson[MonthDay])(
            isLeft(equalTo("(--12-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          )
        },
        test("OffsetDateTime") {
          assert(stringify("").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal offset date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal offset date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal offset date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal offset date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal offset date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal offset date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal offset date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, illegal offset date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal offset date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, illegal offset date time at index 23)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal offset date time at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, illegal offset date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal offset date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJson[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(-1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal offset date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("OffsetTime") {
          assert(stringify("").fromJson[OffsetTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal offset time at index 0)")
            )
          ) &&
          assert(stringify("0").fromJson[OffsetTime])(
            isLeft(
              equalTo("(0 is not a valid ISO-8601 format, illegal offset time at index 0)")
            )
          ) &&
          assert(stringify("01:0").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:0 is not a valid ISO-8601 format, illegal offset time at index 3)")
            )
          ) &&
          assert(stringify("X1:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(X1:01 is not a valid ISO-8601 format, expected digit at index 0)")
            )
          ) &&
          assert(stringify("0X:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(0X:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("24:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(24:01 is not a valid ISO-8601 format, illegal hour at index 1)")
            )
          ) &&
          assert(stringify("01X01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01X01 is not a valid ISO-8601 format, expected ':' at index 2)")
            )
          ) &&
          assert(stringify("01:X1").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:X1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("01:0X").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:0X is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("01:60").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:60 is not a valid ISO-8601 format, illegal minute at index 4)")
            )
          ) &&
          assert(stringify("01:01").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 5)"
              )
            )
          ) &&
          assert(stringify("01:01X").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 5)"
              )
            )
          ) &&
          assert(stringify("01:01:0").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:0 is not a valid ISO-8601 format, illegal offset time at index 6)")
            )
          ) &&
          assert(stringify("01:01:X1Z").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:X1Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("01:01:0XZ").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:0XZ is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("01:01:60Z").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:60Z is not a valid ISO-8601 format, illegal second at index 7)")
            )
          ) &&
          assert(stringify("01:01:01").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 8)"
              )
            )
          ) &&
          assert(stringify("01:01:012").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 8)"
              )
            )
          ) &&
          assert(stringify("01:01:01.").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 9)"
              )
            )
          ) &&
          assert(stringify("01:01:01.X").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 9)"
              )
            )
          ) &&
          assert(stringify("01:01:01.123456789X").fromJson[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 18)"
              )
            )
          ) &&
          assert(stringify("01:01:01ZX").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01ZX is not a valid ISO-8601 format, illegal offset time at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+X1:01:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+0").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+0 is not a valid ISO-8601 format, illegal offset time at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+0X:01:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 10)")
            )
          ) &&
          assert(stringify("01:01:01+19:01:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 10)")
            )
          ) &&
          assert(stringify("01:01:01+01X01:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01X01:01 is not a valid ISO-8601 format, illegal offset time at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:0").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:0 is not a valid ISO-8601 format, illegal offset time at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:X1:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:0X:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 13)")
            )
          ) &&
          assert(stringify("01:01:01+01:60:01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 13)")
            )
          ) &&
          assert(stringify("01:01:01+01:01X01").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01X01 is not a valid ISO-8601 format, illegal offset time at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:0").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:0 is not a valid ISO-8601 format, illegal offset time at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:X1").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:0X").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 16)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:60").fromJson[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 16)")
            )
          )
        },
        test("Period") {
          assert(stringify("").fromJson[Period])(
            isLeft(equalTo("( is not a valid ISO-8601 format, illegal period at index 0)"))
          ) &&
          assert(stringify("X").fromJson[Period])(
            isLeft(equalTo("(X is not a valid ISO-8601 format, expected 'P' or '-' at index 0)"))
          ) &&
          assert(stringify("P").fromJson[Period])(
            isLeft(equalTo("(P is not a valid ISO-8601 format, illegal period at index 1)"))
          ) &&
          assert(stringify("-").fromJson[Period])(
            isLeft(equalTo("(- is not a valid ISO-8601 format, illegal period at index 1)"))
          ) &&
          assert(stringify("PXY").fromJson[Period])(
            isLeft(equalTo("(PXY is not a valid ISO-8601 format, expected '-' or digit at index 1)"))
          ) &&
          assert(stringify("P-").fromJson[Period])(
            isLeft(equalTo("(P- is not a valid ISO-8601 format, illegal period at index 2)"))
          ) &&
          assert(stringify("P-XY").fromJson[Period])(
            isLeft(equalTo("(P-XY is not a valid ISO-8601 format, expected digit at index 2)"))
          ) &&
          assert(stringify("P1XY").fromJson[Period])(
            isLeft(
              equalTo("(P1XY is not a valid ISO-8601 format, expected 'Y' or 'M' or 'W' or 'D' or digit at index 2)")
            )
          ) &&
          assert(stringify("P2147483648Y").fromJson[Period])(
            isLeft(equalTo("(P2147483648Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470Y").fromJson[Period])(
            isLeft(equalTo("(P21474836470Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649Y").fromJson[Period])(
            isLeft(equalTo("(P-2147483649Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648M").fromJson[Period])(
            isLeft(equalTo("(P2147483648M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470M").fromJson[Period])(
            isLeft(equalTo("(P21474836470M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649M").fromJson[Period])(
            isLeft(equalTo("(P-2147483649M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648W").fromJson[Period])(
            isLeft(equalTo("(P2147483648W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470W").fromJson[Period])(
            isLeft(equalTo("(P21474836470W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649W").fromJson[Period])(
            isLeft(equalTo("(P-2147483649W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648D").fromJson[Period])(
            isLeft(equalTo("(P2147483648D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470D").fromJson[Period])(
            isLeft(equalTo("(P21474836470D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649D").fromJson[Period])(
            isLeft(equalTo("(P-2147483649D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P1YXM").fromJson[Period])(
            isLeft(equalTo("(P1YXM is not a valid ISO-8601 format, expected '-' or digit at index 3)"))
          ) &&
          assert(stringify("P1Y-XM").fromJson[Period])(
            isLeft(equalTo("(P1Y-XM is not a valid ISO-8601 format, expected digit at index 4)"))
          ) &&
          assert(stringify("P1Y1XM").fromJson[Period])(
            isLeft(equalTo("(P1Y1XM is not a valid ISO-8601 format, expected 'M' or 'W' or 'D' or digit at index 4)"))
          ) &&
          assert(stringify("P1Y2147483648M").fromJson[Period])(
            isLeft(equalTo("(P1Y2147483648M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470M").fromJson[Period])(
            isLeft(equalTo("(P1Y21474836470M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649M").fromJson[Period])(
            isLeft(equalTo("(P1Y-2147483649M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y2147483648W").fromJson[Period])(
            isLeft(equalTo("(P1Y2147483648W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470W").fromJson[Period])(
            isLeft(equalTo("(P1Y21474836470W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649W").fromJson[Period])(
            isLeft(equalTo("(P1Y-2147483649W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y2147483648D").fromJson[Period])(
            isLeft(equalTo("(P1Y2147483648D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470D").fromJson[Period])(
            isLeft(equalTo("(P1Y21474836470D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649D").fromJson[Period])(
            isLeft(equalTo("(P1Y-2147483649D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y1MXW").fromJson[Period])(
            isLeft(equalTo("(P1Y1MXW is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 5)"))
          ) &&
          assert(stringify("P1Y1M-XW").fromJson[Period])(
            isLeft(equalTo("(P1Y1M-XW is not a valid ISO-8601 format, expected digit at index 6)"))
          ) &&
          assert(stringify("P1Y1M1XW").fromJson[Period])(
            isLeft(equalTo("(P1Y1M1XW is not a valid ISO-8601 format, expected 'W' or 'D' or digit at index 6)"))
          ) &&
          assert(stringify("P1Y1M306783379W").fromJson[Period])(
            isLeft(equalTo("(P1Y1M306783379W is not a valid ISO-8601 format, illegal period at index 14)"))
          ) &&
          assert(stringify("P1Y1M3067833790W").fromJson[Period])(
            isLeft(equalTo("(P1Y1M3067833790W is not a valid ISO-8601 format, illegal period at index 14)"))
          ) &&
          assert(stringify("P1Y1M-306783379W").fromJson[Period])(
            isLeft(equalTo("(P1Y1M-306783379W is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M2147483648D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M2147483648D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M21474836470D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M21474836470D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M-2147483649D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M-2147483649D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M1WXD").fromJson[Period])(
            isLeft(equalTo("(P1Y1M1WXD is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 7)"))
          ) &&
          assert(stringify("P1Y1M1W-XD").fromJson[Period])(
            isLeft(equalTo("(P1Y1M1W-XD is not a valid ISO-8601 format, expected digit at index 8)"))
          ) &&
          assert(stringify("P1Y1M1W1XD").fromJson[Period])(
            isLeft(equalTo("(P1Y1M1W1XD is not a valid ISO-8601 format, expected 'D' or digit at index 8)"))
          ) &&
          assert(stringify("P1Y1M306783378W8D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M306783378W8D is not a valid ISO-8601 format, illegal period at index 16)"))
          ) &&
          assert(stringify("P1Y1M-306783378W-8D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M-306783378W-8D is not a valid ISO-8601 format, illegal period at index 18)"))
          ) &&
          assert(stringify("P1Y1M1W2147483647D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M1W2147483647D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M-1W-2147483648D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M-1W-2147483648D is not a valid ISO-8601 format, illegal period at index 19)"))
          ) &&
          assert(stringify("P1Y1M0W2147483648D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M0W2147483648D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M0W21474836470D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M0W21474836470D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M0W-2147483649D").fromJson[Period])(
            isLeft(equalTo("(P1Y1M0W-2147483649D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M1W1DX").fromJson[Period])(
            isLeft(equalTo("(P1Y1M1W1DX is not a valid ISO-8601 format, illegal period at index 9)"))
          )
        },
        test("Year") {
          assert(stringify("").fromJson[Year])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("2").fromJson[Year])(
            isLeft(
              equalTo("(2 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("22").fromJson[Year])(
            isLeft(
              equalTo("(22 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("222").fromJson[Year])(
            isLeft(
              equalTo("(222 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("X020").fromJson[Year])(
            isLeft(
              equalTo("(X020 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20").fromJson[Year])(
            isLeft(
              equalTo("(2X20 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0").fromJson[Year])(
            isLeft(
              equalTo("(20X0 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X").fromJson[Year])(
            isLeft(
              equalTo("(202X is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+X0000").fromJson[Year])(
            isLeft(
              equalTo("(+X0000 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000").fromJson[Year])(
            isLeft(
              equalTo("(+1X000 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00").fromJson[Year])(
            isLeft(
              equalTo("(+10X00 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0").fromJson[Year])(
            isLeft(
              equalTo("(+100X0 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X").fromJson[Year])(
            isLeft(
              equalTo("(+1000X is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X").fromJson[Year])(
            isLeft(
              equalTo("(+10000X is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X").fromJson[Year])(
            isLeft(
              equalTo("(+100000X is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X").fromJson[Year])(
            isLeft(
              equalTo("(+1000000X is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000").fromJson[Year])(
            isLeft(
              equalTo("(+1000000000 is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-1000000000").fromJson[Year])(
            isLeft(
              equalTo("(-1000000000 is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-0000").fromJson[Year])(
            isLeft(
              equalTo("(-0000 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("10000").fromJson[Year])(
            isLeft(
              equalTo("(10000 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          )
        },
        test("YearMonth") {
          assert(stringify("").fromJson[YearMonth])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal year month at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal year month at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal year month at index 5)")
            )
          ) &&
          assert(stringify("2020-012").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020-012 is not a valid ISO-8601 format, illegal year month at index 7)")
            )
          ) &&
          assert(stringify("X020-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(X020-01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(2X20-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(20X0-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(202X-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020X01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020-X1 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020-0X is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("+X0000-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+X0000-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+1X000-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+10X00-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+100X0-01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+1000X-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+10000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+100000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+1000000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(+1000000000-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(-1000000000-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01").fromJson[YearMonth])(
            isLeft(
              equalTo("(-0000-01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJson[YearMonth])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal year month at index 6)")
            )
          ) &&
          assert(stringify("2020-00").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020-00 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13").fromJson[YearMonth])(
            isLeft(
              equalTo("(2020-13 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          )
        },
        test("ZonedDateTime") {
          assert(stringify("").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal zoned date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal zoned date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal zoned date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal zoned date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal zoned date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal zoned date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal zoned date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, expected '[' at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal zoned date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, expected '[' at index 22)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal zoned date time at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, expected '[' at index 25)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal zoned date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:01X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:01X is not a valid ISO-8601 format, expected '[' at index 28)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJson[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(-1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal zoned date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01Z[ is not a valid ISO-8601 format, illegal zoned date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[X]").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01Z[X] is not a valid ISO-8601 format, illegal zoned date time at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[GMT]X").fromJson[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01Z[GMT]X is not a valid ISO-8601 format, illegal zoned date time at index 22)")
            )
          )
        },
        test("ZoneId") {
          assert(stringify("America/New York").fromJson[ZoneId])(
            isLeft(equalTo("(America/New York is not a valid ISO-8601 format, illegal zone id at index 0)"))
          ) &&
          assert(stringify("Solar_System/Mars").fromJson[ZoneId])(
            isLeft(equalTo("(Solar_System/Mars is not a valid ISO-8601 format, illegal zone id at index 0)"))
          )
        },
        test("ZoneOffset") {
          assert(stringify("").fromJson[ZoneOffset])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal zone offset at index 0)")
            )
          ) &&
          assert(stringify("X").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 0)")
            )
          ) &&
          assert(stringify("+X1:01:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+X1:01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+0").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+0 is not a valid ISO-8601 format, illegal zone offset at index 1)")
            )
          ) &&
          assert(stringify("+0X:01:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+0X:01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+19:01:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo(
                "(+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 2)"
              )
            )
          ) &&
          assert(stringify("+01X01:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01X01:01 is not a valid ISO-8601 format, illegal zone offset at index 4)"
              )
            )
          ) &&
          assert(stringify("+01:0").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+01:0 is not a valid ISO-8601 format, illegal zone offset at index 4)")
            )
          ) &&
          assert(stringify("+01:X1:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+01:X1:01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+01:0X:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+01:0X:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+01:60:01").fromJson[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 5)"
              )
            )
          ) &&
          assert(stringify("+01:01X01").fromJson[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:01X01 is not a valid ISO-8601 format, illegal zone offset at index 7)"
              )
            )
          ) &&
          assert(stringify("+01:01:0").fromJson[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:01:0 is not a valid ISO-8601 format, illegal zone offset at index 7)"
              )
            )
          ) &&
          assert(stringify("+01:01:X1").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+01:01:X1 is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("+01:01:0X").fromJson[ZoneOffset])(
            isLeft(
              equalTo("(+01:01:0X is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("+01:01:60").fromJson[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 8)"
              )
            )
          )
        }
      )
    )
}
