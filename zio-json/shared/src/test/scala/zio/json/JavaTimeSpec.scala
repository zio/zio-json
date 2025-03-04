package zio.json

import zio.json.ast._
import zio.test.Assertion._
import zio.test._
import java.time._
import java.time.format.DateTimeFormatter

// zioJsonJVM/testOnly zio.json.JavaTimeSpec
object JavaTimeSpec extends ZIOSpecDefault {

  private def stringify(s: Any): String = s""" "${s.toString}" """

  private def equalToStringified(expected: String) = equalTo(s""""$expected"""")

  private def equalToJsonStr(expected: String): Assertion[Either[String, Json]] = isRight(equalTo(Json.Str(expected)))

  val spec: Spec[Environment, Any] =
    suite("java.time")(
      suite("Encoder")(
        test("DayOfWeek toJson") {
          assert(DayOfWeek.MONDAY.toJson)(equalToStringified("MONDAY")) &&
          assert(DayOfWeek.TUESDAY.toJson)(equalToStringified("TUESDAY")) &&
          assert(DayOfWeek.WEDNESDAY.toJson)(equalToStringified("WEDNESDAY")) &&
          assert(DayOfWeek.THURSDAY.toJson)(equalToStringified("THURSDAY")) &&
          assert(DayOfWeek.FRIDAY.toJson)(equalToStringified("FRIDAY")) &&
          assert(DayOfWeek.SATURDAY.toJson)(equalToStringified("SATURDAY")) &&
          assert(DayOfWeek.SUNDAY.toJson)(equalToStringified("SUNDAY"))
        },
        test("DayOfWeek toJsonAST") {
          assert(DayOfWeek.MONDAY.toJsonAST)(equalToJsonStr("MONDAY")) &&
          assert(DayOfWeek.TUESDAY.toJsonAST)(equalToJsonStr("TUESDAY")) &&
          assert(DayOfWeek.WEDNESDAY.toJsonAST)(equalToJsonStr("WEDNESDAY")) &&
          assert(DayOfWeek.THURSDAY.toJsonAST)(equalToJsonStr("THURSDAY")) &&
          assert(DayOfWeek.FRIDAY.toJsonAST)(equalToJsonStr("FRIDAY")) &&
          assert(DayOfWeek.SATURDAY.toJsonAST)(equalToJsonStr("SATURDAY")) &&
          assert(DayOfWeek.SUNDAY.toJsonAST)(equalToJsonStr("SUNDAY"))
        },
        test("Duration toJson") {
          assert(Duration.ofDays(0).toJson)(equalToStringified("PT0S")) &&
          assert(Duration.ofDays(1).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofHours(24).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofMinutes(1440).toJson)(equalToStringified("PT24H")) &&
          assert(Duration.ofSeconds(Long.MaxValue, 999999999L).toJson)(
            equalToStringified("PT2562047788015215H30M7.999999999S")
          )
        },
        test("Duration toJsonAST") {
          assert(Duration.ofDays(0).toJsonAST)(equalToJsonStr("PT0S")) &&
          assert(Duration.ofDays(1).toJsonAST)(equalToJsonStr("PT24H")) &&
          assert(Duration.ofHours(24).toJsonAST)(equalToJsonStr("PT24H")) &&
          assert(Duration.ofMinutes(1440).toJsonAST)(equalToJsonStr("PT24H")) &&
          assert(Duration.ofSeconds(Long.MaxValue, 999999999L).toJsonAST)(
            equalToJsonStr("PT2562047788015215H30M7.999999999S")
          )
        },
        test("Instant toJson") {
          val n = Instant.now()
          assert(Instant.EPOCH.toJson)(equalToStringified("1970-01-01T00:00:00Z")) &&
          assert(n.toJson)(equalToStringified(n.toString))
        },
        test("Instant toJsonAST") {
          val n = Instant.now()
          assert(Instant.EPOCH.toJsonAST)(equalToJsonStr("1970-01-01T00:00:00Z")) &&
          assert(n.toJsonAST)(equalToJsonStr(n.toString))
        },
        test("LocalDate toJson") {
          val n = LocalDate.now()
          val p = LocalDate.of(2020, 1, 1)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_DATE))) &&
          assert(p.toJson)(equalToStringified("2020-01-01"))
        },
        test("LocalDate toJsonAST") {
          val n = LocalDate.now()
          val p = LocalDate.of(2020, 1, 1)
          assert(n.toJsonAST)(equalToJsonStr(n.format(DateTimeFormatter.ISO_LOCAL_DATE))) &&
          assert(p.toJsonAST)(equalToJsonStr("2020-01-01"))
        },
        test("LocalDateTime toJson") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))) &&
          assert(p.toJson)(equalToStringified("2020-01-01T12:36:00"))
        },
        test("LocalDateTime toJsonAST") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(n.toJsonAST)(equalToJsonStr(n.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))) &&
          assert(p.toJsonAST)(equalToJsonStr("2020-01-01T12:36:00"))
        },
        test("LocalTime toJson") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_LOCAL_TIME))) &&
          assert(p.toJson)(equalToStringified("12:36:00"))
        },
        test("LocalTime toJsonAST") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(n.toJsonAST)(equalToJsonStr(n.format(DateTimeFormatter.ISO_LOCAL_TIME))) &&
          assert(p.toJsonAST)(equalToJsonStr("12:36:00"))
        },
        test("Month toJson") {
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
        test("Month toJsonAST") {
          assert(Month.JANUARY.toJsonAST)(equalToJsonStr("JANUARY")) &&
          assert(Month.FEBRUARY.toJsonAST)(equalToJsonStr("FEBRUARY")) &&
          assert(Month.MARCH.toJsonAST)(equalToJsonStr("MARCH")) &&
          assert(Month.APRIL.toJsonAST)(equalToJsonStr("APRIL")) &&
          assert(Month.MAY.toJsonAST)(equalToJsonStr("MAY")) &&
          assert(Month.JUNE.toJsonAST)(equalToJsonStr("JUNE")) &&
          assert(Month.JULY.toJsonAST)(equalToJsonStr("JULY")) &&
          assert(Month.AUGUST.toJsonAST)(equalToJsonStr("AUGUST")) &&
          assert(Month.SEPTEMBER.toJsonAST)(equalToJsonStr("SEPTEMBER")) &&
          assert(Month.OCTOBER.toJsonAST)(equalToJsonStr("OCTOBER")) &&
          assert(Month.NOVEMBER.toJsonAST)(equalToJsonStr("NOVEMBER")) &&
          assert(Month.DECEMBER.toJsonAST)(equalToJsonStr("DECEMBER"))
        },
        test("MonthDay toJson") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(p.toJson)(equalToStringified("--01-01"))
        },
        test("MonthDay toJsonAST") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(n.toJsonAST)(equalToJsonStr(n.toString)) &&
          assert(p.toJsonAST)(equalToJsonStr("--01-01"))
        },
        test("OffsetDateTime toJson") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))) &&
          assert(p.toJson)(equalToStringified("2020-01-01T12:36:12Z"))
        },
        test("OffsetDateTime toJsonAST") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(n.toJsonAST)(equalToJsonStr(n.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))) &&
          assert(p.toJsonAST)(equalToJsonStr("2020-01-01T12:36:12Z"))
        },
        test("OffsetTime toJson") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_OFFSET_TIME))) &&
          assert(p.toJson)(equalToStringified("12:36:12-04:00"))
        },
        test("OffsetTime toJsonAST") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(n.toJsonAST)(equalToJsonStr(n.format(DateTimeFormatter.ISO_OFFSET_TIME))) &&
          assert(p.toJsonAST)(equalToJsonStr("12:36:12-04:00"))
        },
        test("Period toJson") {
          assert(Period.ZERO.toJson)(equalToStringified("P0D")) &&
          assert(Period.ofDays(1).toJson)(equalToStringified("P1D")) &&
          assert(Period.ofMonths(2).toJson)(equalToStringified("P2M")) &&
          assert(Period.ofWeeks(52).toJson)(equalToStringified("P364D")) &&
          assert(Period.ofYears(10).toJson)(equalToStringified("P10Y"))
        },
        test("Period toJsonAST") {
          assert(Period.ZERO.toJsonAST)(equalToJsonStr("P0D")) &&
          assert(Period.ofDays(1).toJsonAST)(equalToJsonStr("P1D")) &&
          assert(Period.ofMonths(2).toJsonAST)(equalToJsonStr("P2M")) &&
          assert(Period.ofWeeks(52).toJsonAST)(equalToJsonStr("P364D")) &&
          assert(Period.ofYears(10).toJsonAST)(equalToJsonStr("P10Y"))
        },
        test("Year toJson") {
          val n = Year.now()
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(Year.of(1999).toJson)(equalToStringified("1999")) &&
          assert(Year.of(10000).toJson)(equalToStringified("+10000"))
        },
        test("Year toJsonAST") {
          val n = Year.now()
          assert(n.toJsonAST)(equalToJsonStr(n.toString)) &&
          assert(Year.of(1999).toJsonAST)(equalToJsonStr("1999")) &&
          assert(Year.of(10000).toJsonAST)(equalToJsonStr("+10000"))
        },
        test("YearMonth toJson") {
          val n = YearMonth.now()
          assert(n.toJson)(equalToStringified(n.toString)) &&
          assert(YearMonth.of(1999, 12).toJson)(equalToStringified("1999-12")) &&
          assert(YearMonth.of(1999, 1).toJson)(equalToStringified("1999-01"))
        },
        test("YearMonth toJsonAST") {
          val n = YearMonth.now()
          assert(n.toJsonAST)(equalToJsonStr(n.toString)) &&
          assert(YearMonth.of(1999, 12).toJsonAST)(equalToJsonStr("1999-12")) &&
          assert(YearMonth.of(1999, 1).toJsonAST)(equalToJsonStr("1999-01"))
        },
        test("ZonedDateTime toJson") {
          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          assert(n.toJson)(equalToStringified(n.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))) &&
          assert(est.toJson)(equalToStringified("2020-01-01T12:36:00-05:00[America/New_York]")) &&
          assert(utc.toJson)(equalToStringified("2020-01-01T12:36:00Z[Etc/UTC]"))
        },
        test("ZonedDateTime toJsonAST") {
          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          assert(n.toJsonAST)(equalToJsonStr(n.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))) &&
          assert(est.toJsonAST)(equalToJsonStr("2020-01-01T12:36:00-05:00[America/New_York]")) &&
          assert(utc.toJsonAST)(equalToJsonStr("2020-01-01T12:36:00Z[Etc/UTC]"))
        },
        test("ZoneId toJson") {
          assert(ZoneId.of("America/New_York").toJson)(equalToStringified("America/New_York")) &&
          assert(ZoneId.of("Etc/UTC").toJson)(equalToStringified("Etc/UTC")) &&
          assert(ZoneId.of("Pacific/Auckland").toJson)(equalToStringified("Pacific/Auckland")) &&
          assert(ZoneId.of("Asia/Shanghai").toJson)(equalToStringified("Asia/Shanghai")) &&
          assert(ZoneId.of("Africa/Cairo").toJson)(equalToStringified("Africa/Cairo"))
        },
        test("ZoneId toJsonAST") {
          assert(ZoneId.of("America/New_York").toJsonAST)(equalToJsonStr("America/New_York")) &&
          assert(ZoneId.of("Etc/UTC").toJsonAST)(equalToJsonStr("Etc/UTC")) &&
          assert(ZoneId.of("Pacific/Auckland").toJsonAST)(equalToJsonStr("Pacific/Auckland")) &&
          assert(ZoneId.of("Asia/Shanghai").toJsonAST)(equalToJsonStr("Asia/Shanghai")) &&
          assert(ZoneId.of("Africa/Cairo").toJsonAST)(equalToJsonStr("Africa/Cairo"))
        },
        test("ZoneOffset toJson") {
          assert(ZoneOffset.UTC.toJson)(equalToStringified("Z")) &&
          assert(ZoneOffset.ofHours(5).toJson)(equalToStringified("+05:00")) &&
          assert(ZoneOffset.ofHours(-5).toJson)(equalToStringified("-05:00"))
        },
        test("ZoneOffset toJsonAST") {
          assert(ZoneOffset.UTC.toJsonAST)(equalToJsonStr("Z")) &&
          assert(ZoneOffset.ofHours(5).toJsonAST)(equalToJsonStr("+05:00")) &&
          assert(ZoneOffset.ofHours(-5).toJsonAST)(equalToJsonStr("-05:00"))
        }
      ),
      suite("Decoder")(
        test("DayOfWeek fromJson") {
          assert(stringify("MONDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("TUESDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.TUESDAY))) &&
          assert(stringify("WEDNESDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.WEDNESDAY))) &&
          assert(stringify("THURSDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.THURSDAY))) &&
          assert(stringify("FRIDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.FRIDAY))) &&
          assert(stringify("SATURDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.SATURDAY))) &&
          assert(stringify("SUNDAY").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.SUNDAY))) &&
          assert(stringify("monday").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("MonDay").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("MonDa\\u0079").fromJson[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("Mon").fromJson[DayOfWeek])(isLeft(equalTo("(expected a DayOfWeek)")))
        },
        test("DayOfWeek fromJsonAST") {
          assert(Json.Str("MONDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(Json.Str("TUESDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.TUESDAY))) &&
          assert(Json.Str("WEDNESDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.WEDNESDAY))) &&
          assert(Json.Str("THURSDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.THURSDAY))) &&
          assert(Json.Str("FRIDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.FRIDAY))) &&
          assert(Json.Str("SATURDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.SATURDAY))) &&
          assert(Json.Str("SUNDAY").as[DayOfWeek])(isRight(equalTo(DayOfWeek.SUNDAY))) &&
          assert(Json.Str("monday").as[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(Json.Str("MonDay").as[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(Json.Str("MonDa\\u0079").as[DayOfWeek])(isLeft(equalTo("(expected a DayOfWeek)"))) &&
          assert(Json.Str("Mon").as[DayOfWeek])(isLeft(equalTo("(expected a DayOfWeek)")))
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
          assert(stringify("2020-01-01T12:36:00").fromJson[LocalDateTime])(isRight(equalTo(p))) &&
          assert(stringify("2020-01-01T12:36:00.").fromJson[LocalDateTime])(isRight(equalTo(p)))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(stringify(n).fromJson[LocalTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36").fromJson[LocalTime])(isRight(equalTo(p))) &&
          assert(stringify("12:36:00").fromJson[LocalTime])(isRight(equalTo(p))) &&
          assert(stringify("12:36:00.").fromJson[LocalTime])(isRight(equalTo(p)))
        },
        test("Month fromJson") {
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
          assert(stringify("December").fromJson[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(stringify("Decembe\\u0072").fromJson[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(stringify("Dec").fromJson[Month])(isLeft(equalTo("(expected a Month)")))
        },
        test("Month fromJsonAST") {
          assert(Json.Str("JANUARY").as[Month])(isRight(equalTo(Month.JANUARY))) &&
          assert(Json.Str("FEBRUARY").as[Month])(isRight(equalTo(Month.FEBRUARY))) &&
          assert(Json.Str("MARCH").as[Month])(isRight(equalTo(Month.MARCH))) &&
          assert(Json.Str("APRIL").as[Month])(isRight(equalTo(Month.APRIL))) &&
          assert(Json.Str("MAY").as[Month])(isRight(equalTo(Month.MAY))) &&
          assert(Json.Str("JUNE").as[Month])(isRight(equalTo(Month.JUNE))) &&
          assert(Json.Str("JULY").as[Month])(isRight(equalTo(Month.JULY))) &&
          assert(Json.Str("AUGUST").as[Month])(isRight(equalTo(Month.AUGUST))) &&
          assert(Json.Str("SEPTEMBER").as[Month])(isRight(equalTo(Month.SEPTEMBER))) &&
          assert(Json.Str("OCTOBER").as[Month])(isRight(equalTo(Month.OCTOBER))) &&
          assert(Json.Str("NOVEMBER").as[Month])(isRight(equalTo(Month.NOVEMBER))) &&
          assert(Json.Str("DECEMBER").as[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(Json.Str("december").as[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(Json.Str("December").as[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(Json.Str("Decembe\\u0072").as[Month])(isLeft(equalTo("(expected a Month)"))) &&
          assert(Json.Str("Dec").as[Month])(isLeft(equalTo("(expected a Month)")))
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

          zdtAssert(
            "+164433183-11-15T12:32:00.076988677Z[Atlantic/Madeira]",
            OffsetDateTime
              .parse("+164433183-11-15T12:32:00.076988677Z")
              .atZoneSameInstant(ZoneId.of("Atlantic/Madeira"))
          ) &&
          zdtAssert(n.toString, n) &&
          zdtAssert("2020-01-01T12:36:00-05:00[America/New_York]", est) &&
          zdtAssert("2020-01-01T12:36:00Z[Etc/UTC]", utc) &&
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
          assert(stringify("-05:00").fromJson[ZoneOffset])(isRight(equalTo(ZoneOffset.ofHours(-5)))) &&
          assert(stringify("+05:10:10").fromJson[ZoneOffset])(
            isRight(equalTo(ZoneOffset.ofHoursMinutesSeconds(5, 10, 10)))
          )
        }
      ),
      suite("Decoder Sad Path")(
        test("Duration") {
          assert("""""""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""X"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""-"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""-X"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""PXD"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P-"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P-XD"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1XD"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""PT"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""PT0SX"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P106751991167301D"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1067519911673000D"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P-106751991167301D"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DX1H"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DTXH"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT-XH"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1XH"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1XM"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT2562047788015216H"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT-2562047788015216H"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT153722867280912931M"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT-153722867280912931M"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT9223372036854775808S"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT92233720368547758000S"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT-9223372036854775809S"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1MXS"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1M-XS"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1M0XS"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1M0.XS"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1M0.012345678XS"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P1DT1H1M0.0123456789S"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT0H0M9223372036854775808S"""".fromJson[Duration])(
            isLeft(containsString("expected a Duration"))
          ) &&
          assert(""""P0DT0H0M92233720368547758080S"""".fromJson[Duration])(
            isLeft(containsString("expected a Duration"))
          ) &&
          assert(""""P0DT0H0M-9223372036854775809S"""".fromJson[Duration])(
            isLeft(containsString("expected a Duration"))
          ) &&
          assert(""""P106751991167300DT24H"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT2562047788015215H60M"""".fromJson[Duration])(isLeft(containsString("expected a Duration"))) &&
          assert(""""P0DT0H153722867280912930M60S"""".fromJson[Duration])(isLeft(containsString("expected a Duration")))
        },
        test("Instant") {
          assert(stringify("").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-0").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-0").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T0").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:0").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("X020-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2X20-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("20X0-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("202X-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020X01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-X1-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-0X-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01X01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-X1T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-0XT01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01X01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01TX1:01").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T0X:01").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T24:01").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01X01").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:X1").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:0X").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:60").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01X").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("+1000000001-01-01T01:01Z").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("+3333333333-01-01T01:01Z").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("-1000000001-01-01T01:01Z").fromJson[Instant])(
            isLeft(containsString("expected an Instant"))
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("+10000").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-00-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-13-01T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-00T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-01-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-02-30T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-03-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-04-31T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-05-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-06-31T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-07-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-08-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-09-31T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-10-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-11-31T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant"))) &&
          assert(stringify("2020-12-32T01:01Z").fromJson[Instant])(isLeft(containsString("expected an Instant")))
        },
        test("LocalDate") {
          assert(stringify("").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-0").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01-0").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01-012").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("X020-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2X20-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("20X0-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("202X-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020X01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-X1-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-0X-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01X01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01-X1").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01-0X").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+X0000-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+1X000-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+10X00-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+100X0-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+1000X-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+10000X-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+100000X-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+1000000X-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+1000000000-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("-1000000000-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("-0000-01-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("+10000").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-00-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-13-01").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01-00").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-01-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-02-30").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-03-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-04-31").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-05-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-06-31").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-07-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-08-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-09-31").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-10-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-11-31").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate"))) &&
          assert(stringify("2020-12-32").fromJson[LocalDate])(isLeft(containsString("expected a LocalDate")))
        },
        test("LocalDateTime") {
          assert(stringify("").fromJson[LocalDateTime])(isLeft(containsString("expected a LocalDateTime"))) &&
          assert(stringify("2020").fromJson[LocalDateTime])(isLeft(containsString("expected a LocalDateTime"))) &&
          assert(stringify("2020-0").fromJson[LocalDateTime])(isLeft(containsString("expected a LocalDateTime"))) &&
          assert(stringify("2020-01-0").fromJson[LocalDateTime])(isLeft(containsString("expected a LocalDateTime"))) &&
          assert(stringify("2020-01-01T0").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("X020-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2X20-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("20X0-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("202X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020X01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-X1-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-0X-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01X01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-X1T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-0XT01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01X01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:X1").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:0X").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:60").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+X0000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+1X000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+10X00-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+100X0-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+1000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+10000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+100000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+1000000X-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+1000000000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("-1000000000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("-0000-01-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("+10000").fromJson[LocalDateTime])(isLeft(containsString("expected a LocalDateTime"))) &&
          assert(stringify("2020-00-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-13-01T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-00T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-01-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-02-30T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-03-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-04-31T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-05-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-06-31T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-07-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-08-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-09-31T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-10-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-11-31T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          ) &&
          assert(stringify("2020-12-32T01:01").fromJson[LocalDateTime])(
            isLeft(containsString("expected a LocalDateTime"))
          )
        },
        test("LocalTime") {
          assert(stringify("").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("0").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:0").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("X1:01").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("0X:01").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("24:01").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01X01").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:X1").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:0X").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:60").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01X").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01:0").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01:X1").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01:0X").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01:60").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01:012").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime"))) &&
          assert(stringify("01:01:01.X").fromJson[LocalTime])(isLeft(containsString("expected a LocalTime")))
        },
        test("MonthDay") {
          assert(stringify("").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("X-01-01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("-X01-01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--X1-01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--0X-01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--00-01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--13-01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--01X01").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--01-X1").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--01-0X").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--01-00").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--01-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--02-30").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--03-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--04-31").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--05-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--06-31").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--07-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--08-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--09-31").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--10-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--11-31").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay"))) &&
          assert(stringify("--12-32").fromJson[MonthDay])(isLeft(containsString("expected a MonthDay")))
        },
        test("OffsetDateTime") {
          assert(stringify("").fromJson[OffsetDateTime])(isLeft(containsString("expected an OffsetDateTime"))) &&
          assert(stringify("2020").fromJson[OffsetDateTime])(isLeft(containsString("expected an OffsetDateTime"))) &&
          assert(stringify("2020-0").fromJson[OffsetDateTime])(isLeft(containsString("expected an OffsetDateTime"))) &&
          assert(stringify("2020-01-0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01X").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("+10000").fromJson[OffsetDateTime])(isLeft(containsString("expected an OffsetDateTime"))) &&
          assert(stringify("2020-00-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJson[OffsetDateTime])(
            isLeft(containsString("expected an OffsetDateTime"))
          )
        },
        test("OffsetTime") {
          assert(stringify("").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("0").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:0").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("X1:01").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("0X:01").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("24:01").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01X01").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:X1").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:0X").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:60").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01X").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:0").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:X1Z").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:0XZ").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:60Z").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:012").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01.").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01.X").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01.123456789X").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01ZX").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01+X1:01:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+0").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01+0X:01:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+19:01:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01X01:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01X").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01+01:0").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01+01:X1:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:0X:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:60:01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:01X").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime"))) &&
          assert(stringify("01:01:01+01:01X01").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:01:0").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:01:X1").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:01:0X").fromJson[OffsetTime])(
            isLeft(containsString("expected an OffsetTime"))
          ) &&
          assert(stringify("01:01:01+01:01:60").fromJson[OffsetTime])(isLeft(containsString("expected an OffsetTime")))
        },
        test("Period") {
          assert(stringify("").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("X").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("-").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("PXY").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P-").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P-XY").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1XY").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P2147483648Y").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P21474836470Y").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P-2147483649Y").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P2147483648M").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P21474836470M").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P-2147483649M").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P2147483648W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P21474836470W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P-2147483649W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P2147483648D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P21474836470D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P-2147483649D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1YXM").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y-XM").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1XM").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y2147483648M").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y21474836470M").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y-2147483649M").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y2147483648W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y21474836470W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y-2147483649W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y2147483648D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y21474836470D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y-2147483649D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1MXW").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M-XW").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M1XW").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M306783379W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M3067833790W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M-306783379W").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M2147483648D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M21474836470D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M-2147483649D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M1WXD").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M1W-XD").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M1W1XD").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M306783378W8D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M-306783378W-8D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M1W2147483647D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M-1W-2147483648D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M0W2147483648D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M0W21474836470D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M0W-2147483649D").fromJson[Period])(isLeft(containsString("expected a Period"))) &&
          assert(stringify("P1Y1M1W1DX").fromJson[Period])(isLeft(containsString("expected a Period")))
        },
        test("Year") {
          assert(stringify("").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("2").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("22").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("222").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("X020").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("2X20").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("20X0").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("202X").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+X0000").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+1X000").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+10X00").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+100X0").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+1000X").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+10000X").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+100000X").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+1000000X").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("+1000000000").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("-1000000000").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("-0000").fromJson[Year])(isLeft(containsString("expected a Year"))) &&
          assert(stringify("10000").fromJson[Year])(isLeft(containsString("expected a Year")))
        },
        test("YearMonth") {
          assert(stringify("").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020-0").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020-012").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("X020-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2X20-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("20X0-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("202X-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020X01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020-X1").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020-0X").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+X0000-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+1X000-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+10X00-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+100X0-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+1000X-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+10000X-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+100000X-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+1000000X-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+1000000000-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("-1000000000-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("-0000-01").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("+10000").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020-00").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth"))) &&
          assert(stringify("2020-13").fromJson[YearMonth])(isLeft(containsString("expected a YearMonth")))
        },
        test("ZonedDateTime") {
          assert(stringify("").fromJson[ZonedDateTime])(isLeft(containsString("expected a ZonedDateTime"))) &&
          assert(stringify("2020").fromJson[ZonedDateTime])(isLeft(containsString("expected a ZonedDateTime"))) &&
          assert(stringify("2020-0").fromJson[ZonedDateTime])(isLeft(containsString("expected a ZonedDateTime"))) &&
          assert(stringify("2020-01-0").fromJson[ZonedDateTime])(isLeft(containsString("expected a ZonedDateTime"))) &&
          assert(stringify("2020-01-01T0").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:0").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01TX1:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T0X:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T24:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01X01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:X1").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:0X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:60").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:01X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("+10000").fromJson[ZonedDateTime])(isLeft(containsString("expected a ZonedDateTime"))) &&
          assert(stringify("2020-00-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01Z[").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01Z[X]").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          ) &&
          assert(stringify("2020-01-01T01:01Z[GMT]X").fromJson[ZonedDateTime])(
            isLeft(containsString("expected a ZonedDateTime"))
          )
        },
        test("ZoneId") {
          assert(stringify("America/New York").fromJson[ZoneId])(isLeft(containsString("expected a ZoneId"))) &&
          assert(stringify("Solar_System/Mars").fromJson[ZoneId])(isLeft(containsString("expected a ZoneId")))
        },
        test("ZoneOffset") {
          assert(stringify("").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("X").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+X1:01:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+0").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+0X:01:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+19:01:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01X01:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01X").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:0").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:X1:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:0X:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:60:01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:01X").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:01X01").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:01:0").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:01:X1").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:01:0X").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset"))) &&
          assert(stringify("+01:01:60").fromJson[ZoneOffset])(isLeft(containsString("expected a ZoneOffset")))
        }
      )
    )
}
