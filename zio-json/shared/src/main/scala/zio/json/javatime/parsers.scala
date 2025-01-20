/*
 * Copyright 2019-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.json.javatime

import java.time.{
  DateTimeException,
  Duration,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  MonthDay,
  OffsetDateTime,
  OffsetTime,
  Period,
  Year,
  YearMonth,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import java.util.concurrent.ConcurrentHashMap
import scala.annotation.switch
import scala.util.control.NoStackTrace

private[json] object parsers {
  private[this] final val zoneOffsets: Array[ZoneOffset]             = new Array(145)
  private[this] final val zoneIds: ConcurrentHashMap[String, ZoneId] = new ConcurrentHashMap(256)

  def unsafeParseDuration(input: String): Duration = {
    val len          = input.length
    var pos          = 0
    var seconds      = 0L
    var nanos, state = 0
    if (pos >= len) durationError(pos)
    var ch = input.charAt(pos)
    pos += 1
    val isNeg = ch == '-'
    if (isNeg) {
      if (pos >= len) durationError(pos)
      ch = input.charAt(pos)
      pos += 1
    }
    if (ch != 'P') durationOrPeriodStartError(isNeg, pos - 1)
    if (pos >= len) durationError(pos)
    ch = input.charAt(pos)
    pos += 1
    while ({
      if (state == 0) {
        if (ch == 'T') {
          if (pos >= len) durationError(pos)
          ch = input.charAt(pos)
          pos += 1
          state = 1
        }
      } else if (state == 1) {
        if (ch != 'T') charsError('T', '"', pos - 1)
        if (pos >= len) durationError(pos)
        ch = input.charAt(pos)
        pos += 1
      } else if (state == 4 && pos >= len) durationError(pos - 1)
      val isNegX = ch == '-'
      if (isNegX) {
        if (pos >= len) durationError(pos)
        ch = input.charAt(pos)
        pos += 1
      }
      if (ch < '0' || ch > '9') durationOrPeriodDigitError(isNegX, state <= 1, pos - 1)
      var x: Long = ('0' - ch).toLong
      while (
        (pos < len) && {
          ch = input.charAt(pos)
          ch >= '0' && ch <= '9'
        }
      ) {
        if (
          x < -922337203685477580L || {
            x = x * 10 + ('0' - ch)
            x > 0
          }
        ) durationError(pos)
        pos += 1
      }
      if (!(isNeg ^ isNegX)) {
        if (x == -9223372036854775808L) durationError(pos)
        x = -x
      }
      if (ch == 'D' && state <= 0) {
        if (x < -106751991167300L || x > 106751991167300L)
          durationError(pos) // -106751991167300L == Long.MinValue / 86400
        seconds = x * 86400
        state = 1
      } else if (ch == 'H' && state <= 1) {
        if (x < -2562047788015215L || x > 2562047788015215L)
          durationError(pos) // -2562047788015215L == Long.MinValue / 3600
        seconds = sumSeconds(x * 3600, seconds, pos)
        state = 2
      } else if (ch == 'M' && state <= 2) {
        if (x < -153722867280912930L || x > 153722867280912930L)
          durationError(pos) // -153722867280912930L == Long.MinValue / 60
        seconds = sumSeconds(x * 60, seconds, pos)
        state = 3
      } else if (ch == '.') {
        pos += 1
        seconds = sumSeconds(x, seconds, pos)
        var nanoDigitWeight = 100000000
        while (
          (pos < len) && {
            ch = input.charAt(pos)
            ch >= '0' && ch <= '9' && nanoDigitWeight != 0
          }
        ) {
          nanos += (ch - '0') * nanoDigitWeight
          nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
          pos += 1
        }
        if (ch != 'S') nanoError(nanoDigitWeight, 'S', pos)
        if (isNeg ^ isNegX) nanos = -nanos
        state = 4
      } else if (ch == 'S') {
        seconds = sumSeconds(x, seconds, pos)
        state = 4
      } else durationError(state, pos)
      pos += 1
      (pos < len) && {
        ch = input.charAt(pos)
        pos += 1
        true
      }
    }) ()
    Duration.ofSeconds(seconds, nanos.toLong)
  }

  def unsafeParseInstant(input: String): Instant = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 4 >= len) instantError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      val ch4 = input.charAt(pos + 4)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 != '-') charError('-', pos + 4)
        pos += 5
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 < '0' || ch4 > '9') digitError(pos + 4)
        pos += 5
        var year       = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
        var yearDigits = 4
        var ch: Char   = '0'
        while ({
          if (pos >= len) instantError(pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && yearDigits < 10
        }) {
          year =
            if (year > 100000000) 2147483647
            else year * 10 + (ch - '0')
          yearDigits += 1
        }
        if (yearDigits == 10 && year > 1000000000) yearError(pos - 2)
        if (yearNeg) {
          if (year == 0) yearError(pos - 2)
          year = -year
        }
        if (ch != '-') yearError(yearNeg, yearDigits, pos - 1)
        year
      }
    }
    val month = {
      if (pos + 2 >= len) instantError(pos)
      val ch0   = input.charAt(pos)
      val ch1   = input.charAt(pos + 1)
      val ch2   = input.charAt(pos + 2)
      val month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (month < 1 || month > 12) monthError(pos + 1)
      if (ch2 != '-') charError('-', pos + 2)
      pos += 3
      month
    }
    val day = {
      if (pos + 2 >= len) instantError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val day = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (day == 0 || (day > 28 && day > maxDayForYearMonth(year, month))) dayError(pos + 1)
      if (ch2 != 'T') charError('T', pos + 2)
      pos += 3
      day
    }
    val epochDay =
      epochDayForYear(year) + (dayOfYearForYearMonth(year, month) + day - 719529) // 719528 == days 0000 to 1970
    var epochSecond = {
      if (pos + 2 >= len) instantError(pos)
      val ch0  = input.charAt(pos)
      val ch1  = input.charAt(pos + 1)
      val ch2  = input.charAt(pos + 2)
      val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (hour > 23) hourError(pos + 1)
      if (ch2 != ':') charError(':', pos + 2)
      pos += 3
      hour * 3600
    }
    epochSecond += {
      if (pos + 1 >= len) instantError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (ch0 > '5') minuteError(pos + 1)
      pos += 2
      (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
    }
    var nanoDigitWeight = -1
    var nano            = 0
    var ch              = (0: Char)
    if (pos < len) {
      ch = input.charAt(pos)
      pos += 1
      if (ch == ':') {
        nanoDigitWeight = -2
        epochSecond += {
          if (pos + 1 >= len) instantError(pos)
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          if (ch0 < '0' || ch0 > '9') digitError(pos)
          if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
          if (ch0 > '5') secondError(pos + 1)
          pos += 2
          ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        }
        if (pos < len) {
          ch = input.charAt(pos)
          pos += 1
          if (ch == '.') {
            nanoDigitWeight = 100000000
            while (
              pos < len && {
                ch = input.charAt(pos)
                pos += 1
                ch >= '0' && ch <= '9' && nanoDigitWeight != 0
              }
            ) {
              nano += (ch - '0') * nanoDigitWeight
              nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
            }
          }
        }
      }
    }
    var offsetTotal = 0
    if (ch != 'Z') {
      val offsetNeg = ch == '-' || (ch != '+' && timezoneSignError(nanoDigitWeight, pos - 1))
      offsetTotal = {
        if (pos + 1 >= len) instantError(pos)
        val ch0        = input.charAt(pos)
        val ch1        = input.charAt(pos + 1)
        val offsetHour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (offsetHour > 18) timezoneOffsetHourError(pos + 1)
        pos += 2
        offsetHour * 3600
      }
      if (
        pos < len && {
          ch = input.charAt(pos)
          pos += 1
          ch == ':'
        }
      ) {
        offsetTotal += {
          if (pos + 1 >= len) instantError(pos)
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          if (ch0 < '0' || ch0 > '9') digitError(pos)
          if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
          if (ch0 > '5') timezoneOffsetMinuteError(pos + 1)
          pos += 2
          (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
        }
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':'
          }
        ) {
          offsetTotal += {
            if (pos + 1 >= len) instantError(pos)
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            if (ch0 < '0' || ch0 > '9') digitError(pos)
            if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
            if (ch0 > '5') timezoneOffsetSecondError(pos + 1)
            pos += 2
            ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          }
        }
      }
      if (offsetTotal > 64800) zoneOffsetError(pos) // 64800 == 18 * 60 * 60
      if (offsetNeg) offsetTotal = -offsetTotal
    }
    if (pos != len) instantError(pos)
    Instant.ofEpochSecond(
      epochDay * 86400 + (epochSecond - offsetTotal),
      nano.toLong
    ) // 86400 == seconds per day
  }

  def unsafeParseLocalDate(input: String): LocalDate = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 4 >= len) localDateError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      val ch4 = input.charAt(pos + 4)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 != '-') charError('-', pos + 4)
        pos += 5
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 < '0' || ch4 > '9') digitError(pos + 4)
        pos += 5
        var year       = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
        var yearDigits = 4
        var ch: Char   = '0'
        while ({
          if (pos >= len) localDateError(pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && yearDigits < 9
        }) {
          year = year * 10 + (ch - '0')
          yearDigits += 1
        }
        if (yearNeg) {
          if (year == 0) yearError(pos - 2)
          year = -year
        }
        if (ch != '-') yearError(yearNeg, yearDigits, pos - 1)
        year
      }
    }
    val month = {
      if (pos + 2 >= len) localDateError(pos)
      val ch0   = input.charAt(pos)
      val ch1   = input.charAt(pos + 1)
      val ch2   = input.charAt(pos + 2)
      val month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (month < 1 || month > 12) monthError(pos + 1)
      if (ch2 != '-') charError('-', pos + 2)
      pos += 3
      month
    }
    val day = {
      if (pos + 1 >= len) localDateError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val day = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (day == 0 || (day > 28 && day > maxDayForYearMonth(year, month))) dayError(pos + 1)
      pos += 2
      day
    }
    if (pos != len) localDateError(pos)
    LocalDate.of(year, month, day)
  }

  def unsafeParseLocalDateTime(input: String): LocalDateTime = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 4 >= len) localDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      val ch4 = input.charAt(pos + 4)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 != '-') charError('-', pos + 4)
        pos += 5
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 < '0' || ch4 > '9') digitError(pos + 4)
        pos += 5
        var year       = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
        var yearDigits = 4
        var ch: Char   = '0'
        while ({
          if (pos >= len) localDateTimeError(pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && yearDigits < 9
        }) {
          year = year * 10 + (ch - '0')
          yearDigits += 1
        }
        if (yearNeg) {
          if (year == 0) yearError(pos - 2)
          year = -year
        }
        if (ch != '-') yearError(yearNeg, yearDigits, pos - 1)
        year
      }
    }
    val month = {
      if (pos + 2 >= len) localDateTimeError(pos)
      val ch0   = input.charAt(pos)
      val ch1   = input.charAt(pos + 1)
      val ch2   = input.charAt(pos + 2)
      val month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (month < 1 || month > 12) monthError(pos + 1)
      if (ch2 != '-') charError('-', pos + 2)
      pos += 3
      month
    }
    val day = {
      if (pos + 2 >= len) localDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val day = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (day == 0 || (day > 28 && day > maxDayForYearMonth(year, month))) dayError(pos + 1)
      if (ch2 != 'T') charError('T', pos + 2)
      pos += 3
      day
    }
    val hour = {
      if (pos + 2 >= len) localDateTimeError(pos)
      val ch0  = input.charAt(pos)
      val ch1  = input.charAt(pos + 1)
      val ch2  = input.charAt(pos + 2)
      val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (hour > 23) hourError(pos + 1)
      if (ch2 != ':') charError(':', pos + 2)
      pos += 3
      hour
    }
    val minute = {
      if (pos + 1 >= len) localDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (ch0 > '5') minuteError(pos + 1)
      pos += 2
      ch0 * 10 + ch1 - 528 // 528 == '0' * 11
    }
    var second, nano = 0
    if (pos < len) {
      if (input.charAt(pos) != ':') charError(':', pos)
      pos += 1
      second = {
        if (pos + 1 >= len) localDateTimeError(pos)
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch0 > '5') secondError(pos + 1)
        pos += 2
        ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      }
      if (pos < len) {
        if (input.charAt(pos) != '.') charError('.', pos)
        pos += 1
        var nanoDigitWeight = 100000000
        var ch              = '0'
        while (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch >= '0' && ch <= '9' && nanoDigitWeight != 0
          }
        ) {
          nano += (ch - '0') * nanoDigitWeight
          nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
        }
        if (pos != len || ch < '0' || ch > '9') localDateTimeError(pos - 1)
      }
    }
    LocalDateTime.of(year, month, day, hour, minute, second, nano)
  }

  def unsafeParseLocalTime(input: String): LocalTime = {
    val len = input.length
    var pos = 0
    val hour = {
      if (pos + 2 >= len) localTimeError(pos)
      val ch0  = input.charAt(pos)
      val ch1  = input.charAt(pos + 1)
      val ch2  = input.charAt(pos + 2)
      val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (hour > 23) hourError(pos + 1)
      if (ch2 != ':') charError(':', pos + 2)
      pos += 3
      hour
    }
    val minute = {
      if (pos + 1 >= len) localTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (ch0 > '5') minuteError(pos + 1)
      pos += 2
      ch0 * 10 + ch1 - 528 // 528 == '0' * 11
    }
    var second, nano = 0
    if (pos < len) {
      if (input.charAt(pos) != ':') charError(':', pos)
      pos += 1
      second = {
        if (pos + 1 >= len) localTimeError(pos)
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch0 > '5') secondError(pos + 1)
        pos += 2
        ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      }
      if (pos < len) {
        if (input.charAt(pos) != '.') charError('.', pos)
        pos += 1
        var nanoDigitWeight = 100000000
        var ch              = '0'
        while (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch >= '0' && ch <= '9' && nanoDigitWeight != 0
          }
        ) {
          nano += (ch - '0') * nanoDigitWeight
          nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
        }
        if (pos != len || ch < '0' || ch > '9') localTimeError(pos - 1)
      }
    }
    LocalTime.of(hour, minute, second, nano)
  }

  def unsafeParseMonthDay(input: String): MonthDay = {
    if (input.length != 7) error("illegal month day", 0)
    val ch0   = input.charAt(0)
    val ch1   = input.charAt(1)
    val ch2   = input.charAt(2)
    val ch3   = input.charAt(3)
    val ch4   = input.charAt(4)
    val ch5   = input.charAt(5)
    val ch6   = input.charAt(6)
    val month = ch2 * 10 + ch3 - 528 // 528 == '0' * 11
    val day   = ch5 * 10 + ch6 - 528 // 528 == '0' * 11
    if (ch0 != '-') charError('-', 0)
    if (ch1 != '-') charError('-', 1)
    if (ch2 < '0' || ch2 > '9') digitError(2)
    if (ch3 < '0' || ch3 > '9') digitError(3)
    if (month < 1 || month > 12) monthError(3)
    if (ch4 != '-') charError('-', 4)
    if (ch5 < '0' || ch5 > '9') digitError(5)
    if (ch6 < '0' || ch6 > '9') digitError(6)
    if (day == 0 || (day > 28 && day > maxDayForMonth(month))) dayError(6)
    MonthDay.of(month, day)
  }

  def unsafeParseOffsetDateTime(input: String): OffsetDateTime = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 4 >= len) offsetDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      val ch4 = input.charAt(pos + 4)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 != '-') charError('-', pos + 4)
        pos += 5
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 < '0' || ch4 > '9') digitError(pos + 4)
        pos += 5
        var year       = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
        var yearDigits = 4
        var ch: Char   = '0'
        while ({
          if (pos >= len) offsetDateTimeError(pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && yearDigits < 9
        }) {
          year = year * 10 + (ch - '0')
          yearDigits += 1
        }
        if (yearNeg) {
          if (year == 0) yearError(pos - 2)
          year = -year
        }
        if (ch != '-') yearError(yearNeg, yearDigits, pos - 1)
        year
      }
    }
    val month = {
      if (pos + 2 >= len) offsetDateTimeError(pos)
      val ch0   = input.charAt(pos)
      val ch1   = input.charAt(pos + 1)
      val ch2   = input.charAt(pos + 2)
      val month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (month < 1 || month > 12) monthError(pos + 1)
      if (ch2 != '-') charError('-', pos + 2)
      pos += 3
      month
    }
    val day = {
      if (pos + 2 >= len) offsetDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val day = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (day == 0 || (day > 28 && day > maxDayForYearMonth(year, month))) dayError(pos + 1)
      if (ch2 != 'T') charError('T', pos + 2)
      pos += 3
      day
    }
    val hour = {
      if (pos + 2 >= len) offsetDateTimeError(pos)
      val ch0  = input.charAt(pos)
      val ch1  = input.charAt(pos + 1)
      val ch2  = input.charAt(pos + 2)
      val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (hour > 23) hourError(pos + 1)
      if (ch2 != ':') charError(':', pos + 2)
      pos += 3
      hour
    }
    val minute = {
      if (pos + 1 >= len) offsetDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (ch0 > '5') minuteError(pos + 1)
      pos += 2
      ch0 * 10 + ch1 - 528 // 528 == '0' * 11
    }
    var second, nano    = 0
    var nanoDigitWeight = -1
    if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
    var ch = input.charAt(pos)
    pos += 1
    if (ch == ':') {
      nanoDigitWeight = -2
      second = {
        if (pos + 1 >= len) offsetDateTimeError(pos)
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch0 > '5') secondError(pos + 1)
        pos += 2
        ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      }
      if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
      ch = input.charAt(pos)
      pos += 1
      if (ch == '.') {
        nanoDigitWeight = 100000000
        while ({
          if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && nanoDigitWeight != 0
        }) {
          nano += (ch - '0') * nanoDigitWeight
          nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
        }
      }
    }
    val zoneOffset =
      if (ch == 'Z') ZoneOffset.UTC
      else {
        val offsetNeg = ch == '-' || (ch != '+' && timezoneSignError(nanoDigitWeight, pos - 1))
        val offsetHour = {
          if (pos + 1 >= len) offsetDateTimeError(pos)
          val ch0        = input.charAt(pos)
          val ch1        = input.charAt(pos + 1)
          val offsetHour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          if (ch0 < '0' || ch0 > '9') digitError(pos)
          if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
          if (offsetHour > 18) timezoneOffsetHourError(pos + 1)
          pos += 2
          offsetHour
        }
        var offsetMinute, offsetSecond = 0
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':'
          }
        ) {
          offsetMinute = {
            if (pos + 1 >= len) offsetDateTimeError(pos)
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            if (ch0 < '0' || ch0 > '9') digitError(pos)
            if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
            if (ch0 > '5') timezoneOffsetMinuteError(pos + 1)
            pos += 2
            ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          }
          if (
            pos < len && {
              ch = input.charAt(pos)
              pos += 1
              ch == ':'
            }
          ) {
            offsetSecond = {
              if (pos + 1 >= len) offsetDateTimeError(pos)
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              if (ch0 < '0' || ch0 > '9') digitError(pos)
              if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
              if (ch0 > '5') timezoneOffsetSecondError(pos + 1)
              pos += 2
              ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            }
          }
        }
        toZoneOffset(offsetNeg, offsetHour, offsetMinute, offsetSecond, pos)
      }
    if (pos != len) offsetDateTimeError(pos)
    OffsetDateTime.of(year, month, day, hour, minute, second, nano, zoneOffset)
  }

  def unsafeParseOffsetTime(input: String): OffsetTime = {
    val len = input.length
    var pos = 0
    val hour = {
      if (pos + 2 >= len) offsetTimeError(pos)
      val ch0  = input.charAt(pos)
      val ch1  = input.charAt(pos + 1)
      val ch2  = input.charAt(pos + 2)
      val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (hour > 23) hourError(pos + 1)
      if (ch2 != ':') charError(':', pos + 2)
      pos += 3
      hour
    }
    val minute = {
      if (pos + 1 >= len) offsetTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (ch0 > '5') minuteError(pos + 1)
      pos += 2
      ch0 * 10 + ch1 - 528 // 528 == '0' * 11
    }
    var second, nano    = 0
    var nanoDigitWeight = -1
    if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
    var ch = input.charAt(pos)
    pos += 1
    if (ch == ':') {
      nanoDigitWeight = -2
      second = {
        if (pos + 1 >= len) offsetTimeError(pos)
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch0 > '5') secondError(pos + 1)
        pos += 2
        ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      }
      if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
      ch = input.charAt(pos)
      pos += 1
      if (ch == '.') {
        nanoDigitWeight = 100000000
        while ({
          if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && nanoDigitWeight != 0
        }) {
          nano += (ch - '0') * nanoDigitWeight
          nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
        }
      }
    }
    val zoneOffset =
      if (ch == 'Z') ZoneOffset.UTC
      else {
        val offsetNeg = ch == '-' || (ch != '+' && timezoneSignError(nanoDigitWeight, pos - 1))
        nanoDigitWeight = -3
        val offsetHour = {
          if (pos + 1 >= len) offsetTimeError(pos)
          val ch0        = input.charAt(pos)
          val ch1        = input.charAt(pos + 1)
          val offsetHour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          if (ch0 < '0' || ch0 > '9') digitError(pos)
          if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
          if (offsetHour > 18) timezoneOffsetHourError(pos + 1)
          pos += 2
          offsetHour
        }
        var offsetMinute, offsetSecond = 0
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':'
          }
        ) {
          offsetMinute = {
            if (pos + 1 >= len) offsetTimeError(pos)
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            if (ch0 < '0' || ch0 > '9') digitError(pos)
            if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
            if (ch0 > '5') timezoneOffsetMinuteError(pos + 1)
            pos += 2
            ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          }
          if (
            pos < len && {
              ch = input.charAt(pos)
              pos += 1
              ch == ':'
            }
          ) {
            nanoDigitWeight = -4
            offsetSecond = {
              if (pos + 1 >= len) offsetTimeError(pos)
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              if (ch0 < '0' || ch0 > '9') digitError(pos)
              if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
              if (ch0 > '5') timezoneOffsetSecondError(pos + 1)
              pos += 2
              ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            }
          }
        }
        toZoneOffset(offsetNeg, offsetHour, offsetMinute, offsetSecond, pos)
      }
    if (pos != len) offsetTimeError(pos)
    OffsetTime.of(hour, minute, second, nano, zoneOffset)
  }

  def unsafeParsePeriod(input: String): Period = {
    val len                             = input.length
    var pos, state, years, months, days = 0
    if (pos >= len) periodError(pos)
    var ch = input.charAt(pos)
    pos += 1
    val isNeg = ch == '-'
    if (isNeg) {
      if (pos >= len) periodError(pos)
      ch = input.charAt(pos)
      pos += 1
    }
    if (ch != 'P') durationOrPeriodStartError(isNeg, pos - 1)
    if (pos >= len) periodError(pos)
    ch = input.charAt(pos)
    pos += 1
    while ({
      if (state == 4 && pos >= len) periodError(pos - 1)
      val isNegX = ch == '-'
      if (isNegX) {
        if (pos >= len) periodError(pos)
        ch = input.charAt(pos)
        pos += 1
      }
      if (ch < '0' || ch > '9') durationOrPeriodDigitError(isNegX, state <= 1, pos - 1)
      var x: Int = '0' - ch
      while (
        (pos < len) && {
          ch = input.charAt(pos)
          ch >= '0' && ch <= '9'
        }
      ) {
        if (
          x < -214748364 || {
            x = x * 10 + ('0' - ch)
            x > 0
          }
        ) periodError(pos)
        pos += 1
      }
      if (!(isNeg ^ isNegX)) {
        if (x == -2147483648) periodError(pos)
        x = -x
      }
      if (ch == 'Y' && state <= 0) {
        years = x
        state = 1
      } else if (ch == 'M' && state <= 1) {
        months = x
        state = 2
      } else if (ch == 'W' && state <= 2) {
        if (x < -306783378 || x > 306783378) periodError(pos)
        days = x * 7
        state = 3
      } else if (ch == 'D') {
        val ds = x.toLong + days
        if (ds != ds.toInt) periodError(pos)
        days = ds.toInt
        state = 4
      } else periodError(state, pos)
      pos += 1
      (pos < len) && {
        ch = input.charAt(pos)
        pos += 1
        true
      }
    }) ()
    Period.of(years, months, days)
  }

  def unsafeParseYear(input: String): Year = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 3 >= len) yearError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (len != 4) yearError(pos + 4)
        pos += 4
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        pos += 4
        var year       = ch1 * 100 + ch2 * 10 + ch3 - 5328 // 53328 == '0' * 111
        var yearDigits = 3
        var ch: Char   = '0'
        while (
          pos < len && {
            ch = input.charAt(pos)
            ch >= '0' && ch <= '9' && yearDigits < 9
          }
        ) {
          year = year * 10 + (ch - '0')
          yearDigits += 1
          pos += 1
        }
        if (yearNeg) {
          if (year == 0) yearError(pos - 1)
          year = -year
        }
        if (pos != len || ch < '0' || ch > '9') {
          if (yearDigits == 9) yearError(pos)
          digitError(pos)
        }
        year
      }
    }
    Year.of(year)
  }

  def unsafeParseYearMonth(input: String): YearMonth = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 4 >= len) yearMonthError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      val ch4 = input.charAt(pos + 4)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 != '-') charError('-', pos + 4)
        pos += 5
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 < '0' || ch4 > '9') digitError(pos + 4)
        pos += 5
        var year       = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
        var yearDigits = 4
        var ch: Char   = '0'
        while ({
          if (pos >= len) yearMonthError(pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && yearDigits < 9
        }) {
          year = year * 10 + (ch - '0')
          yearDigits += 1
        }
        if (yearNeg) {
          if (year == 0) yearError(pos - 2)
          year = -year
        }
        if (ch != '-') yearError(yearNeg, yearDigits, pos - 1)
        year
      }
    }
    val month = {
      if (pos + 1 >= len) yearMonthError(pos)
      val ch0   = input.charAt(pos)
      val ch1   = input.charAt(pos + 1)
      val month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (month < 1 || month > 12) monthError(pos + 1)
      pos += 2
      month
    }
    if (pos != len) yearMonthError(pos)
    YearMonth.of(year, month)
  }

  def unsafeParseZonedDateTime(input: String): ZonedDateTime = {
    val len = input.length
    var pos = 0
    val year = {
      if (pos + 4 >= len) zonedDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val ch3 = input.charAt(pos + 3)
      val ch4 = input.charAt(pos + 4)
      if (ch0 >= '0' && ch0 <= '9') {
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 != '-') charError('-', pos + 4)
        pos += 5
        ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
      } else {
        val yearNeg = ch0 == '-' || (ch0 != '+' && charsOrDigitError('-', '+', pos))
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch2 < '0' || ch2 > '9') digitError(pos + 2)
        if (ch3 < '0' || ch3 > '9') digitError(pos + 3)
        if (ch4 < '0' || ch4 > '9') digitError(pos + 4)
        pos += 5
        var year       = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
        var yearDigits = 4
        var ch: Char   = '0'
        while ({
          if (pos >= len) zonedDateTimeError(pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && yearDigits < 9
        }) {
          year = year * 10 + (ch - '0')
          yearDigits += 1
        }
        if (yearNeg) {
          if (year == 0) yearError(pos - 2)
          year = -year
        }
        if (ch != '-') yearError(yearNeg, yearDigits, pos - 1)
        year
      }
    }
    val month = {
      if (pos + 2 >= len) zonedDateTimeError(pos)
      val ch0   = input.charAt(pos)
      val ch1   = input.charAt(pos + 1)
      val ch2   = input.charAt(pos + 2)
      val month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (month < 1 || month > 12) monthError(pos + 1)
      if (ch2 != '-') charError('-', pos + 2)
      pos += 3
      month
    }
    val day = {
      if (pos + 2 >= len) zonedDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      val ch2 = input.charAt(pos + 2)
      val day = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (day == 0 || (day > 28 && day > maxDayForYearMonth(year, month))) dayError(pos + 1)
      if (ch2 != 'T') charError('T', pos + 2)
      pos += 3
      day
    }
    val hour = {
      if (pos + 2 >= len) zonedDateTimeError(pos)
      val ch0  = input.charAt(pos)
      val ch1  = input.charAt(pos + 1)
      val ch2  = input.charAt(pos + 2)
      val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (hour > 23) hourError(pos + 1)
      if (ch2 != ':') charError(':', pos + 2)
      pos += 3
      hour
    }
    val minute = {
      if (pos + 1 >= len) zonedDateTimeError(pos)
      val ch0 = input.charAt(pos)
      val ch1 = input.charAt(pos + 1)
      if (ch0 < '0' || ch0 > '9') digitError(pos)
      if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
      if (ch0 > '5') minuteError(pos + 1)
      pos += 2
      ch0 * 10 + ch1 - 528 // 528 == '0' * 11
    }
    var second, nano    = 0
    var nanoDigitWeight = -1
    if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
    var ch = input.charAt(pos)
    pos += 1
    if (ch == ':') {
      nanoDigitWeight = -2
      second = {
        if (pos + 1 >= len) zonedDateTimeError(pos)
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (ch0 > '5') secondError(pos + 1)
        pos += 2
        ch0 * 10 + ch1 - 528 // 528 == '0' * 11
      }
      if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
      ch = input.charAt(pos)
      pos += 1
      if (ch == '.') {
        nanoDigitWeight = 100000000
        while ({
          if (pos >= len) timezoneSignError(nanoDigitWeight, pos)
          ch = input.charAt(pos)
          pos += 1
          ch >= '0' && ch <= '9' && nanoDigitWeight != 0
        }) {
          nano += (ch - '0') * nanoDigitWeight
          nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
        }
      }
    }
    val localDateTime = LocalDateTime.of(year, month, day, hour, minute, second, nano)
    val zoneOffset =
      if (ch == 'Z') {
        if (pos < len) {
          ch = input.charAt(pos)
          if (ch != '[') charError('[', pos)
          pos += 1
        }
        ZoneOffset.UTC
      } else {
        val offsetNeg = ch == '-' || (ch != '+' && timezoneSignError(nanoDigitWeight, pos - 1))
        nanoDigitWeight = -3
        val offsetHour = {
          if (pos + 1 >= len) zonedDateTimeError(pos)
          val ch0        = input.charAt(pos)
          val ch1        = input.charAt(pos + 1)
          val offsetHour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          if (ch0 < '0' || ch0 > '9') digitError(pos)
          if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
          if (offsetHour > 18) timezoneOffsetHourError(pos + 1)
          pos += 2
          offsetHour
        }
        var offsetMinute, offsetSecond = 0
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':' || ch != '[' && charError('[', pos - 1)
          }
        ) {
          offsetMinute = {
            if (pos + 1 >= len) zonedDateTimeError(pos)
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            if (ch0 < '0' || ch0 > '9') digitError(pos)
            if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
            if (ch0 > '5') timezoneOffsetMinuteError(pos + 1)
            pos += 2
            ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          }
          if (
            pos < len && {
              ch = input.charAt(pos)
              pos += 1
              ch == ':' || ch != '[' && charError('[', pos - 1)
            }
          ) {
            nanoDigitWeight = -4
            offsetSecond = {
              if (pos + 1 >= len) zonedDateTimeError(pos)
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              if (ch0 < '0' || ch0 > '9') digitError(pos)
              if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
              if (ch0 > '5') timezoneOffsetSecondError(pos + 1)
              pos += 2
              ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            }
            if (pos < len) {
              ch = input.charAt(pos)
              if (ch != '[') charError('[', pos)
              pos += 1
            }
          }
        }
        toZoneOffset(offsetNeg, offsetHour, offsetMinute, offsetSecond, pos)
      }
    if (ch == '[') {
      val zone =
        try {
          val from = pos
          while ({
            if (pos >= len) zonedDateTimeError(pos)
            ch = input.charAt(pos)
            ch != ']'
          }) pos += 1
          val key    = input.substring(from, pos)
          var zoneId = zoneIds.get(key)
          if (
            (zoneId eq null) && {
              zoneId = ZoneId.of(key)
              !zoneId.isInstanceOf[ZoneOffset] || zoneId.asInstanceOf[ZoneOffset].getTotalSeconds % 900 == 0
            }
          ) zoneIds.put(key, zoneId)
          zoneId
        } catch {
          case _: DateTimeException => zonedDateTimeError(pos - 1)
        }
      pos += 1
      if (pos != len) zonedDateTimeError(pos)
      ZonedDateTime.ofInstant(localDateTime, zoneOffset, zone)
    } else ZonedDateTime.ofLocal(localDateTime, zoneOffset, null)
  }

  def unsafeParseZoneId(input: String): ZoneId =
    try {
      var zoneId = zoneIds.get(input)
      if (
        (zoneId eq null) && {
          zoneId = ZoneId.of(input)
          !zoneId.isInstanceOf[ZoneOffset] || zoneId.asInstanceOf[ZoneOffset].getTotalSeconds % 900 == 0
        }
      ) zoneIds.put(input, zoneId)
      zoneId
    } catch {
      case _: DateTimeException => error("illegal zone id", 0)
    }

  def unsafeParseZoneOffset(input: String): ZoneOffset = {
    val len                  = input.length
    var pos, nanoDigitWeight = 0
    if (pos >= len) zoneOffsetError(pos)
    var ch = input.charAt(pos)
    pos += 1
    if (ch == 'Z') ZoneOffset.UTC
    else {
      val offsetNeg = ch == '-' || (ch != '+' && timezoneSignError(nanoDigitWeight, pos - 1))
      nanoDigitWeight = -3
      val offsetHour = {
        if (pos + 1 >= len) zoneOffsetError(pos)
        val ch0        = input.charAt(pos)
        val ch1        = input.charAt(pos + 1)
        val offsetHour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        if (ch0 < '0' || ch0 > '9') digitError(pos)
        if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
        if (offsetHour > 18) timezoneOffsetHourError(pos + 1)
        pos += 2
        offsetHour
      }
      var offsetMinute, offsetSecond = 0
      if (
        pos < len && {
          ch = input.charAt(pos)
          pos += 1
          ch == ':'
        }
      ) {
        offsetMinute = {
          if (pos + 1 >= len) zoneOffsetError(pos)
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          if (ch0 < '0' || ch0 > '9') digitError(pos)
          if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
          if (ch0 > '5') timezoneOffsetMinuteError(pos + 1)
          pos += 2
          ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        }
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':'
          }
        ) {
          nanoDigitWeight = -4
          offsetSecond = {
            if (pos + 1 >= len) zoneOffsetError(pos)
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            if (ch0 < '0' || ch0 > '9') digitError(pos)
            if (ch1 < '0' || ch1 > '9') digitError(pos + 1)
            if (ch0 > '5') timezoneOffsetSecondError(pos + 1)
            pos += 2
            ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          }
        }
      }
      if (pos != len) zoneOffsetError(pos)
      toZoneOffset(offsetNeg, offsetHour, offsetMinute, offsetSecond, pos)
    }
  }

  private[this] def toZoneOffset(
    offsetNeg: Boolean,
    offsetHour: Int,
    offsetMinute: Int,
    offsetSecond: Int,
    pos: Int
  ): ZoneOffset = {
    var offsetTotal = offsetHour * 3600 + offsetMinute * 60 + offsetSecond
    var qp          = offsetTotal * 37283
    if (offsetTotal > 64800) zoneOffsetError(pos) // 64800 == 18 * 60 * 60
    if ((qp & 0x1ff8000) == 0) {                  // check if offsetTotal divisible by 900
      qp >>>= 25                                  // divide offsetTotal by 900
      if (offsetNeg) qp = -qp
      var zoneOffset = zoneOffsets(qp + 72)
      if (zoneOffset ne null) zoneOffset
      else {
        if (offsetNeg) offsetTotal = -offsetTotal
        zoneOffset = ZoneOffset.ofTotalSeconds(offsetTotal)
        zoneOffsets(qp + 72) = zoneOffset
        zoneOffset
      }
    } else {
      if (offsetNeg) offsetTotal = -offsetTotal
      ZoneOffset.ofTotalSeconds(offsetTotal)
    }
  }

  private[this] def sumSeconds(s1: Long, s2: Long, pos: Int): Long = {
    val s = s1 + s2
    if (((s1 ^ s) & (s2 ^ s)) < 0) durationError(pos)
    s
  }

  private[this] def epochDayForYear(year: Int): Long =
    year * 365L + ((year + 3 >> 2) - {
      val cp = year * 1374389535L
      if (year < 0) (cp >> 37) - (cp >> 39)                        // year / 100 - year / 400
      else (cp + 136064563965L >> 37) - (cp + 548381424465L >> 39) // (year + 99) / 100 - (year + 399) / 400
    }.toInt)

  private[this] def dayOfYearForYearMonth(year: Int, month: Int): Int =
    (month * 1002277 - 988622 >> 15) - // (month * 367 - 362) / 12
      (if (month <= 2) 0
       else if (isLeap(year)) 1
       else 2)

  private[this] def maxDayForMonth(month: Int): Int =
    if (month != 2) ((month >> 3) ^ (month & 0x1)) + 30
    else 29

  private[this] def maxDayForYearMonth(year: Int, month: Int): Int =
    if (month != 2) ((month >> 3) ^ (month & 0x1)) + 30
    else if (isLeap(year)) 29
    else 28

  private[this] def isLeap(year: Int): Boolean = (year & 0x3) == 0 && { // (year % 100 != 0 || year % 400 == 0)
    val cp = year * 1374389535L
    val cc = year >> 31
    ((cp ^ cc) & 0x1fc0000000L) != 0 || (((cp >> 37).toInt - cc) & 0x3) == 0
  }

  private[this] def nanoError(nanoDigitWeight: Int, ch: Char, pos: Int): Nothing = {
    if (nanoDigitWeight == 0) charError(ch, pos)
    charOrDigitError(ch, pos)
  }

  private[this] def durationOrPeriodStartError(isNeg: Boolean, pos: Int) =
    error(
      if (isNeg) "expected 'P'"
      else "expected 'P' or '-'",
      pos
    )

  private[this] def durationOrPeriodDigitError(isNegX: Boolean, isNumReq: Boolean, pos: Int): Nothing =
    error(
      if (isNegX) "expected digit"
      else if (isNumReq) "expected '-' or digit"
      else "expected '\"' or '-' or digit",
      pos
    )

  private[this] def durationError(state: Int, pos: Int): Nothing =
    error(
      (state: @switch) match {
        case 0 => "expected 'D' or digit"
        case 1 => "expected 'H' or 'M' or 'S or '.' or digit"
        case 2 => "expected 'M' or 'S or '.' or digit"
        case 3 => "expected 'S or '.' or digit"
      },
      pos
    )

  private[this] def durationError(pos: Int) = error("illegal duration", pos)

  private[this] def timezoneSignError(nanoDigitWeight: Int, pos: Int) =
    error(
      if (nanoDigitWeight == -2) "expected '.' or '+' or '-' or 'Z'"
      else if (nanoDigitWeight == -1) "expected ':' or '+' or '-' or 'Z'"
      else if (nanoDigitWeight == 0) "expected '+' or '-' or 'Z'"
      else "expected digit or '+' or '-' or 'Z'",
      pos
    )

  private[this] def instantError(pos: Int) = error("illegal instant", pos)

  private[this] def localDateError(pos: Int) = error("illegal local date", pos)

  private[this] def localDateTimeError(pos: Int) = error("illegal local date time", pos)

  private[this] def localTimeError(pos: Int) = error("illegal local time", pos)

  private[this] def offsetDateTimeError(pos: Int) = error("illegal offset date time", pos)

  private[this] def offsetTimeError(pos: Int) = error("illegal offset time", pos)

  private[this] def periodError(state: Int, pos: Int): Nothing =
    error(
      (state: @switch) match {
        case 0 => "expected 'Y' or 'M' or 'W' or 'D' or digit"
        case 1 => "expected 'M' or 'W' or 'D' or digit"
        case 2 => "expected 'W' or 'D' or digit"
        case 3 => "expected 'D' or digit"
      },
      pos
    )

  private[this] def periodError(pos: Int) = error("illegal period", pos)

  private[this] def yearMonthError(pos: Int) = error("illegal year month", pos)

  private[this] def zonedDateTimeError(pos: Int) = error("illegal zoned date time", pos)

  private[this] def zoneOffsetError(pos: Int) = error("illegal zone offset", pos)

  private[this] def yearError(yearNeg: Boolean, yearDigits: Int, pos: Int) = {
    if (!yearNeg && yearDigits == 4) digitError(pos)
    if (yearDigits == 9) charError('-', pos)
    charOrDigitError('-', pos)
  }

  private[this] def yearError(pos: Int) = error("illegal year", pos)

  private[this] def monthError(pos: Int) = error("illegal month", pos)

  private[this] def dayError(pos: Int) = error("illegal day", pos)

  private[this] def hourError(pos: Int) = error("illegal hour", pos)

  private[this] def minuteError(pos: Int) = error("illegal minute", pos)

  private[this] def secondError(pos: Int) = error("illegal second", pos)

  private[this] def timezoneOffsetHourError(pos: Int) = error("illegal timezone offset hour", pos)

  private[this] def timezoneOffsetMinuteError(pos: Int) = error("illegal timezone offset minute", pos)

  private[this] def timezoneOffsetSecondError(pos: Int) = error("illegal timezone offset second", pos)

  private[this] def digitError(pos: Int) = error("expected digit", pos)

  private[this] def charsOrDigitError(ch1: Char, ch2: Char, pos: Int) =
    error(s"expected '$ch1' or '$ch2' or digit", pos)

  private[this] def charsError(ch1: Char, ch2: Char, pos: Int) = error(s"expected '$ch1' or '$ch2'", pos)

  private[this] def charOrDigitError(ch1: Char, pos: Int) = error(s"expected '$ch1' or digit", pos)

  private[this] def charError(ch: Char, pos: Int) = error(s"expected '$ch'", pos)

  @noinline
  private[this] def error(msg: String, pos: Int): Nothing =
    throw new DateTimeException(msg + " at index " + pos) with NoStackTrace
}
