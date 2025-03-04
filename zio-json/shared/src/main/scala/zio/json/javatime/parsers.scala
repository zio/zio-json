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

import java.time._
import java.util.concurrent.ConcurrentHashMap
import scala.util.control.NoStackTrace

private[json] object parsers {
  private[this] final val zoneOffsets: Array[ZoneOffset]             = new Array(145)
  private[this] final val zoneIds: ConcurrentHashMap[String, ZoneId] = new ConcurrentHashMap(256)

  def unsafeParseDuration(input: String): Duration = {
    val len          = input.length
    var pos          = 0
    var seconds      = 0L
    var nanos, state = 0
    if (pos >= len) durationError()
    var ch = input.charAt(pos)
    pos += 1
    val isNeg = ch == '-'
    if (isNeg) {
      if (pos >= len) durationError()
      ch = input.charAt(pos)
      pos += 1
    }
    if (ch != 'P' || pos >= len) durationError()
    ch = input.charAt(pos)
    pos += 1
    while ({
      if (state == 0) {
        if (ch == 'T') {
          if (pos >= len) durationError()
          ch = input.charAt(pos)
          pos += 1
          state = 1
        }
      } else if (state == 1) {
        if (ch != 'T' || pos >= len) durationError()
        ch = input.charAt(pos)
        pos += 1
      } else if (state == 4 && pos >= len) durationError()
      val isNegX = ch == '-'
      if (isNegX) {
        if (pos >= len) durationError()
        ch = input.charAt(pos)
        pos += 1
      }
      if (ch < '0' || ch > '9') durationError()
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
        ) durationError()
        pos += 1
      }
      if (!(isNeg ^ isNegX)) {
        if (x == -9223372036854775808L) durationError()
        x = -x
      }
      if (ch == 'D' && state <= 0) {
        if (x < -106751991167300L || x > 106751991167300L) durationError()
        seconds = x * 86400
        state = 1
      } else if (ch == 'H' && state <= 1) {
        if (x < -2562047788015215L || x > 2562047788015215L) durationError()
        seconds = sumSeconds(x * 3600, seconds)
        state = 2
      } else if (ch == 'M' && state <= 2) {
        if (x < -153722867280912930L || x > 153722867280912930L) durationError()
        seconds = sumSeconds(x * 60, seconds)
        state = 3
      } else if (ch == '.') {
        pos += 1
        seconds = sumSeconds(x, seconds)
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
        if (ch != 'S') durationError()
        if (isNeg ^ isNegX) nanos = -nanos
        state = 4
      } else if (ch == 'S') {
        seconds = sumSeconds(x, seconds)
        state = 4
      } else durationError()
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
    val len                   = input.length
    var pos, year, month, day = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            ch4 != '-'
          } else {
            year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
            val yearNeg = ch0 == '-' || (ch0 != '+' && instantError())
            ch4 < '0' || ch4 > '9' || {
              var yearDigits = 4
              var ch         = '0'
              while (
                pos < len && {
                  ch = input.charAt(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && yearDigits < 10
                }
              ) {
                year =
                  if (year > 100000000) 2147483647
                  else year * 10 + (ch - '0')
                yearDigits += 1
              }
              yearDigits == 10 && year > 1000000000 || yearNeg && {
                year = -year
                year == 0
              } || ch != '-'
            }
          }
        }
      } || pos + 5 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        val ch5 = input.charAt(pos + 5)
        pos += 6
        month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
        ch4 < '0' || ch4 > '9' || ch5 != 'T' || month < 1 || month > 12 || day == 0 ||
        (day > 28 && day > maxDayForYearMonth(year, month))
      }
    ) instantError()
    val epochDay =
      epochDayForYear(year) + (dayOfYearForYearMonth(year, month) + day - 719529) // 719528 == days 0000 to 1970
    var epochSecond = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        val hour = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        epochSecond = hour * 3600 + (ch3 * 10 + ch4 - 528) * 60 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
        ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
      }
    ) instantError()
    var nano = 0
    var ch   = '0'
    if (pos < len) {
      ch = input.charAt(pos)
      pos += 1
      if (ch == ':') {
        if (
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            epochSecond += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          }
        ) instantError()
        if (pos < len) {
          ch = input.charAt(pos)
          pos += 1
          if (ch == '.') {
            var nanoDigitWeight = 100000000
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
      val offsetNeg = ch == '-' || (ch != '+' && instantError())
      if (
        pos + 1 >= len || {
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          pos += 2
          offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
        }
      ) instantError()
      if (
        pos < len && {
          ch = input.charAt(pos)
          pos += 1
          ch == ':'
        }
      ) {
        if (
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          }
        ) instantError()
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':'
          }
        ) {
          if (
            pos + 1 >= len || {
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              pos += 2
              offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) instantError()
        }
      }
      if (offsetTotal > 64800) instantError() // 64800 == 18 * 60 * 60
      if (offsetNeg) offsetTotal = -offsetTotal
    }
    if (pos != len) instantError()
    Instant.ofEpochSecond(epochDay * 86400 + (epochSecond - offsetTotal), nano.toLong)
  }

  def unsafeParseLocalDate(input: String): LocalDate = {
    val len                   = input.length
    var pos, year, month, day = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            ch4 != '-'
          } else {
            year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
            val yearNeg = ch0 == '-' || (ch0 != '+' && localDateError())
            ch4 < '0' || ch4 > '9' || {
              var yearDigits = 4
              var ch         = '0'
              while (
                pos < len && {
                  ch = input.charAt(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && yearDigits < 9
                }
              ) {
                year = year * 10 + (ch - '0')
                yearDigits += 1
              }
              yearNeg && {
                year = -year
                year == 0
              } || ch != '-'
            }
          }
        }
      } || pos + 5 != len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
        ch4 < '0' || ch4 > '9' || month < 1 || month > 12 || day == 0 ||
        (day > 28 && day > maxDayForYearMonth(year, month))
      }
    ) localDateError()
    LocalDate.of(year, month, day)
  }

  def unsafeParseLocalDateTime(input: String): LocalDateTime = {
    val len                   = input.length
    var pos, year, month, day = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            ch4 != '-'
          } else {
            year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
            val yearNeg = ch0 == '-' || (ch0 != '+' && localDateTimeError())
            ch4 < '0' || ch4 > '9' || {
              var yearDigits = 4
              var ch         = '0'
              while (
                pos < len && {
                  ch = input.charAt(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && yearDigits < 9
                }
              ) {
                year = year * 10 + (ch - '0')
                yearDigits += 1
              }
              yearNeg && {
                year = -year
                year == 0
              } || ch != '-'
            }
          }
        }
      } || pos + 5 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        val ch5 = input.charAt(pos + 5)
        pos += 6
        month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
        ch4 < '0' || ch4 > '9' || ch5 != 'T' || day == 0 || month < 1 || month > 12 ||
        (day > 28 && day > maxDayForYearMonth(year, month))
      }
    ) localDateTimeError()
    var hour, minute = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
        minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
        ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
      }
    ) localDateTimeError()
    var second, nano = 0
    if (pos < len) {
      if (
        input.charAt(pos) != ':' || {
          pos += 1
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          }
        }
      ) localDateTimeError()
      if (pos < len) {
        if (
          input.charAt(pos) != '.' || {
            pos += 1
            var nanoDigitWeight = 100000000
            var ch              = '0'
            while (
              pos < len && {
                ch = input.charAt(pos)
                ch >= '0' && ch <= '9' && nanoDigitWeight != 0
              }
            ) {
              nano += (ch - '0') * nanoDigitWeight
              nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
              pos += 1
            }
            pos != len
          }
        ) localDateTimeError()
      }
    }
    LocalDateTime.of(year, month, day, hour, minute, second, nano)
  }

  def unsafeParseLocalTime(input: String): LocalTime = {
    val len               = input.length
    var pos, hour, minute = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
        minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
        ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
      }
    ) localTimeError()
    var second, nano = 0
    if (pos < len) {
      if (
        input.charAt(pos) != ':' || {
          pos += 1
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          }
        }
      ) localTimeError()
      if (pos < len) {
        if (
          input.charAt(pos) != '.' || {
            pos += 1
            var nanoDigitWeight = 100000000
            var ch              = '0'
            while (
              pos < len && {
                ch = input.charAt(pos)
                ch >= '0' && ch <= '9' && nanoDigitWeight != 0
              }
            ) {
              nano += (ch - '0') * nanoDigitWeight
              nanoDigitWeight = (nanoDigitWeight * 3435973837L >> 35).toInt // divide a positive int by 10
              pos += 1
            }
            pos != len
          }
        ) localTimeError()
      }
    }
    LocalTime.of(hour, minute, second, nano)
  }

  def unsafeParseMonthDay(input: String): MonthDay = {
    var month, day = 0
    if (
      input.length != 7 || {
        val ch0 = input.charAt(0)
        val ch1 = input.charAt(1)
        val ch2 = input.charAt(2)
        val ch3 = input.charAt(3)
        val ch4 = input.charAt(4)
        val ch5 = input.charAt(5)
        val ch6 = input.charAt(6)
        month = ch2 * 10 + ch3 - 528 // 528 == '0' * 11
        day = ch5 * 10 + ch6 - 528   // 528 == '0' * 11
        ch0 != '-' || ch1 != '-' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || ch4 != '-' ||
        ch5 < '0' || ch5 > '9' || ch6 < '0' || ch6 > '9' || month < 1 || month > 12 || day == 0 ||
        (day > 28 && day > maxDayForMonth(month))
      }
    ) monthDayError()
    MonthDay.of(month, day)
  }

  def unsafeParseOffsetDateTime(input: String): OffsetDateTime = {
    val len                   = input.length
    var pos, year, month, day = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            ch4 != '-'
          } else {
            year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
            val yearNeg = ch0 == '-' || (ch0 != '+' && offsetDateTimeError())
            ch4 < '0' || ch4 > '9' || {
              var yearDigits = 4
              var ch         = '0'
              while (
                pos < len && {
                  ch = input.charAt(pos)
                  pos += 1
                  ch >= '0' && ch <= '9' && yearDigits < 9
                }
              ) {
                year = year * 10 + (ch - '0')
                yearDigits += 1
              }
              yearNeg && {
                year = -year
                year == 0
              } || ch != '-'
            }
          }
        }
      } || pos + 5 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        val ch5 = input.charAt(pos + 5)
        pos += 6
        month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
        ch4 < '0' || ch4 > '9' || ch5 != 'T' || month < 1 || month > 12 || day == 0 ||
        (day > 28 && day > maxDayForYearMonth(year, month))
      }
    ) offsetDateTimeError()
    var hour, minute = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
        minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
        ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
      } || pos >= len
    ) offsetDateTimeError()
    var second, nano = 0
    var ch           = input.charAt(pos)
    pos += 1
    if (ch == ':') {
      if (
        pos + 1 >= len || {
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          pos += 2
          second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
        } || pos >= len
      ) offsetDateTimeError()
      ch = input.charAt(pos)
      pos += 1
      if (ch == '.') {
        var nanoDigitWeight = 100000000
        while ({
          if (pos >= len) offsetDateTimeError()
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
        val offsetNeg   = ch == '-' || (ch != '+' && offsetDateTimeError())
        var offsetTotal = 0
        if (
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
          }
        ) offsetDateTimeError()
        if (pos < len) {
          if (
            input.charAt(pos) != ':' || {
              pos += 1
              pos + 1 >= len
            } || {
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              pos += 2
              offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) offsetDateTimeError()
          if (pos < len) {
            if (
              input.charAt(pos) != ':' || {
                pos += 1
                pos + 1 >= len
              } || {
                val ch0 = input.charAt(pos)
                val ch1 = input.charAt(pos + 1)
                pos += 2
                offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) offsetDateTimeError()
          }
        }
        if (offsetTotal > 64800) offsetDateTimeError()
        toZoneOffset(offsetNeg, offsetTotal)
      }
    if (pos != len) offsetDateTimeError()
    OffsetDateTime.of(year, month, day, hour, minute, second, nano, zoneOffset)
  }

  def unsafeParseOffsetTime(input: String): OffsetTime = {
    val len               = input.length
    var pos, hour, minute = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
        minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
        ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
      } || pos >= len
    ) offsetTimeError()
    var second, nano = 0
    var ch           = input.charAt(pos)
    pos += 1
    if (ch == ':') {
      if (
        pos + 1 >= len || {
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          pos += 2
          second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
        } || pos >= len
      ) offsetTimeError()
      ch = input.charAt(pos)
      pos += 1
      if (ch == '.') {
        var nanoDigitWeight = 100000000
        while ({
          if (pos >= len) offsetTimeError()
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
        val offsetNeg   = ch == '-' || (ch != '+' && offsetTimeError())
        var offsetTotal = 0
        if (
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
          }
        ) offsetTimeError()
        if (pos < len) {
          if (
            input.charAt(pos) != ':' || {
              pos += 1
              pos + 1 >= len
            } || {
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              pos += 2
              offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) offsetTimeError()
          if (pos < len) {
            if (
              input.charAt(pos) != ':' || {
                pos += 1
                pos + 1 >= len
              } || {
                val ch0 = input.charAt(pos)
                val ch1 = input.charAt(pos + 1)
                pos += 2
                offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) offsetTimeError()
          }
        }
        if (offsetTotal > 64800) offsetTimeError()
        toZoneOffset(offsetNeg, offsetTotal)
      }
    if (pos != len) offsetTimeError()
    OffsetTime.of(hour, minute, second, nano, zoneOffset)
  }

  def unsafeParsePeriod(input: String): Period = {
    val len                             = input.length
    var pos, state, years, months, days = 0
    if (pos >= len) periodError()
    var ch = input.charAt(pos)
    pos += 1
    val isNeg = ch == '-'
    if (isNeg) {
      if (pos >= len) periodError()
      ch = input.charAt(pos)
      pos += 1
    }
    if (ch != 'P' || pos >= len) periodError()
    ch = input.charAt(pos)
    pos += 1
    while ({
      if (state == 4 && pos >= len) periodError()
      val isNegX = ch == '-'
      if (isNegX) {
        if (pos >= len) periodError()
        ch = input.charAt(pos)
        pos += 1
      }
      if (ch < '0' || ch > '9') periodError()
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
        ) periodError()
        pos += 1
      }
      if (!(isNeg ^ isNegX)) {
        if (x == -2147483648) periodError()
        x = -x
      }
      if (ch == 'Y' && state <= 0) {
        years = x
        state = 1
      } else if (ch == 'M' && state <= 1) {
        months = x
        state = 2
      } else if (ch == 'W' && state <= 2) {
        if (x < -306783378 || x > 306783378) periodError()
        days = x * 7
        state = 3
      } else if (ch == 'D') {
        val ds = x.toLong + days
        if (ds != ds.toInt) periodError()
        days = ds.toInt
        state = 4
      } else periodError()
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
    val len       = input.length
    var pos, year = 0
    if (
      pos + 3 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        pos += 4
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            pos != len
          } else {
            val yearNeg = ch0 == '-' || (ch0 != '+' && yearError())
            year = ch1 * 100 + ch2 * 10 + ch3 - 5328 // 53328 == '0' * 111
            var yearDigits = 3
            var ch         = '0'
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
            yearNeg && {
              year = -year
              year == 0
            } || pos != len
          }
        }
      }
    ) yearError()
    Year.of(year)
  }

  def unsafeParseYearMonth(input: String): YearMonth = {
    val len              = input.length
    var pos, year, month = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            ch4 != '-'
          } else {
            val yearNeg = ch0 == '-' || (ch0 != '+' && yearMonthError())
            year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
            ch4 < '0' || ch4 > '9' || {
              var yearDigits = 4
              var ch         = '0'
              while ({
                if (pos >= len) yearMonthError()
                ch = input.charAt(pos)
                pos += 1
                ch >= '0' && ch <= '9' && yearDigits < 9
              }) {
                year = year * 10 + (ch - '0')
                yearDigits += 1
              }
              yearNeg && {
                year = -year
                year == 0
              } || ch != '-'
            }
          }
        }
      } || pos + 2 != len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        pos += 2
        month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || month < 1 || month > 12
      }
    ) yearMonthError()
    YearMonth.of(year, month)
  }

  def unsafeParseZonedDateTime(input: String): ZonedDateTime = {
    val len                                 = input.length
    var pos, year, month, day, hour, minute = 0
    if (
      pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        ch1 < '0' || ch1 > '9' || ch2 < '0' || ch2 > '9' || ch3 < '0' || ch3 > '9' || {
          if (ch0 >= '0' && ch0 <= '9') {
            year = ch0 * 1000 + ch1 * 100 + ch2 * 10 + ch3 - 53328 // 53328 == '0' * 1111
            ch4 != '-'
          } else {
            val yearNeg = ch0 == '-' || (ch0 != '+' && zonedDateTimeError())
            year = ch1 * 1000 + ch2 * 100 + ch3 * 10 + ch4 - 53328 // 53328 == '0' * 1111
            ch4 < '0' || ch4 > '9' || {
              var yearDigits = 4
              var ch         = '0'
              while ({
                if (pos >= len) zonedDateTimeError()
                ch = input.charAt(pos)
                pos += 1
                ch >= '0' && ch <= '9' && yearDigits < 9
              }) {
                year = year * 10 + (ch - '0')
                yearDigits += 1
              }
              yearNeg && {
                year = -year
                year == 0
              } || ch != '-'
            }
          }
        }
      } || pos + 5 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        val ch5 = input.charAt(pos + 5)
        pos += 6
        month = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
        day = ch3 * 10 + ch4 - 528   // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != '-' || ch3 < '0' || ch3 > '9' ||
        ch4 < '0' || ch4 > '9' || ch5 != 'T' || month < 1 || month > 12 || day == 0 ||
        (day > 28 && day > maxDayForYearMonth(year, month))
      } || pos + 4 >= len || {
        val ch0 = input.charAt(pos)
        val ch1 = input.charAt(pos + 1)
        val ch2 = input.charAt(pos + 2)
        val ch3 = input.charAt(pos + 3)
        val ch4 = input.charAt(pos + 4)
        pos += 5
        hour = ch0 * 10 + ch1 - 528   // 528 == '0' * 11
        minute = ch3 * 10 + ch4 - 528 // 528 == '0' * 11
        ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch2 != ':' ||
        ch3 < '0' || ch3 > '9' || ch4 < '0' || ch4 > '9' || ch3 > '5' || hour > 23
      } || pos >= len
    ) zonedDateTimeError()
    var second, nano = 0
    var ch           = input.charAt(pos)
    pos += 1
    if (ch == ':') {
      if (
        pos + 1 >= len || {
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          pos += 2
          second = ch0 * 10 + ch1 - 528 // 528 == '0' * 11
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
        } || pos >= len
      ) zonedDateTimeError()
      ch = input.charAt(pos)
      pos += 1
      if (ch == '.') {
        var nanoDigitWeight = 100000000
        while ({
          if (pos >= len) zonedDateTimeError()
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
          if (ch != '[') zonedDateTimeError()
          pos += 1
        }
        ZoneOffset.UTC
      } else {
        val offsetNeg   = ch == '-' || (ch != '+' && zonedDateTimeError())
        var offsetTotal = 0
        if (
          pos + 1 >= len || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
          }
        ) zonedDateTimeError()
        if (
          pos < len && {
            ch = input.charAt(pos)
            pos += 1
            ch == ':' || ch != '[' && zonedDateTimeError()
          }
        ) {
          if (
            pos + 1 >= len || {
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              pos += 2
              offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) zonedDateTimeError()
          if (
            pos < len && {
              ch = input.charAt(pos)
              pos += 1
              ch == ':' || ch != '[' && zonedDateTimeError()
            }
          ) {
            if (
              pos + 1 >= len || {
                val ch0 = input.charAt(pos)
                val ch1 = input.charAt(pos + 1)
                pos += 2
                offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
                ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
              }
            ) zonedDateTimeError()
            if (pos < len) {
              ch = input.charAt(pos)
              if (ch != '[') zonedDateTimeError()
              pos += 1
            }
          }
        }
        if (offsetTotal > 64800) zonedDateTimeError()
        toZoneOffset(offsetNeg, offsetTotal)
      }
    if (ch == '[') {
      var zoneId: ZoneId = null
      val from           = pos
      while ({
        if (pos >= len) zonedDateTimeError()
        ch = input.charAt(pos)
        ch != ']'
      }) pos += 1
      val key = input.substring(from, pos)
      zoneId = zoneIds.get(key)
      if (
        (zoneId eq null) && {
          try zoneId = ZoneId.of(key)
          catch {
            case _: DateTimeException => zonedDateTimeError()
          }
          !zoneId.isInstanceOf[ZoneOffset] || zoneId.asInstanceOf[ZoneOffset].getTotalSeconds % 900 == 0
        }
      ) zoneIds.put(key, zoneId)
      if (pos + 1 != len) zonedDateTimeError()
      ZonedDateTime.ofInstant(localDateTime, zoneOffset, zoneId)
    } else {
      if (pos != len) zonedDateTimeError()
      ZonedDateTime.ofLocal(localDateTime, zoneOffset, null)
    }
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
      case _: DateTimeException => zoneIdError()
    }

  def unsafeParseZoneOffset(input: String): ZoneOffset = {
    val len = input.length
    var pos = 0
    if (pos >= len) zoneOffsetError()
    val ch = input.charAt(pos)
    pos += 1
    if (ch == 'Z') {
      if (pos != len) zoneOffsetError()
      ZoneOffset.UTC
    } else {
      val offsetNeg   = ch == '-' || (ch != '+' && zoneOffsetError())
      var offsetTotal = 0
      if (
        pos + 1 >= len || {
          val ch0 = input.charAt(pos)
          val ch1 = input.charAt(pos + 1)
          offsetTotal = (ch0 * 10 + ch1 - 528) * 3600 // 528 == '0' * 11
          pos += 2
          ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9'
        }
      ) zoneOffsetError()
      if (pos < len) {
        if (
          input.charAt(pos) != ':' || {
            pos += 1
            pos + 1 >= len
          } || {
            val ch0 = input.charAt(pos)
            val ch1 = input.charAt(pos + 1)
            pos += 2
            offsetTotal += (ch0 * 10 + ch1 - 528) * 60 // 528 == '0' * 11
            ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
          }
        ) zoneOffsetError()
        if (pos < len) {
          if (
            input.charAt(pos) != ':' || {
              pos += 1
              pos + 1 >= len
            } || {
              val ch0 = input.charAt(pos)
              val ch1 = input.charAt(pos + 1)
              pos += 2
              offsetTotal += ch0 * 10 + ch1 - 528 // 528 == '0' * 11
              ch0 < '0' || ch0 > '9' || ch1 < '0' || ch1 > '9' || ch0 > '5'
            }
          ) zoneOffsetError()
        }
      }
      if (offsetTotal > 64800 || pos != len) zoneOffsetError() // 64800 == 18 * 60 * 60
      toZoneOffset(offsetNeg, offsetTotal)
    }
  }

  private[this] def toZoneOffset(offsetNeg: Boolean, offsetTotal: Int): ZoneOffset = {
    var qp = offsetTotal * 37283
    if ((qp & 0x1ff8000) == 0) { // check if offsetTotal divisible by 900
      qp >>>= 25                 // divide offsetTotal by 900
      if (offsetNeg) qp = -qp
      var zoneOffset = zoneOffsets(qp + 72)
      if (zoneOffset ne null) zoneOffset
      else {
        zoneOffset = ZoneOffset.ofTotalSeconds(if (offsetNeg) -offsetTotal else offsetTotal)
        zoneOffsets(qp + 72) = zoneOffset
        zoneOffset
      }
    } else ZoneOffset.ofTotalSeconds(if (offsetNeg) -offsetTotal else offsetTotal)
  }

  private[this] def sumSeconds(s1: Long, s2: Long): Long = {
    val s = s1 + s2
    if (((s1 ^ s) & (s2 ^ s)) < 0) durationError()
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

  @noinline private[this] def durationError() = error("expected a Duration")

  @noinline private[this] def instantError() = error("expected an Instant")

  @noinline private[this] def localDateError() = error("expected a LocalDate")

  @noinline private[this] def localDateTimeError() = error("expected a LocalDateTime")

  @noinline private[this] def localTimeError() = error("expected a LocalTime")

  @noinline private[this] def offsetDateTimeError() = error("expected an OffsetDateTime")

  @noinline private[this] def offsetTimeError() = error("expected an OffsetTime")

  @noinline private[this] def periodError() = error("expected a Period")

  @noinline private[this] def monthDayError() = error("expected a MonthDay")

  @noinline private[this] def yearMonthError() = error("expected a YearMonth")

  @noinline private[this] def zonedDateTimeError() = error("expected a ZonedDateTime")

  @noinline private[this] def zoneOffsetError() = error("expected a ZoneOffset")

  @noinline private[this] def zoneIdError() = error("expected a ZoneId")

  @noinline private[this] def yearError() = error("expected a Year")

  private[this] def error(msg: String): Nothing = throw new DateTimeException(msg) with NoStackTrace
}
