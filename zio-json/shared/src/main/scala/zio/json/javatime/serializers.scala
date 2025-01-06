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

private[json] object serializers {
  def toString(x: Duration): String = {
    val s = new java.lang.StringBuilder(16)
    s.append('P').append('T')
    val totalSecs = x.getSeconds
    var nano      = x.getNano
    if ((totalSecs | nano) == 0) s.append('0').append('S')
    else {
      var effectiveTotalSecs = totalSecs
      if (totalSecs < 0 && nano > 0) effectiveTotalSecs += 1
      val hours      = effectiveTotalSecs / 3600 // 3600 == seconds in a hour
      val secsOfHour = (effectiveTotalSecs - hours * 3600).toInt
      val minutes    = secsOfHour / 60
      val seconds    = secsOfHour - minutes * 60
      if (hours != 0) s.append(hours).append('H')
      if (minutes != 0) s.append(minutes).append('M')
      if ((seconds | nano) != 0) {
        if (totalSecs < 0 && seconds == 0) s.append('-').append('0')
        else s.append(seconds)
        if (nano != 0) {
          if (totalSecs < 0) nano = 1000000000 - nano
          val dotPos = s.length
          s.append(nano + 1000000000)
          var i = s.length - 1
          while (s.charAt(i) == '0') i -= 1
          s.setLength(i + 1)
          s.setCharAt(dotPos, '.')
        }
        s.append('S')
      }
    }
    s.toString
  }

  def toString(x: Instant): String = {
    val s           = new java.lang.StringBuilder(32)
    val epochSecond = x.getEpochSecond
    val epochDay =
      (if (epochSecond >= 0) epochSecond
       else epochSecond - 86399) / 86400 // 86400 == seconds per day
    val secsOfDay             = (epochSecond - epochDay * 86400).toInt
    var marchZeroDay          = epochDay + 719468 // 719468 == 719528 - 60 == days 0000 to 1970 - days 1st Jan to 1st Mar
    var adjustYear            = 0
    if (marchZeroDay < 0) { // adjust negative years to positive for calculation
      val adjust400YearCycles = to400YearCycle(marchZeroDay + 1) - 1
      adjustYear = adjust400YearCycles * 400
      marchZeroDay -= adjust400YearCycles * 146097L
    }
    var year           = to400YearCycle(marchZeroDay * 400 + 591)
    var marchDayOfYear = toMarchDayOfYear(marchZeroDay, year)
    if (marchDayOfYear < 0) { // fix year estimate
      year -= 1
      marchDayOfYear = toMarchDayOfYear(marchZeroDay, year)
    }
    val marchMonth = (marchDayOfYear * 17135 + 6854) >> 19 // (marchDayOfYear * 5 + 2) / 153
    year += (marchMonth * 3277 >> 15) + adjustYear // year += marchMonth / 10 + adjustYear (reset any negative year and convert march-based values back to january-based)
    val month = marchMonth +
      (if (marchMonth < 10) 3
       else -9)
    val day =
      marchDayOfYear - ((marchMonth * 1002762 - 16383) >> 15) // marchDayOfYear - (marchMonth * 306 + 5) / 10 + 1
    val hour       = secsOfDay * 37283 >>> 27 // divide a small positive int by 3600
    val secsOfHour = secsOfDay - hour * 3600
    val minute     = secsOfHour * 17477 >> 20 // divide a small positive int by 60
    val second     = secsOfHour - minute * 60
    appendYear(year, s)
    append2Digits(month, s.append('-'))
    append2Digits(day, s.append('-'))
    append2Digits(hour, s.append('T'))
    append2Digits(minute, s.append(':'))
    append2Digits(second, s.append(':'))
    val nano = x.getNano
    if (nano != 0) {
      s.append('.')
      val q1 = nano / 1000000
      val r1 = nano - q1 * 1000000
      append3Digits(q1, s)
      if (r1 != 0) {
        val q2 = r1 / 1000
        val r2 = r1 - q2 * 1000
        append3Digits(q2, s)
        if (r2 != 0) append3Digits(r2, s)
      }
    }
    s.append('Z').toString
  }

  def toString(x: LocalDate): String = {
    val s = new java.lang.StringBuilder(16)
    appendLocalDate(x, s)
    s.toString
  }

  def toString(x: LocalDateTime): String = {
    val s = new java.lang.StringBuilder(32)
    appendLocalDate(x.toLocalDate, s)
    appendLocalTime(x.toLocalTime, s.append('T'))
    s.toString
  }

  def toString(x: LocalTime): String = {
    val s = new java.lang.StringBuilder(24)
    appendLocalTime(x, s)
    s.toString
  }

  def toString(x: MonthDay): String = {
    val s = new java.lang.StringBuilder(8)
    append2Digits(x.getMonthValue, s.append('-').append('-'))
    append2Digits(x.getDayOfMonth, s.append('-'))
    s.toString
  }

  def toString(x: OffsetDateTime): String = {
    val s = new java.lang.StringBuilder(48)
    appendLocalDate(x.toLocalDate, s)
    appendLocalTime(x.toLocalTime, s.append('T'))
    appendZoneOffset(x.getOffset, s)
    s.toString
  }

  def toString(x: OffsetTime): String = {
    val s = new java.lang.StringBuilder(32)
    appendLocalTime(x.toLocalTime, s)
    appendZoneOffset(x.getOffset, s)
    s.toString
  }

  def toString(x: Period): String = {
    val s = new java.lang.StringBuilder(16)
    s.append('P')
    if (x.isZero) s.append('0').append('D')
    else {
      val years  = x.getYears
      val months = x.getMonths
      val days   = x.getDays
      if (years != 0) s.append(years).append('Y')
      if (months != 0) s.append(months).append('M')
      if (days != 0) s.append(days).append('D')
    }
    s.toString
  }

  def toString(x: Year): String = {
    val s = new java.lang.StringBuilder(16)
    appendYear(x.getValue, s)
    s.toString
  }

  def toString(x: YearMonth): String = {
    val s = new java.lang.StringBuilder(16)
    appendYear(x.getYear, s)
    append2Digits(x.getMonthValue, s.append('-'))
    s.toString
  }

  def toString(x: ZonedDateTime): String = {
    val s = new java.lang.StringBuilder(48)
    appendLocalDate(x.toLocalDate, s)
    appendLocalTime(x.toLocalTime, s.append('T'))
    appendZoneOffset(x.getOffset, s)
    val zone = x.getZone
    if (!zone.isInstanceOf[ZoneOffset]) s.append('[').append(zone.getId).append(']')
    s.toString
  }

  def toString(x: ZoneId): String = x.getId

  def toString(x: ZoneOffset): String = {
    val s = new java.lang.StringBuilder(16)
    appendZoneOffset(x, s)
    s.toString
  }

  private[this] def appendLocalDate(x: LocalDate, s: java.lang.StringBuilder): Unit = {
    appendYear(x.getYear, s)
    append2Digits(x.getMonthValue, s.append('-'))
    append2Digits(x.getDayOfMonth, s.append('-'))
  }

  private[this] def appendLocalTime(x: LocalTime, s: java.lang.StringBuilder): Unit = {
    append2Digits(x.getHour, s)
    append2Digits(x.getMinute, s.append(':'))
    append2Digits(x.getSecond, s.append(':'))
    val nano = x.getNano
    if (nano != 0) {
      val dotPos = s.length
      s.append(nano + 1000000000)
      var i = s.length - 1
      while (s.charAt(i) == '0') i -= 1
      s.setLength(i + 1)
      s.setCharAt(dotPos, '.')
    }
  }

  private[this] def appendZoneOffset(x: ZoneOffset, s: java.lang.StringBuilder): Unit = {
    val totalSeconds = x.getTotalSeconds
    if (totalSeconds == 0) s.append('Z'): Unit
    else {
      val q0 =
        if (totalSeconds > 0) {
          s.append('+')
          totalSeconds
        } else {
          s.append('-')
          -totalSeconds
        }
      val q1 = q0 * 37283 >>> 27 // divide a small positive int by 3600
      val r1 = q0 - q1 * 3600
      append2Digits(q1, s)
      s.append(':')
      val q2 = r1 * 17477 >> 20 // divide a small positive int by 60
      val r2 = r1 - q2 * 60
      append2Digits(q2, s)
      if (r2 != 0) append2Digits(r2, s.append(':'))
    }
  }

  private[this] def appendYear(x: Int, s: java.lang.StringBuilder): Unit =
    if (x >= 0) {
      if (x < 10000) append4Digits(x, s)
      else s.append('+').append(x): Unit
    } else if (x > -10000) append4Digits(-x, s.append('-'))
    else s.append(x): Unit

  private[this] def append4Digits(x: Int, s: java.lang.StringBuilder): Unit = {
    val q = x * 5243 >> 19 // divide a 4-digit positive int by 100
    append2Digits(q, s)
    append2Digits(x - q * 100, s)
  }

  private[this] def append3Digits(x: Int, s: java.lang.StringBuilder): Unit = {
    val q = x * 1311 >> 17 // divide a 3-digit positive int by 100
    append2Digits(x - q * 100, s.append((q + '0').toChar))
  }

  private[this] def append2Digits(x: Int, s: java.lang.StringBuilder): Unit = {
    val q = x * 103 >> 10 // divide a 2-digit positive int by 10
    s.append((q + '0').toChar).append((x + '0' - q * 10).toChar): Unit
  }

  private[this] def to400YearCycle(day: Long): Int =
    (day / 146097).toInt // 146097 == number of days in a 400 year cycle

  private[this] def toMarchDayOfYear(marchZeroDay: Long, year: Int): Int = {
    val century = year / 100
    (marchZeroDay - year * 365L).toInt - (year >> 2) + century - (century >> 2)
  }
}
