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

import zio.json.internal.{ FastStringWrite, SafeNumbers, Write }

import java.time._

private[json] object serializers {
  def toString(x: Duration): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: Duration, out: Write): Unit = {
    out.write('P', 'T')
    val totalSecs = x.getSeconds
    var nano      = x.getNano
    if ((totalSecs | nano) == 0) out.write('0', 'S')
    else {
      var effectiveTotalSecs = totalSecs
      if (totalSecs < 0 && nano > 0) effectiveTotalSecs += 1
      val hours      = effectiveTotalSecs / 3600 // 3600 == seconds in a hour
      val secsOfHour = (effectiveTotalSecs - hours * 3600).toInt
      val minutes    = secsOfHour / 60
      val seconds    = secsOfHour - minutes * 60
      if (hours != 0) {
        SafeNumbers.write(hours, out)
        out.write('H')
      }
      if (minutes != 0) {
        SafeNumbers.write(minutes, out)
        out.write('M')
      }
      if ((seconds | nano) != 0) {
        if (totalSecs < 0 && seconds == 0) out.write('-', '0')
        else SafeNumbers.write(seconds, out)
        if (nano != 0) {
          if (totalSecs < 0) nano = 1000000000 - nano
          SafeNumbers.writeNano(nano, out)
        }
        out.write('S')
      }
    }
  }

  def toString(x: Instant): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: Instant, out: Write): Unit = {
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
    writeYear(year, out)
    out.write('-')
    SafeNumbers.write2Digits(month, out)
    out.write('-')
    SafeNumbers.write2Digits(day, out)
    out.write('T')
    SafeNumbers.write2Digits(hour, out)
    out.write(':')
    SafeNumbers.write2Digits(minute, out)
    out.write(':')
    SafeNumbers.write2Digits(second, out)
    val nano = x.getNano
    if (nano != 0) {
      out.write('.')
      val q1 = nano / 1000000
      val r1 = nano - q1 * 1000000
      SafeNumbers.write3Digits(q1, out)
      if (r1 != 0) {
        val q2 = r1 / 1000
        val r2 = r1 - q2 * 1000
        SafeNumbers.write3Digits(q2, out)
        if (r2 != 0) SafeNumbers.write3Digits(r2, out)
      }
    }
    out.write('Z')
  }

  def toString(x: LocalDate): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: LocalDate, out: Write): Unit = {
    writeYear(x.getYear, out)
    out.write('-')
    SafeNumbers.write2Digits(x.getMonthValue, out)
    out.write('-')
    SafeNumbers.write2Digits(x.getDayOfMonth, out)
  }

  def toString(x: LocalDateTime): String = {
    val out = writes.get
    write(x, out)
    write(x.toLocalDate, out)
    out.buffer.toString
  }

  def write(x: LocalDateTime, out: Write): Unit = {
    write(x.toLocalDate, out)
    out.write('T')
    write(x.toLocalTime, out)
  }

  def toString(x: LocalTime): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: LocalTime, out: Write): Unit = {
    SafeNumbers.write2Digits(x.getHour, out)
    out.write(':')
    SafeNumbers.write2Digits(x.getMinute, out)
    out.write(':')
    SafeNumbers.write2Digits(x.getSecond, out)
    val nano = x.getNano
    if (nano != 0) SafeNumbers.writeNano(nano, out)
  }

  def toString(x: MonthDay): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: MonthDay, out: Write): Unit = {
    out.write('-', '-')
    SafeNumbers.write2Digits(x.getMonthValue, out)
    out.write('-')
    SafeNumbers.write2Digits(x.getDayOfMonth, out)
  }

  def toString(x: OffsetDateTime): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: OffsetDateTime, out: Write): Unit = {
    write(x.toLocalDate, out)
    out.write('T')
    write(x.toLocalTime, out)
    write(x.getOffset, out)
  }

  def toString(x: OffsetTime): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: OffsetTime, out: Write): Unit = {
    write(x.toLocalTime, out)
    write(x.getOffset, out)
  }

  def toString(x: Period): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: Period, out: Write): Unit = {
    out.write('P')
    if (x.isZero) out.write('0', 'D')
    else {
      val years  = x.getYears
      val months = x.getMonths
      val days   = x.getDays
      if (years != 0) {
        SafeNumbers.write(years, out)
        out.write('Y')
      }
      if (months != 0) {
        SafeNumbers.write(months, out)
        out.write('M')
      }
      if (days != 0) {
        SafeNumbers.write(days, out)
        out.write('D')
      }
    }
  }

  def toString(x: Year): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  @inline def write(x: Year, out: Write): Unit = writeYear(x.getValue, out)

  def toString(x: YearMonth): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: YearMonth, out: Write): Unit = {
    writeYear(x.getYear, out)
    out.write('-')
    SafeNumbers.write2Digits(x.getMonthValue, out)
  }

  def toString(x: ZonedDateTime): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: ZonedDateTime, out: Write): Unit = {
    write(x.toLocalDate, out)
    out.write('T')
    write(x.toLocalTime, out)
    write(x.getOffset, out)
    val zone = x.getZone
    if (!zone.isInstanceOf[ZoneOffset]) {
      out.write('[')
      out.write(zone.getId)
      out.write(']')
    }
  }

  @inline def toString(x: ZoneId): String = x.getId

  @inline def write(x: ZoneId, out: Write): Unit = out.write(x.getId)

  def toString(x: ZoneOffset): String = {
    val out = writes.get
    write(x, out)
    out.buffer.toString
  }

  def write(x: ZoneOffset, out: Write): Unit = {
    val totalSeconds = x.getTotalSeconds
    if (totalSeconds == 0) out.write('Z'): Unit
    else {
      val q0 =
        if (totalSeconds > 0) {
          out.write('+')
          totalSeconds
        } else {
          out.write('-')
          -totalSeconds
        }
      val q1 = q0 * 37283 >>> 27 // divide a small positive int by 3600
      val r1 = q0 - q1 * 3600
      SafeNumbers.write2Digits(q1, out)
      out.write(':')
      val q2 = r1 * 17477 >> 20 // divide a small positive int by 60
      val r2 = r1 - q2 * 60
      SafeNumbers.write2Digits(q2, out)
      if (r2 != 0) {
        out.write(':')
        SafeNumbers.write2Digits(r2, out)
      }
    }
  }

  private[this] def writeYear(x: Int, out: Write): Unit =
    if (x >= 0) {
      if (x < 10000) SafeNumbers.write4Digits(x, out)
      else {
        out.write('+')
        SafeNumbers.write(x, out): Unit
      }
    } else if (x > -10000) {
      out.write('-')
      SafeNumbers.write4Digits(-x, out)
    } else SafeNumbers.write(x, out): Unit

  @inline private[this] def to400YearCycle(day: Long): Int =
    (day / 146097).toInt // 146097 == number of days in a 400 year cycle

  @inline private[this] def toMarchDayOfYear(marchZeroDay: Long, year: Int): Int = {
    val century = year / 100
    (marchZeroDay - year * 365L).toInt - (year >> 2) + century - (century >> 2)
  }

  private[this] val writes = new ThreadLocal[FastStringWrite] {
    override def initialValue(): FastStringWrite = new FastStringWrite(64)

    override def get: FastStringWrite = {
      val w = super.get
      w.reset()
      w
    }
  }
}
