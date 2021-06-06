package zio.json.uuid

import scala.annotation.nowarn

// A port of https://github.com/openjdk/jdk/commit/ebadfaeb2e1cc7b5ce5f101cd8a539bc5478cf5b with optimizations applied
private[json] object UUIDParser {
  // Converts characters to their numeric representation (for example 'E' or 'e' becomes 0XE)
  private val CharToNumeric: Array[Byte] = {
    // by filling in -1's we prevent parseLong from trying to parse invalid characters
    val ns = Array.fill[Byte](256)(-1)

    ns('0') = 0
    ns('1') = 1
    ns('2') = 2
    ns('3') = 3
    ns('4') = 4
    ns('5') = 5
    ns('6') = 6
    ns('7') = 7
    ns('8') = 8
    ns('9') = 9

    ns('A') = 10
    ns('B') = 11
    ns('C') = 12
    ns('D') = 13
    ns('E') = 14
    ns('F') = 15

    ns('a') = 10
    ns('b') = 11
    ns('c') = 12
    ns('d') = 13
    ns('e') = 14
    ns('f') = 15

    ns
  }

  // Used to detect the presence of extra bits
  private val mask8Z  = 0xfffffff00000000L
  private val mask4Z  = 0xfffffffffff0000L
  private val mask12Z = 0xfff000000000000L

  def unsafeParse(input: String): java.util.UUID =
    if (input.length == 36) {
      val ch1 = input.charAt(8)
      val ch2 = input.charAt(13)
      val ch3 = input.charAt(18)
      val ch4 = input.charAt(23)

      if (ch1 == '-' && ch2 == '-' && ch3 == '-' && ch4 == '-') {
        val msb1 = parseNibbles(input, 0)
        val msb2 = parseNibbles(input, 4)
        val msb3 = parseNibbles(input, 9)
        val msb4 = parseNibbles(input, 14)
        val lsb1 = parseNibbles(input, 19)
        val lsb2 = parseNibbles(input, 24)
        val lsb3 = parseNibbles(input, 28)
        val lsb4 = parseNibbles(input, 32)
        if ((msb1 | msb2 | msb3 | msb4 | lsb1 | lsb2 | lsb3 | lsb4) >= 0) {
          new java.util.UUID(
            msb1 << 48 | msb2 << 32 | msb3 << 16 | msb4,
            lsb1 << 48 | lsb2 << 32 | lsb3 << 16 | lsb4
          )
        } else unsafeParseExtended(input)
      } else unsafeParseExtended(input)
    } else unsafeParseExtended(input)

  // A nibble is 4 bits
  @nowarn("msg=implicit numeric widening")
  private def parseNibbles(input: String, position: Int): Long = {
    val ch1 = input.charAt(position)
    val ch2 = input.charAt(position + 1)
    val ch3 = input.charAt(position + 2)
    val ch4 = input.charAt(position + 3)

    if ((ch1 | ch2 | ch3 | ch4) > 0xff) -1L
    else CharToNumeric(ch1) << 12 | CharToNumeric(ch2) << 8 | CharToNumeric(ch3) << 4 | CharToNumeric(ch4)
  }

  private def unsafeParseExtended(name: String): java.util.UUID = {
    val len = name.length
    if (len > 36) throw new IllegalArgumentException("UUID string too large")
    val dash1 = name.indexOf('-', 0)
    val dash2 = name.indexOf('-', dash1 + 1)
    val dash3 = name.indexOf('-', dash2 + 1)
    val dash4 = name.indexOf('-', dash3 + 1)
    val dash5 = name.indexOf('-', dash4 + 1)

    // For any valid input, dash1 through dash4 will be positive and dash5 will be negative,
    // but it's enough to check dash4 and dash5:
    // - if dash1 is -1, dash4 will be -1
    // - if dash1 is positive but dash2 is -1, dash4 will be -1
    // - if dash1 and dash2 is positive, dash3 will be -1, dash4 will be positive, but so will dash5
    if (dash4 < 0 || dash5 >= 0) throw new IllegalArgumentException("Invalid UUID string: " + name)

    val section1 = checkExtraBits(section = parseSection(name, 0, dash1), zeroMask = mask8Z)
    val section2 = checkExtraBits(section = parseSection(name, dash1 + 1, dash2), zeroMask = mask4Z)
    val section3 = checkExtraBits(section = parseSection(name, dash2 + 1, dash3), zeroMask = mask4Z)
    val section4 = checkExtraBits(section = parseSection(name, dash3 + 1, dash4), zeroMask = mask4Z)
    val section5 = checkExtraBits(section = parseSection(name, dash4 + 1, len), zeroMask = mask12Z)

    var mostSigBits = section1
    mostSigBits <<= 16
    mostSigBits |= section2
    mostSigBits <<= 16
    mostSigBits |= section3

    var leastSigBits = section4
    leastSigBits <<= 48
    leastSigBits |= section5

    new java.util.UUID(mostSigBits, leastSigBits)
  }

  // Adapted from java.lang.Long.parseLong with multiple optimizations from @plokhotnyuk
  @nowarn("msg=implicit numeric widening")
  private def parseSection(s: CharSequence, beginIndex: Int, endIndex: Int): Long = {
    if ((endIndex - beginIndex) > 15) throw new NumberFormatException("UUID group exceeds acceptable length")

    var i     = beginIndex
    val limit = -Long.MaxValue

    if (i < endIndex) {
      val multmin = limit >> 4 // equivalent to dividing by 16 (Radix)
      var result  = 0L
      while (i < endIndex) {
        val digit = CharToNumeric(s.charAt(i))
        if (digit < 0 || result < multmin) throw charSequenceError(s, beginIndex, endIndex, i)
        result <<= 4 // equivalent to multiplying by 16 (Radix)
        i += 1
        result -= digit
      }
      -result
    } else throw new NumberFormatException("Invalid start and end indices when parsing UUID group")
  }

  // Checks whether the user has tried to supply more Hex digits than what the UUID section expects
  // Use with maskXZ variants
  private def checkExtraBits(section: Long, zeroMask: Long): Long =
    if ((section & zeroMask) > 0)
      throw new NumberFormatException(s"Extra bits detected in section: 0x${section.toHexString}")
    else section

  private def charSequenceError(
    s: CharSequence,
    beginIndex: Int,
    endIndex: Int,
    errorIndex: Int
  ): NumberFormatException =
    new NumberFormatException(
      s"""Invalid UUID format: Error at index ${errorIndex - beginIndex} in: "${s.subSequence(
        beginIndex,
        endIndex
      )}""""
    )
}
