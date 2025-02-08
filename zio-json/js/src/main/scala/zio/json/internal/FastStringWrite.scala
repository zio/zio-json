package zio.json.internal

final class FastStringWrite(initial: Int) extends Write {
  require(initial >= 8)
  private[this] var chars: String = ""

  @inline def reset(): Unit = chars = ""

  @inline private[internal] def length: Int = chars.length

  @inline private[internal] def getChars: Array[Char] = chars.toCharArray

  @inline def write(s: String): Unit = chars += s

  @inline def write(c: Char): Unit = chars += c

  @inline override def write(cs: Array[Char], from: Int, to: Int): Unit = {
    var i = from
    while (i < to) {
      chars += cs(i)
      i += 1
    }
  }

  @inline override def write(c1: Char, c2: Char): Unit = {
    chars += c1
    chars += c2
  }

  @inline override def write(c1: Char, c2: Char, c3: Char): Unit = {
    chars += c1
    chars += c2
    chars += c3
  }

  @inline override def write(c1: Char, c2: Char, c3: Char, c4: Char): Unit = {
    chars += c1
    chars += c2
    chars += c3
    chars += c4
  }

  @inline override def write(c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): Unit = {
    chars += c1
    chars += c2
    chars += c3
    chars += c4
    chars += c5
  }

  @inline override def write(s: Short): Unit = {
    chars += (s & 0xff).toChar
    chars += (s >> 8).toChar
  }

  @inline override def write(s1: Short, s2: Short): Unit = {
    chars += (s1 & 0xff).toChar
    chars += (s1 >> 8).toChar
    chars += (s2 & 0xff).toChar
    chars += (s2 >> 8).toChar
  }

  @inline override def write(s1: Short, s2: Short, s3: Short): Unit = {
    chars += (s1 & 0xff).toChar
    chars += (s1 >> 8).toChar
    chars += (s2 & 0xff).toChar
    chars += (s2 >> 8).toChar
    chars += (s3 & 0xff).toChar
    chars += (s3 >> 8).toChar
  }

  @inline override def write(s1: Short, s2: Short, s3: Short, s4: Short): Unit = {
    chars += (s1 & 0xff).toChar
    chars += (s1 >> 8).toChar
    chars += (s2 & 0xff).toChar
    chars += (s2 >> 8).toChar
    chars += (s3 & 0xff).toChar
    chars += (s3 >> 8).toChar
    chars += (s4 & 0xff).toChar
    chars += (s4 >> 8).toChar
  }

  @inline def buffer: CharSequence = chars
}
