package zio.json.internal.async

import scala.util.control.NoStackTrace

case class Registers(registers: Array[Any])

trait AsyncDecoding[+Value] {

  /**
   * Resets all state of the decoding to the initial values, such that the
   * decoding is equivalent to a new one. This method can be used by single-fibered
   * code in order to accelerate performance and avoid re-creating decodings for
   * the same type of data.
   */
  def reset(): Unit

  /**
   * Returns the number of characters consumed. If this is equal to the length of the
   * char sequence, then the full length was consumed, but no more data is desired,
   * because the decoding processs is finished. On the other hand, if this is greater
   * than the length of the char sequence, then the full length was consumed, and
   * more data is desired. If this is less than the length of the char sequence, then
   * no more data is required, and the decoding stopped short of consuming all characters
   * in the char sequence. If there is an error, then an `AsyncDecodingError` is thrown.
   */
  def feed(chars: CharSequence): Int
}

final case class AsyncDecodingError(message: String, path: List[String] = Nil)
    extends Exception(message)
    with NoStackTrace

trait AsyncDecoder[+Value] {
  def unsafeNewDecoding(index: Int, registers: Registers): AsyncDecoding[Value]
}
object AsyncDecoder {

  /**
   * -x => leftovers
   * 0 => complete match
   * +x => need more
   */
  def consumeChars(chars: CharSequence, expect: Array[Char], errorMessage: String): Int = {
    var consumed: Int = 0

    val charsLen: Int    = chars.length()
    val expectedLen: Int = expect.length
    val len: Int         = Math.min(charsLen, expectedLen)

    while (consumed < len) {
      if (chars.charAt(consumed) != expect(consumed))
        throw AsyncDecodingError(errorMessage)
      consumed += 1
    }

    if (consumed < charsLen)
      consumed - charsLen
    else
      expectedLen - consumed
  }

  val boolean: AsyncDecoder[Boolean] =
    (index: Int, registers: Registers) =>
      new AsyncDecoding[Boolean] {
        var state: Int = 0

        val trueMatrix: Array[Array[Char]] = Array(
          "rue".toCharArray,
          "ue".toCharArray,
          "e".toCharArray
        )

        val falseMatrix: Array[Array[Char]] = Array(
          "alse".toCharArray,
          "lse".toCharArray,
          "se".toCharArray,
          "e".toCharArray
        )

        val error: String     = """Expected "true" or "false" boolean literal"""

        val BranchMask   = 0x3
        val BranchBitLen = 2

        def branch(): Int   = (state & BranchMask)
        def position(): Int = (state >> BranchBitLen)

        val Undecided = 0
        val IsTrue    = 1
        val IsFalse   = 2

        def setPosition(int: Int): Unit                       = state = (state & BranchMask) | (int << BranchBitLen)
        def setBranchAndPosition(branch: Int, pos: Int): Unit = state = branch | (pos << BranchBitLen)

        // 't' 'r' 'u' 'e'
        // 'f' 'a' 'l' 's' 'e'
        def reset(): Unit = state = 0

        def feed(chars: CharSequence): Int =
          if (chars.length == 0)
            branch() match {
              case Undecided => 0
              case IsTrue    => 0
              case IsFalse   => 0
            }
          else {
            branch() match {
              case Undecided =>
                chars.charAt(0) match {
                  case 't' =>
                    val rest       = trueMatrix(0)
                    val charsLen   = chars.length
                    val restLength = rest.length
                    val consumed   = consumeChars(chars.subSequence(1, charsLen), rest, error)

                    if (consumed <= 0) {
                      setBranchAndPosition(IsTrue, restLength + 1)
                      registers.registers(index) = true
                    } else
                      setBranchAndPosition(IsTrue, restLength - consumed)

                    consumed

                  case 'f' =>
                    val rest       = falseMatrix(0)
                    val charsLen   = chars.length
                    val restLength = rest.length
                    val consumed   = consumeChars(chars.subSequence(1, charsLen), rest, error)

                    if (consumed <= 0) {
                      setBranchAndPosition(IsFalse, restLength + 1)
                      registers.registers(index) = false
                    } else
                      setBranchAndPosition(IsFalse, restLength - consumed)

                    consumed

                  case _ => throw AsyncDecodingError("""Expected "true" or "false" boolean literal""")
                }
              case IsTrue =>
                val curPos = position()

                val rest       = trueMatrix(curPos)
                val restLength = rest.length
                val consumed   = consumeChars(chars, rest, error)

                if (consumed <= 0) {
                  setPosition(restLength + 1 + curPos)
                  registers.registers(index) = true
                } else
                  setPosition(restLength - consumed + curPos)

                consumed

              case IsFalse =>
                val curPos = position()

                val rest       = falseMatrix(curPos)
                val restLength = rest.length
                val consumed   = consumeChars(chars, rest, error)

                if (consumed <= 0) {
                  setPosition(restLength + 1 + curPos)
                  registers.registers(index) = false
                } else
                  setPosition(restLength - consumed + curPos)

                consumed
            }
          }
      }
}
