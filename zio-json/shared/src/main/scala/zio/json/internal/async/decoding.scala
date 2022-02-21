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
   * Returns the number of characters consumed. If this is equal to the 0, then the full length was consumed, but no more data is desired,
   * because the decoding process is finished. On the other hand, if this is greater
   * than the 0, then the full length was consumed, and
   * more data is desired. If this is less than the 0, then
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

  def decodeLiteral(
    literal: Array[Char],
    decodingPosition: Int,
    chars: CharSequence,
    errorMessage: String
  ): Int = {
    val seqLen     = chars.length()
    val literalLen = literal.length

    if (seqLen == 0) {
      literalLen - decodingPosition
    } else {
      val len                       = Math.min(seqLen, literalLen - decodingPosition)
      var charSequencePosition: Int = 0

      while (charSequencePosition < len) {
        if (chars.charAt(charSequencePosition) != literal(charSequencePosition + decodingPosition))
          throw AsyncDecodingError(errorMessage)
        charSequencePosition += 1
      }

      val newPosition = charSequencePosition + decodingPosition

      if (newPosition < seqLen)
        newPosition - seqLen
      else
        literalLen - newPosition
    }
  }

  abstract class AnyOfLiterals[Value](literals: Array[Array[Char]], error: String) extends AsyncDecoding[Value] {
    private var state = 0

    private val BranchMask: Int = Integer.highestOneBit(literals.length) * 2 - 1
    private val BranchBitLen: Int = Integer.bitCount(Integer.highestOneBit(literals.length) * 2 - 1)
    protected def branch(): Int = (state & BranchMask)
    private def position(): Int = (state >> BranchBitLen)

    private def setPosition(int: Int): Unit                       = state = (state & BranchMask) | (int << BranchBitLen)
    private def setBranchAndPosition(branch: Int, pos: Int): Unit = state = branch | (pos << BranchBitLen)

    override def reset(): Unit =
      state = 0

    private def indexOf(char: Char): Int = {
      var i = 0
      while (i < literals.length) {
        if (char == literals(i)(0)) return i
        i += 1
      }
      -1
    }

    override def feed(chars: CharSequence): Int =
      if (chars.length() == 0) {
        literals(branch() - 1).length - position()
      } else
        branch() match {
          case 0 =>
            val firstChar = chars.charAt(0)

            val index = indexOf(firstChar)

            if (index == -1)
              throw AsyncDecodingError(error)

            val literal = literals(index)

            val curPos   = position()
            val consumed = decodeLiteral(literal, curPos, chars, error)

            if (consumed <= 0) {
              setBranchAndPosition(index + 1, literal.length)
              setRegister
            } else
              setBranchAndPosition(index + 1, literal.length - consumed)
            consumed
          case branch =>
            val curPos  = position()
            val literal = literals(branch - 1)

            val consumed = decodeLiteral(literal, curPos, chars, error)
            if (consumed <= 0) {
              setPosition(literal.length)
              setRegister
            } else
              setPosition(literal.length - consumed)

            consumed
        }

    def setRegister: Unit

  }

  val boolean: AsyncDecoder[Boolean] =
    (index: Int, registers: Registers) =>
      new AnyOfLiterals[Boolean](
        Array("true".toCharArray, "false".toCharArray),
        """Expected "true" or "false" boolean literal"""
      ) {
        def setRegister: Unit =
          branch() match {
            case 1 => registers.registers(index) = true
            case 2 => registers.registers(index) = false
            case _ =>
          }
      }

  val any: AsyncDecoder[Any] =
    (index: Int, registers: Registers) =>
      new AnyOfLiterals[Any](
        Array("true".toCharArray, "false".toCharArray, "null".toCharArray, "literal".toCharArray),
        """Expected "true" or "false" boolean literal"""
      ) {
        def setRegister: Unit =
          branch() match {
            case 1 => registers.registers(index) = true
            case 2 => registers.registers(index) = false
            case 3 => registers.registers(index) = None
            case 4 => registers.registers(index) = "literal"
            case _ =>
          }
      }

}
