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
  class LiteralDecoding(literal: Array[Char], errorMessage: Option[String] = None) extends AsyncDecoding[String] {
    private var position: Int = 0

    def reset(): Unit = position = 0

    def feed(charSequence: CharSequence): Int = {
      val seqLen = charSequence.length()

      if (seqLen == 0) {
        seqLen - position
      } else {
        val literalLen                = literal.length
        val len                       = Math.min(seqLen, literalLen - position)
        var charSequencePosition: Int = 0

        while (charSequencePosition < len) {
          if (charSequence.charAt(charSequencePosition) != literal(charSequencePosition + position))
            throw AsyncDecodingError(errorMessage.getOrElse(s"""Expected "${new String(literal)}" literal"""))
          charSequencePosition += 1
        }

        position += charSequencePosition

        if (position < seqLen)
          position - seqLen
        else
          literalLen - position
      }
    }
  }

  val boolean: AsyncDecoder[Boolean] =
    (index: Int, registers: Registers) =>
      new AsyncDecoding[Boolean] {
        val trueDecoder: AsyncDecoding[String] =
          new LiteralDecoding("true".toCharArray, Some("""Expected "true" or "false" boolean literal"""))
        val falseDecoder: AsyncDecoding[String] =
          new LiteralDecoding("false".toCharArray, Some("""Expected "true" or "false" boolean literal"""))

        def reset(): Unit = {
          trueDecoder.reset()
          falseDecoder.reset()
        }

        def feed(chars: CharSequence): Int =
          try {
            consumeTrue(chars)
          } catch {
            case _: AsyncDecodingError =>
              consumeFalse(chars)
          }

        private def consumeTrue(chars: CharSequence): Int = {
          val consumed = trueDecoder.feed(chars)

          if (consumed <= 0)
            registers.registers(index) = true

          consumed
        }

        private def consumeFalse(chars: CharSequence): Int = {
          val consumed = falseDecoder.feed(chars)

          if (consumed <= 0)
            registers.registers(index) = false

          consumed
        }

      }

}
