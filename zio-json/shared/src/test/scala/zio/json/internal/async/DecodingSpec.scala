package zio.json.internal.async

import zio.json.internal.async.AsyncDecoder.LiteralDecoding
import zio.test._

object DecodingSpec extends DefaultRunnableSpec {
  val spec: ZSpec[Environment, Failure] =
    suite("AsyncDecoding")(
      suite("boolean")(
        suite("true")(
          test("no leftovers") {
            val registers = Registers(Array(()))
            val decoding  = AsyncDecoder.boolean.unsafeNewDecoding(0, registers)

            val leftovers = decoding.feed("true")

            assertTrue(leftovers == 0) &&
            assertTrue(registers.registers(0) == true)
          },
          test("needs more") {
            val registers = Registers(Array(()))
            val decoding  = AsyncDecoder.boolean.unsafeNewDecoding(0, registers)

            val need2More         = decoding.feed("tr")
            val need2MoreRegister = registers.registers(0)

            val need1More         = decoding.feed("u")
            val need1MoreRegister = registers.registers(0)

            val done = decoding.feed("e")

            assertTrue(need2More == 2) &&
            assertTrue(need2MoreRegister == ()) &&
            assertTrue(need1More == 1) &&
            assertTrue(need1MoreRegister == ()) &&
            assertTrue(done == 0) &&
            assertTrue(registers.registers(0) == true)
          },
          test("too many") {
            val registers = Registers(Array(()))
            val decoding  = AsyncDecoder.boolean.unsafeNewDecoding(0, registers)

            val state = decoding.feed("truetr")

            assertTrue(state == -2) &&
            assertTrue(registers.registers(0) == true)
          }
        ),
        suite("false")(
          test("no leftovers") {
            val registers = Registers(Array(()))
            val decoding  = AsyncDecoder.boolean.unsafeNewDecoding(0, registers)

            val leftovers = decoding.feed("false")

            assertTrue(leftovers == 0) &&
            assertTrue(registers.registers(0) == false)
          },
          test("needs more") {
            val registers = Registers(Array(()))
            val decoding  = AsyncDecoder.boolean.unsafeNewDecoding(0, registers)

            val need2More         = decoding.feed("fal")
            val need2MoreRegister = registers.registers(0)

            val need1More         = decoding.feed("s")
            val need1MoreRegister = registers.registers(0)

            val done = decoding.feed("e")

            assertTrue(need2More == 2) &&
            assertTrue(need2MoreRegister == ()) &&
            assertTrue(need1More == 1) &&
            assertTrue(need1MoreRegister == ()) &&
            assertTrue(done == 0) &&
            assertTrue(registers.registers(0) == false)
          },
          test("too many") {
            val registers = Registers(Array(()))
            val decoding  = AsyncDecoder.boolean.unsafeNewDecoding(0, registers)

            val state = decoding.feed("falsefa")

            assertTrue(state == -2) &&
            assertTrue(registers.registers(0) == false)
          }
        )
      ),
      suite("literal")(
        test("no leftovers") {
          val literal = "literal"
          val literalDecoding =
            new LiteralDecoding(literal = literal.toCharArray)

          assertTrue(literalDecoding.feed(literal) == 0)
        },
        test("needs more") {
          val literal = "literal"
          val literalDecoding =
            new LiteralDecoding(literal = literal.toCharArray)

          val needs4More = literalDecoding.feed(literal.take(3))
          val needs1More = literalDecoding.feed("era")
          val done       = literalDecoding.feed("l")
          assertTrue(needs4More == 4) && assertTrue(needs1More == 1) && assertTrue(done == 0)
        },
        test("tooMany") {
          val literal = "literal"
          val literalDecoding =
            new LiteralDecoding(literal = literal.toCharArray)

          assertTrue(literalDecoding.feed(literal ++ "12") == -2)
        }
      )
    )
}
