package zio.json.internal.async

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

            val leftovers = decoding.feed("tr")

            assertTrue(leftovers == 2) &&
            assertTrue(registers.registers(0) == ())

            val oneMore = decoding.feed("u")

            assertTrue(oneMore == 1) &&
            assertTrue(registers.registers(0) == ())

            val done = decoding.feed("e")

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

            val leftovers = decoding.feed("fal")

            assertTrue(leftovers == 2) &&
            assertTrue(registers.registers(0) == ())

            val oneMore = decoding.feed("s")

            assertTrue(oneMore == 1) &&
            assertTrue(registers.registers(0) == ())

            val done = decoding.feed("e")

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
      )
    )
}
