package zio.json.internal

import zio.json._
import zio.test._

object LexerSpec extends ZIOSpecDefault {

  final case class KnownField(b: Boolean)
  object KnownField {
    implicit val decoder: JsonDecoder[KnownField] = DeriveJsonDecoder.gen[KnownField]
  }

  private def skip(input: String): Either[Throwable, Unit] =
    try {
      Lexer.skipValue(Nil, new FastStringReader(input))
      Right(())
    } catch {
      case t: Throwable => Left(t)
    }

  // Skip the first value and return whatever remains in the reader, including EOF as "".
  private def skipAndRemainder(input: String): Either[Throwable, String] = {
    val reader = new FastStringReader(input)
    try {
      Lexer.skipValue(Nil, reader)
      val sb = new StringBuilder
      try
        while (true) sb.append(reader.readChar())
      catch {
        case _: UnexpectedEnd => ()
      }
      Right(sb.toString)
    } catch {
      case t: Throwable => Left(t)
    }
  }

  val spec: Spec[Environment, Any] =
    suite("Lexer.skipValue")(
      suite("bare numbers terminated by EOF")(
        test("integer")(assertTrue(skip("42").isRight)),
        test("decimal")(assertTrue(skip("42.5").isRight)),
        test("negative")(assertTrue(skip("-7").isRight)),
        test("scientific")(assertTrue(skip("1e10").isRight)),
        test("scientific with negative exponent")(assertTrue(skip("1.5e-3").isRight)),
        test("zero")(assertTrue(skip("0").isRight)),
        test("negative decimal")(assertTrue(skip("-3.14").isRight))
      ),
      suite("bare literals terminated by EOF")(
        test("true")(assertTrue(skip("true").isRight)),
        test("false")(assertTrue(skip("false").isRight)),
        test("null")(assertTrue(skip("null").isRight))
      ),
      suite("numbers followed by a delimiter (regression guards)")(
        test("trailing whitespace")(assertTrue(skipAndRemainder("42 ") == Right(" "))),
        test("trailing comma")(assertTrue(skipAndRemainder("42,") == Right(","))),
        test("trailing brace")(assertTrue(skipAndRemainder("42}") == Right("}"))),
        test("trailing bracket")(assertTrue(skipAndRemainder("42]") == Right("]"))),
        test("decimal followed by comma")(assertTrue(skipAndRemainder("3.14,rest") == Right(",rest"))),
        test("exponent followed by whitespace")(assertTrue(skipAndRemainder("1e5 xyz") == Right(" xyz")))
      ),
      test("skipping a numeric unknown field inside an object works end-to-end") {
        assertTrue("""{"a":42,"b":true}""".fromJson[KnownField] == Right(KnownField(true))) &&
        assertTrue("""{"a":1.5e-3,"b":false}""".fromJson[KnownField] == Right(KnownField(false))) &&
        assertTrue("""{"b":true,"a":42}""".fromJson[KnownField] == Right(KnownField(true)))
      },
      test("skipping multiple values in sequence") {
        val reader = new FastStringReader("""42,true,null,"x",[1,2],{"k":1}""")
        Lexer.skipValue(Nil, reader)
        val c1 = reader.readChar()
        Lexer.skipValue(Nil, reader)
        val c2 = reader.readChar()
        Lexer.skipValue(Nil, reader)
        val c3 = reader.readChar()
        Lexer.skipValue(Nil, reader)
        val c4 = reader.readChar()
        Lexer.skipValue(Nil, reader)
        val c5 = reader.readChar()
        Lexer.skipValue(Nil, reader)
        // all values skipped; reader should now be exhausted
        val exhausted =
          try {
            reader.readChar(); false
          } catch {
            case _: UnexpectedEnd => true
          }
        assertTrue(c1 == ',' && c2 == ',' && c3 == ',' && c4 == ',' && c5 == ',' && exhausted)
      }
    )
}
