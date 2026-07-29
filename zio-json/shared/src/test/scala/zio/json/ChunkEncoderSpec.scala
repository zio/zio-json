package zio.json

import zio.json.ast.Json
import zio.test._

import java.nio.charset.StandardCharsets.UTF_8

object ChunkEncoderSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int, tags: List[String])

  object Person {
    implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
  }

  private def viaString[A](a: A)(implicit encoder: JsonEncoder[A]): Array[Byte] = a.toJson.getBytes(UTF_8)

  /** Every encoded form must match the `String` path byte for byte. */
  private def parity[A](a: A)(implicit encoder: JsonEncoder[A]): TestResult = {
    val expected = viaString(a)

    assertTrue(
      a.toJsonBytesArray.sameElements(expected),
      a.toJsonBytes.toArray.sameElements(expected),
      JsonEncoder[A].encodeJsonBytesArray(a).sameElements(expected),
      JsonEncoder[A].encodeJsonBytes(a).toArray.sameElements(expected)
    )
  }

  private def parityPretty[A](a: A)(implicit encoder: JsonEncoder[A]): TestResult = {
    val expected = JsonEncoder[A].encodeJson(a, Some(0)).toString.getBytes(UTF_8)

    assertTrue(JsonEncoder[A].encodeJsonBytesArray(a, Some(0)).sameElements(expected))
  }

  val spec: Spec[Environment, Any] =
    suite("ChunkEncoder")(
      suite("values")(
        test("encodes scalars identically to the String path") {
          parity(true) &&
          parity(42) &&
          parity(-1500.0) &&
          parity(Long.MaxValue) &&
          parity(BigDecimal("0.1234567890123456789")) &&
          parity("plain ascii") &&
          parity(List(1, 2, 3)) &&
          parity(Map("a" -> 1, "b" -> 2)) &&
          parity(Option.empty[Int]) &&
          parity(Option(5))
        },
        test("encodes case classes and collections identically") {
          parity(Person("Jules", 42, List("a", "b"))) &&
          parity(List.fill(3)(Person("x", 1, Nil))) &&
          parity(Map("k1" -> Person("x", 1, Nil), "k2" -> Person("y", 2, List("z"))))
        },
        test("encodes escaped strings identically") {
          parity("a\"b\\c\bd\fe\nf\rg\th") &&
          parity(Person("a\"b", 1, List("\u0000", "\u001f")))
        },
        test("encodes an astral char next to an escape identically") {
          // an escapable char forces the char-by-char path, which writes a surrogate pair as two separate calls
          parity("a\"b😀") &&
          parity("😀\na") &&
          parity(Person("a\"b😀", 1, Nil))
        }
      ),
      suite("UTF-8")(
        test("encodes 1, 2, 3 and 4 byte sequences identically") {
          parity("a") &&
          parity("é") &&
          parity("中") &&
          parity("€") &&
          parity("😀") &&
          parity("aé中😀z") &&
          parity(Person("é中😀", 1, List("😀😀", "é")))
        },
        test("encodes multi-byte field names identically") {
          final case class Süß(größe: Int, `🔑`: String)
          implicit val enc: JsonEncoder[Süß] = DeriveJsonEncoder.gen[Süß]

          parity(Süß(3, "é"))
        },
        test("encodes every code point in the BMP and a sample above it identically") {
          // excludes the surrogate range, which cannot appear in a well formed String this way
          val bmp    = ((0x20 to 0xd7ff) ++ (0xe000 to 0xffff)).map(_.toChar).mkString
          val astral = (0x10000 to 0x10ffff by 997).map(cp => new String(Character.toChars(cp))).mkString

          // batched: putting a 63k character string directly in the assertion crashes zio-test's pretty printer on
          // Scala Native (see ChunkDecoderSpec), so only the indices of mismatching batches reach the assertion
          def badBatches(s: String, size: Int): Seq[Int] =
            s.grouped(size)
              .zipWithIndex
              .collect {
                case (batch, i) if !batch.toJsonBytesArray.sameElements(viaString(batch)) => i
              }
              .toSeq

          assertTrue(
            badBatches(bmp, 512) == Seq.empty[Int],
            badBatches(astral, 64) == Seq.empty[Int]
          )
        },
        test("replaces a lone surrogate the same way String.getBytes(UTF_8) does") {
          // '?' (0x3f), not U+FFFD -- String.getBytes(UTF_8)'s own encoder uses a single-byte default replacement,
          // unlike the decode direction (bytes -> chars), which is where U+FFFD comes from
          parity("abc\uD800") &&
          parity("abc\uDC00") &&
          parity("abc\uD800xyz") &&
          parity("a\uD800\uD800b") &&
          parity("a\uD800Zb") &&
          parity(Person("a\uD800b", 1, List("x\uDC00y"))) &&
          // a pending surrogate resolved by the multi-char write of an escape sequence, and one left for the
          // closing quote -- the escaped path is the only place a surrogate and an escape can interleave
          parity("a\uD800\"b") &&
          parity("a\uD800\nb") &&
          parity("\"\uD800")
        },
        test("agrees with the String path on generated strings, including lone surrogates") {
          val genMaybeSurrogate =
            Gen.oneOf(Gen.unicodeChar, Gen.const('\uD800'), Gen.const('\uDC00'), Gen.const('\uD83D'))

          check(Gen.listOf(genMaybeSurrogate).map(_.mkString)) { s =>
            parity(s)
          }
        }
      ),
      suite("pretty printing")(
        test("indented output matches the String path") {
          parityPretty(Person("Jules é😀", 42, List("a\"b", "line\nbreak"))) &&
          parityPretty(List.fill(3)(Person("x", 1, List("a", "b"))))
        }
      ),
      suite("large payloads")(
        test("encodes payloads spanning many internal array growths identically") {
          val big = List.fill(5000)(Person("aé中😀z" * 3, 1, List("x", "y", "z")))

          parity(big)
        },
        test("round trips through toJsonAST while another encode is already on the stack") {
          // toJsonAST's default implementation recursively calls encodeJson, exercising the write pool's
          // per-recursion-level slots
          val p = Person("Jules é中😀", 42, List("a\"b", "😀"))

          assertTrue(p.toJsonAST == p.toJson.fromJson[Json])
        }
      ),
      suite("properties")(
        test("agrees with the String path on generated values") {
          check(Gen.string(Gen.unicodeChar), Gen.int, Gen.listOf(Gen.string(Gen.unicodeChar))) { (name, age, tags) =>
            parity(Person(name, age, tags))
          }
        }
      )
    )
}
