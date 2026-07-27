package zio.json

import zio._
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

import java.nio.charset.StandardCharsets.UTF_8

object ChunkDecoderSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int)

  object Person {
    implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
  }

  private def bytes(s: String): Chunk[Byte] = Chunk.fromArray(s.getBytes(UTF_8))

  private def viaString(c: Chunk[Byte]): String = new String(c.toArray, UTF_8)

  /** Same bytes, but as a `Chunk.Concat` rather than a single backing array. */
  private def split(c: Chunk[Byte], at: Int): Chunk[Byte] =
    Chunk.fromArray(c.take(at).toArray) ++ Chunk.fromArray(c.drop(at).toArray)

  private def str(s: String): Chunk[Byte] = bytes(Json.Str(s).toString)

  val spec: Spec[Environment, Any] =
    suite("ChunkDecoder")(
      suite("fromJson")(
        test("decodes the same values as the CharSequence path") {
          val json = """{"name":"Jules","age":42}"""

          assert(bytes(json).fromJson[Person])(isRight(equalTo(Person("Jules", 42)))) &&
          assertTrue(bytes(json).fromJson[Person] == json.fromJson[Person])
        },
        test("decodes multi-byte characters") {
          // 1, 2, 3 and 4 byte UTF-8 sequences, the last one a surrogate pair
          assert(str("a").fromJson[String])(isRight(equalTo("a"))) &&
          assert(str("é").fromJson[String])(isRight(equalTo("é"))) &&
          assert(str("中€").fromJson[String])(isRight(equalTo("中€"))) &&
          assert(str("😀").fromJson[String])(isRight(equalTo("😀"))) &&
          assert(str("aé中😀z").fromJson[String])(isRight(equalTo("aé中😀z")))
        },
        test("decodes multi-byte characters in field names") {
          final case class Süß(größe: Int)
          implicit val decoder: JsonDecoder[Süß] = DeriveJsonDecoder.gen[Süß]

          assert(bytes("""{"größe":3}""").fromJson[Süß])(isRight(equalTo(Süß(3))))
        },
        test("skips whitespace") {
          val json = "  {\n\t\"name\" : \"Jules\" ,\r\n  \"age\" : 42\n}  "

          assert(bytes(json).fromJson[Person])(isRight(equalTo(Person("Jules", 42))))
        },
        test("decodes chunks that are not backed by a single array") {
          val json = """{"name":"aé中😀z","age":42}"""
          val all  = bytes(json)

          // every split point, including ones that fall inside a multi-byte sequence
          assertTrue((0 to all.length).forall(at => split(all, at).fromJson[Person] == all.fromJson[Person]))
        },
        test("decodes payloads larger than the internal window") {
          // numbers have no terminator, so every element retracts, including across a window refill
          val ints = (1 to 20000).toList
          val all  = bytes(ints.toJson)

          assertTrue(all.length > 8192) &&
          assert(all.fromJson[List[Int]])(isRight(equalTo(ints))) &&
          assert(split(all, all.length / 3).fromJson[List[Int]])(isRight(equalTo(ints)))
        },
        test("retracts to a position the window has already moved past") {
          // more leading whitespace than the window holds, then a number running to the very end of input: the
          // number retracts after hitting the end, by which point the window no longer covers the marked position
          val padded = " " * 20000 + "42"

          assert(bytes(padded).fromJson[Int])(isRight(equalTo(42))) &&
          assertTrue(bytes(padded).fromJson[Int] == padded.fromJson[Int]) &&
          assertTrue(bytes(" " * 20000).fromJson[Int] == (" " * 20000).fromJson[Int])
        },
        test("decodes multi-byte characters straddling the window boundary") {
          // the padding shifts every sequence by one byte, so across the four cases a 2, 3 and 4 byte sequence each
          // land on the boundary
          val texts = (0 to 3).map(pad => "a" * pad + "é中😀" * 4000)

          assertTrue(texts.forall(t => str(t).fromJson[String] == Right(t)))
        },
        test("reports the same errors as the CharSequence path") {
          val cases = List(
            "",
            "   ",
            """{"name":"x","age":""",
            """{"name":"x","age":"nope"}""",
            """{"name":"x"}""",
            """{"name":"x","age":1""",
            """[1,2,""",
            """{"name":"é","age":"nope"}"""
          )

          assertTrue(cases.forall(json => bytes(json).fromJson[Person] == json.fromJson[Person])) &&
          assert(bytes("").fromJson[Person])(isLeft(equalTo("Unexpected end of input"))) &&
          assert(bytes("""{"name":"x","age":"nope"}""").fromJson[Person])(isLeft(equalTo(".age(expected an Int)")))
        },
        test("retracts correctly at the end of input") {
          // numbers have no terminator, so the parser reads one char past them and retracts
          assert(bytes("42").fromJson[Int])(isRight(equalTo(42))) &&
          assert(bytes("-1.5e3").fromJson[Double])(isRight(equalTo(-1500.0))) &&
          assert(bytes("[1,2,3]").fromJson[List[Int]])(isRight(equalTo(List(1, 2, 3)))) &&
          assert(bytes(" 42 ").fromJson[Int])(isRight(equalTo(42)))
        },
        test("supports decoders that rewind") {
          // orElse wraps the reader in a RecordingReader, which retracts and replays
          implicit val decoder: JsonDecoder[Either[Int, Person]] = JsonDecoder[Int].orElseEither(JsonDecoder[Person])

          assert(bytes("42").fromJson[Either[Int, Person]])(isRight(isLeft(equalTo(42)))) &&
          assert(bytes("""{"name":"é","age":42}""").fromJson[Either[Int, Person]])(
            isRight(isRight(equalTo(Person("é", 42))))
          )
        },
        test("decodes into the AST") {
          val json = """{"a":[1,"é",true,null],"b":{"😀":1.5}}"""

          assertTrue(bytes(json).fromJson[Json] == json.fromJson[Json])
        },
        test("replaces malformed UTF-8, as new String(bytes, UTF_8) does") {
          def decode(bs: Int*): Either[String, String] =
            (Chunk.fromArray("\"".getBytes(UTF_8)) ++ Chunk.fromIterable(bs.map(_.toByte)) ++
              Chunk.fromArray("\"".getBytes(UTF_8))).fromJson[String]

          assert(decode(0xc3))(isRight(equalTo("�"))) &&                      // truncated 2 byte sequence
          assert(decode(0x80))(isRight(equalTo("�"))) &&                      // stray continuation byte
          assert(decode(0xc3, 0x28))(isRight(equalTo("�("))) &&               // bad continuation, byte re-examined
          assert(decode(0xc0, 0x80))(isRight(equalTo("��"))) &&               // overlong encoding of NUL
          assert(decode(0xe0, 0xa0))(isRight(equalTo("�"))) &&                // truncated 3 byte sequence
          assert(decode(0xf5, 0x80, 0x80, 0x80))(isRight(equalTo("����"))) && // beyond U+10FFFF
          assert(decode(0xed, 0xa0, 0x80))(isRight(equalTo("�")))             // CESU-8 surrogate, see the scaladoc
        },
        test("agrees with the CharSequence path on generated input") {
          check(Gen.string(Gen.unicodeChar), Gen.int) { (name, age) =>
            val bs = bytes(Person(name, age).toJson)

            assertTrue(bs.fromJson[Person] == viaString(bs).fromJson[Person])
          }
        },
        test("agrees with the CharSequence path on every truncation") {
          // truncating at every byte offset walks the parser into each of its end-of-input and retract corners
          val docs = List(
            """{"name":"Jules","age":42}""",
            """{"name":"aé中😀z","age":-1}""",
            """ { "name" : "x" , "age" : 42 } """,
            """{"age":42,"name":"x","extra":[1,2,{"a":null}]}"""
          )

          assertTrue(docs.forall { doc =>
            val all = bytes(doc)
            (0 to all.length).forall { n =>
              val prefix = all.take(n)
              // compare against the same bytes seen as a String, so a split multi-byte char is not the difference
              prefix.fromJson[Person] == viaString(prefix).fromJson[Person] &&
              prefix.fromJson[Json] == viaString(prefix).fromJson[Json]
            }
          })
        }
      )
    )
}
