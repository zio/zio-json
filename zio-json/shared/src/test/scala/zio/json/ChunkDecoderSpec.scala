package zio.json

import zio._
import zio.json.ast.Json
import zio.test._

import java.nio.charset.StandardCharsets.UTF_8

object ChunkDecoderSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int)

  object Person {
    implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
  }

  /** Mirrors `Utf8ChunkReader.WindowSize`, so that the tests can straddle it deliberately. */
  private val Window = 8192

  private def utf8(s: String): Array[Byte] = s.getBytes(UTF_8)

  private def bytes(s: String): Chunk[Byte] = Chunk.fromArray(utf8(s))

  private def quoted(s: String): String = Json.Str(s).toString

  /**
   * Every `Chunk` shape the reader has to cope with. `Chunk.ByteArray` is read in place; everything else goes through
   * the window, so both code paths are exercised by every case that uses this.
   */
  private def shapes(bs: Array[Byte]): List[(String, Chunk[Byte])] = {
    val lead   = Array[Byte](1, 2, 3)
    val padded = lead ++ bs ++ Array[Byte](4, 5)
    val half   = bs.length / 2
    List(
      "ByteArray"        -> Chunk.fromArray(bs),
      "ByteArray+offset" -> Chunk.ByteArray(padded, lead.length, bs.length),
      "Concat"           -> (Chunk.fromArray(bs.take(half)) ++ Chunk.fromArray(bs.drop(half))),
      "Concat*8"         -> bs.grouped(math.max(bs.length / 8, 1)).foldLeft(Chunk.empty[Byte])(_ ++ Chunk.fromArray(_)),
      "Slice"            -> Chunk.fromArray(padded).drop(lead.length).take(bs.length),
      "Iterable"         -> Chunk.fromIterable(bs.toList)
    )
  }

  /** Every shape must decode exactly as the same bytes seen through a `String` do, errors included. */
  private def parity[A](bs: Array[Byte])(implicit decoder: JsonDecoder[A]): TestResult = {
    val expected = new String(bs, UTF_8).fromJson[A]
    val wrong    = shapes(bs).collect { case (name, c) if c.fromJson[A] != expected => name -> c.fromJson[A] }

    assertTrue(wrong.isEmpty)
  }

  private def parityOf[A](s: String)(implicit decoder: JsonDecoder[A]): TestResult = parity[A](utf8(s))

  /** A JSON string literal built from raw bytes, so malformed UTF-8 can be fed through the parser. */
  private def rawString(bs: Chunk[Byte]): Chunk[Byte] =
    Chunk.fromArray(utf8("\"")) ++ bs ++ Chunk.fromArray(utf8("\""))

  private def rawString(bs: Int*): Chunk[Byte] = rawString(Chunk.fromIterable(bs.map(_.toByte)))

  /** Bytes that a JSON string literal can carry unescaped. Mostly malformed UTF-8, which is the point. */
  private val genStringBytes: Gen[Any, Chunk[Byte]] =
    Gen.chunkOfBounded(0, 32)(Gen.byte.filter(b => b != '"' && b != '\\' && (b & 0xff) >= 0x20))

  val spec: Spec[Environment, Any] =
    suite("ChunkDecoder")(
      suite("values")(
        test("decodes the same values as the CharSequence path") {
          val json = """{"name":"Jules","age":42}"""

          assertTrue(
            bytes(json).fromJson[Person] == Right(Person("Jules", 42)),
            bytes(json).fromJson[Person] == json.fromJson[Person]
          )
        },
        test("decodes every scalar type") {
          assertTrue(
            bytes("true").fromJson[Boolean] == Right(true),
            bytes("null").fromJson[Option[Int]] == Right(None),
            bytes(""""a"""").fromJson[Char] == Right('a'),
            bytes("-128").fromJson[Byte] == Right(Byte.MinValue),
            bytes("32767").fromJson[Short] == Right(Short.MaxValue),
            bytes("-2147483648").fromJson[Int] == Right(Int.MinValue),
            bytes("9223372036854775807").fromJson[Long] == Right(Long.MaxValue),
            bytes("-1.5e3").fromJson[Double] == Right(-1500.0),
            bytes("1e-3").fromJson[Float] == Right(0.001f),
            bytes("123456789012345678901234567890").fromJson[BigInt] == Right(BigInt("123456789012345678901234567890")),
            bytes("0.1234567890123456789").fromJson[BigDecimal] == Right(BigDecimal("0.1234567890123456789"))
          )
        },
        test("decodes collections and nesting") {
          assertTrue(
            bytes("[]").fromJson[List[Int]] == Right(Nil),
            bytes("[[1],[2,3],[]]").fromJson[List[List[Int]]] == Right(List(List(1), List(2, 3), Nil)),
            bytes("""{"a":1,"b":2}""").fromJson[Map[String, Int]] == Right(Map("a" -> 1, "b" -> 2))
          ) &&
          parityOf[Json]("""{"a":[1,"é",true,null,{"b":[[[]]]}],"c":{"😀":1.5}}""")
        },
        test("decodes string escapes") {
          val escapes = """"a\"b\\c\/d\be\ff\ng\rh\tiéj😀k""""

          assertTrue(bytes(escapes).fromJson[String] == Right("a\"b\\c/d\be\ff\ng\rh\tiéj😀k")) &&
          parity[String](utf8(escapes))
        }
      ),
      suite("UTF-8")(
        test("decodes 1, 2, 3 and 4 byte sequences") {
          val texts = List("a", "é", "中", "€", "😀", "aé中😀z", "", "߿", "ࠀ", "￿", "𝄞")

          assertTrue(texts.forall(t => bytes(quoted(t)).fromJson[String] == Right(t)))
        },
        test("decodes multi-byte characters in field names") {
          final case class Süß(größe: Int, `🔑`: String)
          implicit val decoder: JsonDecoder[Süß] = DeriveJsonDecoder.gen[Süß]

          assertTrue(bytes("""{"größe":3,"🔑":"é"}""").fromJson[Süß] == Right(Süß(3, "é")))
        },
        test("decodes every code point in the BMP and a sample above it") {
          // excludes the surrogate range, which is not encodable
          val bmp    = ((0x20 to 0xd7ff) ++ (0xe000 to 0xffff)).map(_.toChar.toString)
          val astral = (0x10000 to 0x10ffff by 997).map(cp => new String(Character.toChars(cp)))

          // Decoded in batches, and only the indices of bad batches reach the assertion. Putting the whole 63k
          // character string in the assertion instead makes zio-test hand it to PrettyPrint, which overflows a
          // StringBuilder on Scala Native.
          def badBatches(cps: Seq[String], size: Int): Seq[Int] =
            cps
              .grouped(size)
              .zipWithIndex
              .collect {
                case (batch, i) if { val s = batch.mkString; bytes(quoted(s)).fromJson[String] != Right(s) } => i
              }
              .toSeq

          assertTrue(
            badBatches(bmp, 512) == Seq.empty[Int],
            badBatches(astral, 64) == Seq.empty[Int]
          )
        },
        test("replaces malformed input, as new String(bytes, UTF_8) does") {
          def decode(bs: Int*): Either[String, String] = rawString(bs: _*).fromJson[String]

          assertTrue(
            decode(0xc3) == Right("\ufffd"),                                     // truncated 2 byte sequence
            decode(0x80) == Right("\ufffd"),                                     // stray continuation byte
            decode(0xbf) == Right("\ufffd"),                                     // stray continuation byte, high
            decode(0xc3, 0x28) == Right("\ufffd("),                              // bad continuation, byte re-examined
            decode(0xc0, 0x80) == Right("\ufffd\ufffd"),                         // overlong NUL
            decode(0xc1, 0xbf) == Right("\ufffd\ufffd"),                         // overlong solidus
            decode(0xe0, 0x80, 0x80) == Right("\ufffd\ufffd\ufffd"),             // overlong, 3 byte
            decode(0xe0, 0xa0) == Right("\ufffd"),                               // truncated 3 byte sequence
            decode(0xe2, 0x82) == Right("\ufffd"),                               // truncated euro sign
            decode(0xf0, 0x9f, 0x98) == Right("\ufffd"),                         // truncated 4 byte sequence
            decode(0xf0, 0x80, 0x80, 0x80) == Right("\ufffd\ufffd\ufffd\ufffd"), // overlong, 4 byte
            decode(0xf5, 0x80, 0x80, 0x80) == Right("\ufffd\ufffd\ufffd\ufffd"), // beyond U+10FFFF
            decode(0xff) == Right("\ufffd"),                                     // never valid in UTF-8
            decode(0xfe) == Right("\ufffd"),
            decode(0xed, 0xa0, 0x80) == Right("\ufffd"),   // CESU-8 surrogate, one subpart of three
            decode(0x41, 0x80, 0x42) == Right("A\ufffdB"), // recovers to the next valid byte
            decode(0xc3, 0xa9, 0x80, 0xc3, 0xa9) == Right("\u00e9\ufffd\u00e9")
          )
        },
        test("matches CharsetDecoder on every one and two byte sequence") {
          // exhaustive over lead bytes, and over second bytes that a JSON string literal can carry
          val leads   = 0x80 to 0xff
          val seconds = (0x20 to 0xff).filter(b => b != '"'.toInt && b != '\\'.toInt)
          val singles = leads.map(b => Chunk.single(b.toByte))
          val pairs   = for { a <- leads; b <- seconds } yield Chunk(a.toByte, b.toByte)

          assertTrue((singles ++ pairs).filterNot(agreesWithJdk).isEmpty)
        } @@ TestAspect.jvmOnly,
        test("matches CharsetDecoder on three and four byte sequences") {
          val leads  = 0xc0 to 0xff
          val probes = Seq(0x80, 0x8f, 0x90, 0x9f, 0xa0, 0xbf, 0xc0, 0x41, 0xff)
          val threes = for { a <- leads; b <- probes; c <- probes } yield Chunk(a.toByte, b.toByte, c.toByte)
          val fours  =
            for { a <- leads; b <- probes; c <- probes } yield Chunk(a.toByte, b.toByte, c.toByte, 0x80.toByte)

          assertTrue((threes ++ fours).filterNot(agreesWithJdk).map(hex).isEmpty)
        } @@ TestAspect.jvmOnly,
        test("decodes arbitrary bytes the same way whatever the chunk shape") {
          check(genStringBytes) { bs =>
            val results = shapes(rawString(bs).toArray).map { case (_, c) => c.fromJson[String] }

            assertTrue(results.distinct.size == 1)
          }
        },
        test("agrees with the CharSequence path on arbitrary bytes inside a string literal") {
          // most of these are malformed; both sides see exactly the same bytes, so they must still agree
          check(genStringBytes)(bs => parity[String](rawString(bs).toArray))
        } @@ TestAspect.jvmOnly
      ),
      suite("Chunk shapes")(
        test("decodes identically whatever shape the chunk has") {
          parityOf[Person]("""{"name":"aé中😀z","age":42}""") &&
          parityOf[Json]("""[1,"é",{"a":[true,null]},2.5]""") &&
          parityOf[List[Int]]((1 to 500).toList.toJson)
        },
        test("decodes concatenations split at every byte offset") {
          val all = utf8("""{"name":"aé中😀z","age":42}""")

          // including split points that fall inside a multi-byte sequence
          assertTrue((0 to all.length).forall { at =>
            val c = Chunk.fromArray(all.take(at)) ++ Chunk.fromArray(all.drop(at))
            c.fromJson[Person] == Right(Person("aé中😀z", 42))
          })
        },
        test("decodes from a bare Array[Byte] too") {
          val arr = utf8("""{"name":"é","age":42}""")

          assertTrue(
            arr.fromJson[Person] == Right(Person("é", 42)),
            JsonDecoder[Person].decodeJson(arr) == Right(Person("é", 42)),
            arr.fromJson[Person] == Chunk.fromArray(arr).fromJson[Person],
            Array.empty[Byte].fromJson[Person] == Left("Unexpected end of input")
          )
        },
        test("handles empty and single byte chunks") {
          assertTrue(
            Chunk.empty[Byte].fromJson[Person] == Left("Unexpected end of input"),
            Chunk.single('1'.toByte).fromJson[Int] == Right(1),
            Chunk.empty[Byte].fromJson[Json] == "".fromJson[Json]
          ) &&
          parityOf[Int]("7")
        }
      ),
      suite("window boundaries")(
        test("decodes payloads either side of the window size") {
          // the payload is padded to land exactly on, just under and just over one window
          val sizes = List(Window - 1, Window, Window + 1, 2 * Window, 2 * Window + 1)

          assertTrue(sizes.forall { size =>
            val filler = "x" * (size - utf8("""{"name":"","age":42}""").length)
            val doc    = s"""{"name":"$filler","age":42}"""

            bytes(doc).fromJson[Person] == Right(Person(filler, 42))
          })
        },
        test("decodes multi-byte characters straddling the boundary at every alignment") {
          // the padding shifts every sequence along by a byte, so a 2, 3 and 4 byte sequence each land on the split
          val texts = (0 to 3).map(pad => "a" * pad + "é中😀" * 4000)

          assertTrue(
            texts.forall(t => bytes(quoted(t)).fromJson[String] == Right(t)),
            texts.forall { t =>
              val bs = utf8(quoted(t))
              (Chunk.fromArray(bs.take(Window)) ++ Chunk.fromArray(bs.drop(Window))).fromJson[String] == Right(t)
            }
          )
        },
        test("decodes payloads larger than the window") {
          // numbers have no terminator, so every element retracts, including across a refill
          val ints = (1 to 20000).toList
          val all  = utf8(ints.toJson)

          assertTrue(
            all.length > Window,
            shapes(all).forall { case (_, c) => c.fromJson[List[Int]] == Right(ints) }
          )
        },
        test("retracts to a position the window has already moved past") {
          // more leading whitespace than the window holds, then a number running to the very end of input: the number
          // retracts after hitting the end, by which point the window no longer covers the retract target
          val padded = " " * (3 * Window) + "42"

          assertTrue(bytes(padded).fromJson[Int] == Right(42)) &&
          parityOf[Int](padded) &&
          parityOf[Int](" " * (3 * Window))
        }
      ),
      suite("parser interaction")(
        test("skips whitespace") {
          val json = "  {\n\t\"name\" : \"Jules\" ,\r\n  \"age\" : 42\n}  "

          assertTrue(bytes(json).fromJson[Person] == Right(Person("Jules", 42))) &&
          parityOf[Person](json)
        },
        test("retracts at the end of input") {
          // numbers have no terminator, so the parser reads one char past them and retracts
          assertTrue(
            bytes("42").fromJson[Int] == Right(42),
            bytes("-1.5e3").fromJson[Double] == Right(-1500.0),
            bytes("[1,2,3]").fromJson[List[Int]] == Right(List(1, 2, 3)),
            bytes(" 42 ").fromJson[Int] == Right(42)
          ) &&
          parityOf[Int]("42") &&
          parityOf[List[Int]]("[1,2,3]")
        },
        test("supports decoders that rewind") {
          // orElse wraps the reader in a RecordingReader, which retracts and replays
          implicit val decoder: JsonDecoder[Either[Int, Person]] = JsonDecoder[Int].orElseEither(JsonDecoder[Person])

          assertTrue(
            bytes("42").fromJson[Either[Int, Person]] == Right(Left(42)),
            bytes("""{"name":"é","age":42}""").fromJson[Either[Int, Person]] == Right(Right(Person("é", 42)))
          ) &&
          // long enough that the rewind spans a window refill
          parity[Either[Int, Person]](utf8(s"""{"name":"${"é" * Window}","age":1}"""))
        },
        test("rewinds after a multi-byte character") {
          // the retract lands mid surrogate pair, which is the one case the position alone does not describe
          implicit val decoder: JsonDecoder[Either[Int, String]] = JsonDecoder[Int].orElseEither(JsonDecoder[String])

          assertTrue(bytes(quoted("😀é")).fromJson[Either[Int, String]] == Right(Right("😀é")))
        }
      ),
      suite("errors")(
        test("reports the same errors as the CharSequence path") {
          val cases = List(
            "",
            "   ",
            "\n\t",
            """{""",
            """{"name":"x","age":""",
            """{"name":"x","age":"nope"}""",
            """{"name":"x"}""",
            """{"name":"x","age":1""",
            """{"name":"x","age":1}}""",
            """{"name":"x","age":1,}""",
            """[1,2,""",
            """{"name":"é","age":"nope"}""",
            """{"name":"é😀","age":""",
            """"unterminated""",
            """{"name":"x","age":1.2.3}""",
            """tru""",
            """nul"""
          )

          cases.map(parityOf[Person](_)).reduce(_ && _)
        },
        test("reports the documented error strings") {
          assertTrue(
            bytes("").fromJson[Person] == Left("Unexpected end of input"),
            bytes("""{"name":"x","age":"nope"}""").fromJson[Person] == Left(".age(expected an Int)"),
            bytes("""{"name":"x"}""").fromJson[Person] == Left(".age(missing)")
          )
        },
        test("agrees with the CharSequence path on every truncation") {
          // truncating at every byte offset walks the parser into each of its end-of-input and retract corners
          val docs = List(
            """{"name":"Jules","age":42}""",
            """{"name":"aé中😀z","age":-1}""",
            """ { "name" : "x" , "age" : 42 } """,
            """{"age":42,"name":"x","extra":[1,2,{"a":null}]}""",
            """[1,2.5,true,null,"é",{"a":[]}]"""
          )

          assertTrue(docs.forall { doc =>
            val all = utf8(doc)
            (0 to all.length).forall { n =>
              val prefix   = all.take(n)
              val expected = new String(prefix, UTF_8)
              // compared against the same bytes seen as a String, so a split multi-byte char is not the difference
              prefix2Chunks(prefix).forall(c =>
                c.fromJson[Person] == expected.fromJson[Person] && c.fromJson[Json] == expected.fromJson[Json]
              )
            }
          })
        }
      ),
      suite("properties")(
        test("agrees with the CharSequence path on generated values") {
          check(Gen.string(Gen.unicodeChar), Gen.int) { (name, age) =>
            parity[Person](utf8(Person(name, age).toJson))
          }
        },
        test("agrees with the CharSequence path on generated ASTs") {
          check(genJson(3)) { json =>
            parity[Json](utf8(json.toString))
          }
        },
        test("round trips generated strings") {
          check(Gen.string(Gen.unicodeChar)) { s =>
            assertTrue(bytes(quoted(s)).fromJson[String] == Right(s))
          }
        }
      )
    )

  /**
   * Feeds raw bytes through a JSON string literal and checks the decoded text against what the JDK's own UTF-8 decoder
   * makes of the same bytes. JVM only: other platforms do not promise CharsetDecoder's replacement rules.
   */
  private def agreesWithJdk(bs: Chunk[Byte]): Boolean =
    rawString(bs).fromJson[String] == Right(new String(bs.toArray, UTF_8))

  private def hex(bs: Chunk[Byte]): String = bs.map(b => f"${b & 0xff}%02X").mkString(" ")

  private def genJson(depth: Int): Gen[Any, Json] = {
    val leaf: Gen[Any, Json] = Gen.oneOf(
      Gen.const(Json.Null),
      Gen.boolean.map(Json.Bool(_)),
      Gen.int(-100000, 100000).map(i => Json.Num(i)),
      Gen.string(Gen.unicodeChar).map(Json.Str(_))
    )

    if (depth <= 0) leaf
    else {
      val field = for {
        k <- Gen.alphaNumericString
        v <- genJson(depth - 1)
      } yield k -> v

      Gen.oneOf(
        leaf,
        Gen.chunkOfBounded(0, 4)(genJson(depth - 1)).map(Json.Arr(_)),
        // duplicate keys make a Json.Obj unequal even to itself, since Json.equals collapses fields into a Map
        Gen.chunkOfBounded(0, 4)(field).map(fs => Json.Obj(fs.zipWithIndex.map { case ((k, v), n) => s"$k$n" -> v }))
      )
    }
  }

  /** Both code paths, for the truncation sweep, without paying for the full shape list on every prefix. */
  private def prefix2Chunks(bs: Array[Byte]): List[Chunk[Byte]] = {
    val half = bs.length / 2
    List(Chunk.fromArray(bs), Chunk.fromArray(bs.take(half)) ++ Chunk.fromArray(bs.drop(half)))
  }
}
