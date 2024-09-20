package testzio.json

import zio._
import zio.json._
import zio.json.ast._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import java.nio.charset.StandardCharsets

object StreamDecoderSpec extends ZIOSpecDefault {

  val spec =
    suite("Decoder")(
      suite("ZIO Streams integration")(
        test("decodes a stream of chars") {
          for {
            int <- JsonDecoder[Int].decodeJsonStream(ZStream('1', '2', '3'))
          } yield {
            assert(int)(equalTo(123))
          }
        },
        test("decodes an encoded stream of bytes") {
          for {
            int <- JsonDecoder[Int].decodeJsonStreamInput(ZStream.fromIterable("123".getBytes(StandardCharsets.UTF_8)))
          } yield assert(int)(equalTo(123))
        },
        suite("decodeJsonPipeline")(
          suite("Newline delimited")(
            test("decodes single elements") {
              ZStream
                .fromIterable("1001".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001)))
                }
            },
            test("decodes multiple elements") {
              ZStream
                .fromIterable("1001\n1002".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            test("decodes multiple elements when fed in smaller chunks") {
              ZStream
                .fromIterable("1001\n1002".toSeq)
                .rechunk(1)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            test("accepts trailing NL") {
              ZStream
                .fromIterable("1001\n1002\n".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002)))
                }
            },
            test("errors") {
              ZStream
                .fromIterable("1\nfalse\n3".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runDrain
                .exit
                .map { exit =>
                  assert(exit)(fails(anything))
                }
            },
            test("is interruptible") {
              (ZStream.fromIterable("1\n2\n3\n4") ++ ZStream.fromZIO(ZIO.interrupt))
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Newline))
                .runDrain
                .exit
                .map { exit =>
                  assert(exit)(isInterrupted)
                }
            } @@ timeout(2.seconds)
          ),
          suite("Array delimited")(
            test("decodes single elements") {
              ZStream
                .fromIterable("[1001]".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001)))
                }
            },
            test("empty array") {
              ZStream
                .fromIterable("[]".toSeq)
                .via(JsonDecoder[String].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(isEmpty)
                }
            },
            test("decodes multiple elements") {
              ZStream
                .fromIterable("[ 1001, 1002, 1003 ]".toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002, 1003)))
                }
            },
            test("handles whitespace leniently") {
              val in =
                """[
                  1001, 1002,
                  1003
                ]"""

              ZStream
                .fromIterable(in.toSeq)
                .via(JsonDecoder[Int].decodeJsonPipeline(JsonStreamDelimiter.Array))
                .runCollect
                .map { xs =>
                  assert(xs)(equalTo(Chunk(1001, 1002, 1003)))
                }
            }
          )
        )
      )
    )
}
