package zio.json

import zio._
import zio.json.JsonDecoder.JsonError
import zio.json.internal._
import zio.stream.{ Take, ZPipeline, ZStream }

import java.nio.charset.{ Charset, StandardCharsets }
import scala.annotation.tailrec

/**
 * The platform-specific methods of [[JsonDecoder]]. On the JVM they decode from ZIO streams: `decodeJsonStream`,
 * `decodeJsonStreamInput` and `decodeJsonPipeline`. On Scala.js and Scala Native the trait is empty.
 *
 * @see
 *   [[https://zio.dev/zio-json/streaming/ Streaming]]
 */
trait JsonDecoderPlatformSpecific[A] { self: JsonDecoder[A] =>

  private def readAll(reader: java.io.Reader): ZIO[Any, Throwable, A] =
    ZIO.attemptBlocking {
      try unsafeDecode(Nil, new zio.json.internal.WithRetractReader(reader))
      catch {
        case JsonDecoder.UnsafeJson(trace)      => throw new Exception(JsonError.render(trace))
        case _: zio.json.internal.UnexpectedEnd => throw new Exception("unexpected end of input")
      }
    }

  /**
   * Attempts to decode a stream of bytes using the user supplied Charset into a single value of type `A`, but may fail
   * with a human-readable exception if the stream does not encode a value of this type.
   *
   * The exception is a `java.lang.Exception` whose message is the rendered error path, for example `.value(missing)`,
   * or `unexpected end of input` when the stream ends in the middle of the value. A failure of the stream itself is
   * passed through. The charset defaults to UTF-8.
   *
   * Note: This method may not consume the full stream, and ignores what follows the first value.
   *
   * @see
   *   [[decodeJsonStream]] For a `Char` stream variant
   */
  final def decodeJsonStreamInput[R](
    stream: ZStream[R, Throwable, Byte],
    charset: Charset = StandardCharsets.UTF_8
  ): ZIO[R, Throwable, A] =
    ZIO.scoped[R] {
      stream.toInputStream
        .flatMap(is =>
          ZIO
            .fromAutoCloseable(ZIO.succeed(new java.io.InputStreamReader(is, charset)))
            .flatMap(readAll)
        )
    }

  /**
   * Attempts to decode a stream of characters into a single value of type `A`, but may fail with a human-readable
   * exception if the stream does not encode a value of this type.
   *
   * The exception is a `java.lang.Exception` whose message is the rendered error path, for example `.value(missing)`,
   * or `unexpected end of input` when the stream ends in the middle of the value.
   *
   * Note: This method may not consume the full stream, and ignores what follows the first value.
   *
   * @see
   *   also [[decodeJsonStreamInput]]
   */
  final def decodeJsonStream[R](stream: ZStream[R, Throwable, Char]): ZIO[R, Throwable, A] =
    ZIO.scoped[R](stream.toReader.flatMap(readAll))

  /**
   * Returns a pipeline that turns a stream of characters into a stream of decoded values, one per JSON value in the
   * input.
   *
   * With `JsonStreamDelimiter.Array`, the default, the input is one JSON array and each element is emitted. With
   * `JsonStreamDelimiter.Newline`, the input is a sequence of JSON values separated by newlines (`\n` or `\r\n`);
   * values separated only by spaces are accepted too.
   *
   * The stream fails with a `java.lang.Exception` whose message is the rendered error path, for example
   * `(expected an Int)`, at the first value that does not decode, and emits nothing after it. A known bug: when the
   * input ends before the closing `]`, or in the middle of the last value, the stream ends without an error and the
   * incomplete value is dropped.
   *
   * {{{
   * import zio.stream.ZStream
   *
   * final case class Reading(sensor: String, value: Double)
   *
   * implicit val decoder: JsonDecoder[Reading] = DeriveJsonDecoder.gen[Reading]
   *
   * val readings = ZStream
   *   .fromIterable("""[{"sensor":"a","value":1.5},{"sensor":"b","value":2.0}]""")
   *   .via(JsonDecoder[Reading].decodeJsonPipeline())
   *   .runCollect
   * // succeeds with Chunk(Reading(a,1.5),Reading(b,2.0))
   * }}}
   */
  final def decodeJsonPipeline(
    delimiter: JsonStreamDelimiter = JsonStreamDelimiter.Array
  ): ZPipeline[Any, Throwable, Char, A] = {
    Unsafe.unsafe { (u: Unsafe) =>
      implicit val unsafe: Unsafe = u

      ZPipeline.fromPush {
        for {
          // format: off
          runtime    <- ZIO.runtime[Any]
          inQueue    <- Queue.unbounded[Take[Nothing, Char]]
          outQueue   <- Queue.unbounded[Take[Throwable, A]]
          ended      <- Ref.make(false)
          reader     <- ZIO.fromAutoCloseable {
            ZIO.succeed {
              def readPull: Iterator[Chunk[Char]] =
                runtime.unsafe.run(inQueue.take).getOrThrow()
                  .fold(
                    end   = Iterator.empty,
                    error = _ =>  Iterator.empty, // impossible
                    value = v => Iterator.single(v) ++ readPull
                  )

              new zio.stream.internal.ZReader(Iterator.empty ++ readPull)
            }
          }
          jsonReader <- ZIO.fromAutoCloseable(ZIO.succeed(new WithRetractReader(reader)))
          process    <- ZIO.attemptBlockingInterrupt {
            // Exceptions fall through and are pushed into the queue
            @tailrec def loop(atBeginning: Boolean): Unit = {
              val nextElem = try {
                if (atBeginning && delimiter == JsonStreamDelimiter.Array)  {
                  Lexer.char(Nil, jsonReader, '[')

                  jsonReader.nextNonWhitespace() match {
                    case ']' =>
                      // returning empty here instead of falling through, which would
                      // attempt to decode a value that we know doesn’t exist.
                      return ()

                    case _ =>
                      jsonReader.retract()
                  }
                } else {
                  delimiter match {
                    case JsonStreamDelimiter.Newline =>
                      jsonReader.readChar() match {
                        case '\r' =>
                          jsonReader.readChar() match {
                            case '\n' => ()
                            case _    => jsonReader.retract()
                          }
                        case '\n' => ()
                        case _    => jsonReader.retract()
                      }

                    case JsonStreamDelimiter.Array =>
                      jsonReader.nextNonWhitespace() match {
                        case ',' | ']' => ()
                        case _         => jsonReader.retract()
                      }
                  }
                }

                unsafeDecode(Nil, jsonReader)
              } catch {
                case _ @ JsonDecoder.UnsafeJson(trace) =>
                  throw new Exception(JsonError.render(trace))
              }

              Unsafe.unsafe { (u: Unsafe) =>
                implicit val unsafe: Unsafe = u

                runtime.unsafe.run(outQueue.offer(Take.single(nextElem))).getOrThrow()
              }

              loop(false)
            }

            loop(true)
          }
            .catchAll {
              case t: zio.json.internal.UnexpectedEnd =>
                // swallow if stream ended
                ZIO.unlessZIO(ended.get) {
                  outQueue.offer(Take.fail(t))
                }

              case t: Throwable =>
                outQueue.offer(Take.fail(t))
            }
            .interruptible
            .forkScoped
          push = { (is: Option[Chunk[Char]]) =>
            val pollElements: IO[Throwable, Chunk[A]] =
              outQueue
                .takeUpTo(ZStream.DefaultChunkSize)
                .flatMap { takes =>
                  ZIO.foldLeft(takes)(Chunk[A]()) { case (acc, take) =>
                    take.fold(ZIO.succeed(acc), e => ZIO.fail(e.squash), c => ZIO.succeed(acc ++ c))
                  }
                }

            val pullRest =
              outQueue
                .takeAll
                .flatMap { takes =>
                  ZIO.foldLeft(takes)(Chunk[A]()) { case (acc, take) =>
                    take.fold(ZIO.succeed(acc), e => ZIO.fail(e.squash), c => ZIO.succeed(acc ++ c))
                  }
                }

            is match {
              case Some(c) =>
                inQueue.offer(Take.chunk(c)) *> pollElements
              case _ =>
                ended.set(true) *> inQueue.offer(Take.end) *> process.join *> pullRest
            }
          }
        } yield push
        // format: on
      }
    }
  }
}
