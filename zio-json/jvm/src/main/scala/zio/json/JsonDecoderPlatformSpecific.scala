package zio.json

import zio._
import zio.blocking._
import zio.json.JsonDecoder.JsonError
import zio.json.internal._
import zio.stream.{ Take, ZStream, ZTransducer }

import java.nio.charset.{ Charset, StandardCharsets }
import scala.annotation.tailrec

trait JsonDecoderPlatformSpecific[A] { self: JsonDecoder[A] =>

  private def readAll(reader: java.io.Reader): ZIO[Blocking, Throwable, A] =
    effectBlocking {
      try unsafeDecode(Nil, new zio.json.internal.WithRetractReader(reader))
      catch {
        case JsonDecoder.UnsafeJson(trace)      => throw new Exception(JsonError.render(trace))
        case _: zio.json.internal.UnexpectedEnd => throw new Exception("unexpected end of input")
      }
    }

  /**
   * Attempts to decode a stream of bytes using the user supplied Charset into a single value of type `A`, but may fail with
   * a human-readable exception if the stream does not encode a value of this type.
   *
   * Note: This method may not consume the full string.
   *
   * @see [[decodeJsonStream]] For a `Char` stream variant
   */
  final def decodeJsonStreamInput[R <: Blocking](
    stream: ZStream[R, Throwable, Byte],
    charset: Charset = StandardCharsets.UTF_8
  ): ZIO[R, Throwable, A] =
    stream.toInputStream
      .flatMap(is => ZManaged.fromAutoCloseable(UIO(new java.io.InputStreamReader(is, charset))))
      .use(readAll)

  /**
   * Attempts to decode a stream of characters into a single value of type `A`, but may fail with
   * a human-readable exception if the stream does not encode a value of this type.
   *
   * Note: This method may not consume the full string.
   *
   * @see also [[decodeJsonStreamInput]]
   */
  final def decodeJsonStream[R <: Blocking](stream: ZStream[R, Throwable, Char]): ZIO[R, Throwable, A] =
    stream.toReader.use(readAll)

  final def decodeJsonTransducer(
    delimiter: JsonStreamDelimiter = JsonStreamDelimiter.Array
  ): ZTransducer[Blocking, Throwable, Char, A] =
    ZTransducer {
      for {
        // format: off
        runtime    <- ZIO.runtime[Any].toManaged_
        inQueue    <- Queue.unbounded[Take[Nothing, Char]].toManaged_
        outQueue   <- Queue.unbounded[Take[Throwable, A]].toManaged_
        ended      <- Ref.makeManaged(false)
        reader     <- ZManaged.fromAutoCloseable {
                        ZIO.effectTotal {
                          def readPull: Iterator[Chunk[Char]] =
                            runtime.unsafeRun(inQueue.take)
                              .fold(
                                end   = Iterator.empty,
                                error = _ => Iterator.empty, // impossible
                                value = v => Iterator.single(v) ++ readPull
                              )

                          new zio.stream.internal.ZReader(Iterator.empty ++ readPull)
                        }
                      }
        jsonReader <- ZManaged.fromAutoCloseable(ZIO.effectTotal(new WithRetractReader(reader)))
        process    <- effectBlockingInterrupt {
                        // Exceptions fall through and are pushed into the queue
                        @tailrec def loop(atBeginning: Boolean): Unit = {
                          val nextElem = try {
                            if (atBeginning && delimiter == JsonStreamDelimiter.Array)  {
                              Lexer.char(Nil, jsonReader, '[')
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
                            case t @ JsonDecoder.UnsafeJson(trace) =>
                              throw new Exception(JsonError.render(trace))
                          }

                          runtime.unsafeRun(outQueue.offer(Take.single(nextElem)))

                          loop(false)
                        }

                        loop(true)
                      }
                      .catchAll {
                        case t: zio.json.internal.UnexpectedEnd =>
                          // swallow if stream ended
                          ZIO.unlessM(ended.get) {
                            outQueue.offer(Take.fail(t))
                          }

                        case t: Throwable =>
                          outQueue.offer(Take.fail(t))
                      }
                      .interruptible
                      .forkManaged
        push = { is: Option[Chunk[Char]] =>
          val pollElements: IO[Throwable, Chunk[A]] =
            outQueue
              .takeUpTo(ZStream.DefaultChunkSize)
              .flatMap { takes =>
                ZIO.foldLeft(takes)(Chunk[A]()) { case (acc, take) =>
                  take.fold(ZIO.succeedNow(acc), e => ZIO.fail(e.squash), c => ZIO.succeedNow(acc ++ c))
                }
              }

          val pullRest =
            outQueue
              .takeAll
              .flatMap { takes =>
                ZIO.foldLeft(takes)(Chunk[A]()) { case (acc, take) =>
                  take.fold(ZIO.succeedNow(acc), e => ZIO.fail(e.squash), c => ZIO.succeedNow(acc ++ c))
                }
              }

          is match {
            case Some(c) =>
              inQueue.offer(Take.chunk(c)) *> pollElements

            case None =>
              ended.set(true) *> inQueue.offer(Take.end) *> process.join *> pullRest
          }
        }
      } yield push
      // format: on
    }
}
