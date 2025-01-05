package zio.json

import zio._
import zio.json.JsonDecoder.JsonError
import zio.json.internal._
import zio.stream.{ Take, ZPipeline, ZStream }

import java.nio.charset.{ Charset, StandardCharsets }
import scala.annotation.tailrec

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
   * Note: This method may not consume the full string.
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
   * Note: This method may not consume the full string.
   *
   * @see
   *   also [[decodeJsonStreamInput]]
   */
  final def decodeJsonStream[R](stream: ZStream[R, Throwable, Char]): ZIO[R, Throwable, A] =
    ZIO.scoped[R](stream.toReader.flatMap(readAll))

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
                case t @ JsonDecoder.UnsafeJson(trace) =>
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

              case None =>
                ended.set(true) *> inQueue.offer(Take.end) *> process.join *> pullRest
            }
          }
        } yield push
        // format: on
      }
    }
  }
}
