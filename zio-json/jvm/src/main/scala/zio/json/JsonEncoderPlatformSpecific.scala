package zio.json

import zio.json.internal.WriteWriter
import zio.stream._
import zio.{ Chunk, Ref, Unsafe, ZIO }

trait JsonEncoderPlatformSpecific[A] { self: JsonEncoder[A] =>

  /**
   * Encodes the specified value into a character stream.
   */
  final def encodeJsonStream(a: A): ZStream[Any, Throwable, Char] =
    ZStream(a).via(encodeJsonDelimitedPipeline(None, None, None))

  final private def encodeJsonDelimitedPipeline(
    startWith: Option[Char],
    delimiter: Option[Char],
    endWith: Option[Char]
  ): ZPipeline[Any, Throwable, A, Char] =
    Unsafe.unsafe { (u: Unsafe) =>
      implicit val unsafe: Unsafe = u

      ZPipeline.fromPush {
        for {
          runtime     <- ZIO.runtime[Any]
          chunkBuffer <- Ref.make(Chunk.fromIterable(startWith.toList))
          writer <- ZIO.fromAutoCloseable {
                      ZIO.succeed {
                        new java.io.BufferedWriter(
                          new java.io.Writer {
                            override def write(buffer: Array[Char], offset: Int, len: Int): Unit = {
                              val copy = new Array[Char](len)
                              System.arraycopy(buffer, offset, copy, 0, len)

                              val chunk = Chunk.fromArray(copy).drop(offset).take(len)
                              runtime.unsafe.run(chunkBuffer.update(_ ++ chunk)).getOrThrow()
                            }

                            override def close(): Unit = ()
                            override def flush(): Unit = ()
                          },
                          ZStream.DefaultChunkSize
                        )
                      }
                    }
          writeWriter          <- ZIO.succeed(new WriteWriter(writer))
          hasAtLeastOneElement <- Ref.make(false)
          push = { (is: Option[Chunk[A]]) =>
            val pushChars = chunkBuffer.getAndUpdate(c => if (c.isEmpty) c else Chunk())

            is match {
              case None =>
                ZIO.attemptBlocking(writer.close()) *> pushChars.flatMap { terminal =>
                  hasAtLeastOneElement.get.map(nonEmptyStream =>
                    endWith.fold(terminal) { last =>
                      // Chop off terminal delimiter if stream is not empty
                      (if (delimiter.isDefined && nonEmptyStream) terminal.dropRight(1) else terminal) :+ last
                    }
                  )
                }

              case Some(xs) =>
                ZIO.attemptBlocking {
                  for (x <- xs) {
                    unsafeEncode(x, indent = None, writeWriter)

                    for (s <- delimiter)
                      writeWriter.write(s)
                  }
                } *> hasAtLeastOneElement.set(true).when(xs.nonEmpty) *> pushChars
            }
          }
        } yield push
      }
    }

  final lazy val encodeJsonLinesPipeline: ZPipeline[Any, Throwable, A, Char] =
    encodeJsonDelimitedPipeline(None, Some('\n'), None)

  final lazy val encodeJsonArrayPipeline: ZPipeline[Any, Throwable, A, Char] =
    encodeJsonDelimitedPipeline(Some('['), Some(','), Some(']'))
}
