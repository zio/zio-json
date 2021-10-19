package zio.json

import com.github.ghik.silencer.silent
import zio.json.internal.WriteWriter
import zio.stream._
import zio.{ Chunk, Ref, ZIO, ZManaged }
import zio.Random
import zio.ZIO.attemptBlocking

trait JsonEncoderPlatformSpecific[A] { self: JsonEncoder[A] =>

  /**
   * Encodes the specified value into a character stream.
   */
  final def encodeJsonStream(a: A): ZStream[Any, Throwable, Char] =
    ZStream(a).transduce(encodeJsonDelimitedTransducer(None, None, None))

  final private def encodeJsonDelimitedTransducer(
    startWith: Option[Char],
    delimiter: Option[Char],
    endWith: Option[Char]
  ): ZTransducer[Any, Throwable, A, Char] =
    ZTransducer {
      for {
        runtime     <- ZIO.runtime[Any].toManaged
        chunkBuffer <- Ref.makeManaged(Chunk.fromIterable(startWith.toList))
        writer <- ZManaged.fromAutoCloseable {
                    ZIO.succeed {
                      new java.io.BufferedWriter(
                        new java.io.Writer {
                          override def write(buffer: Array[Char], offset: Int, len: Int): Unit = {
                            val copy = new Array[Char](len)
                            System.arraycopy(buffer, offset, copy, 0, len)

                            val chunk = Chunk.fromArray(copy).drop(offset).take(len)
                            runtime.unsafeRun(chunkBuffer.update(_ ++ chunk))
                          }

                          override def close(): Unit = ()
                          override def flush(): Unit = ()
                        },
                        ZStream.DefaultChunkSize
                      )
                    }
                  }
        writeWriter <- ZManaged.succeed(new WriteWriter(writer))
        push = { (is: Option[Chunk[A]]) =>
          val pushChars = chunkBuffer.getAndUpdate(c => if (c.isEmpty) c else Chunk())

          is match {
            case None =>
              attemptBlocking(writer.close()) *> pushChars.map { terminal =>
                endWith.fold(terminal) { last =>
                  // Chop off terminal deliminator
                  (if (delimiter.isDefined) terminal.dropRight(1) else terminal) :+ last
                }
              }

            case Some(xs) =>
              attemptBlocking {
                for (x <- xs) {
                  unsafeEncode(x, indent = None, writeWriter)

                  for (s <- delimiter)
                    writeWriter.write(s)
                }
              } *> pushChars
          }
        }
      } yield push
    }

  final val encodeJsonLinesTransducer: ZTransducer[Any, Throwable, A, Char] =
    encodeJsonDelimitedTransducer(None, Some('\n'), None)

  final val encodeJsonArrayTransducer: ZTransducer[Any, Throwable, A, Char] =
    encodeJsonDelimitedTransducer(Some('['), Some(','), Some(']'))
}
