package zio.json

import zio.blocking._
import zio.json.internal.WriteWriter
import zio.stream._
import zio.{ Chunk, Ref, ZIO, ZManaged }

trait JsonEncoderPlatformSpecific[A] { self: JsonEncoder[A] =>

  /**
   * Encodes the specified value into a character stream.
   */
  final def encodeJsonStream(a: A, indent: Option[Int]): ZStream[Blocking, Throwable, Char] =
    ZStream(a).transduce(encodeJsonDelimitedTransducer(None, None, None))

  final private def encodeJsonDelimitedTransducer(
    startWith: Option[Char],
    delimiter: Option[Char],
    endWith: Option[Char]
  ): ZTransducer[Blocking, Throwable, A, Char] =
    ZTransducer {
      for {
        runtime     <- ZIO.runtime[Any].toManaged_
        chunkBuffer <- Ref.makeManaged(Chunk.fromIterable(startWith.fold(List.empty[Char])(_ :: Nil)))
        writer <- ZManaged.fromAutoCloseable {
                    ZIO.effectTotal {
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
        push = { is: Option[Chunk[A]] =>
          val pushChars = chunkBuffer.getAndUpdate(c => if (c.isEmpty) c else Chunk())

          is match {
            case None =>
              effectBlocking(writer.close()) *> pushChars.map { terminal =>
                endWith.fold(terminal) { last =>
                  // Chop off terminal deliminator
                  (if (delimiter.isDefined) terminal.dropRight(1) else terminal) :+ last
                }
              }

            case Some(xs) =>
              effectBlocking {
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

  final val encodeJsonLinesTransducer: ZTransducer[Blocking, Throwable, A, Char] =
    encodeJsonDelimitedTransducer(None, Some('\n'), None)

  final val encodeJsonArrayTransducer: ZTransducer[Blocking, Throwable, A, Char] =
    encodeJsonDelimitedTransducer(Some('['), Some(','), Some(']'))
}
