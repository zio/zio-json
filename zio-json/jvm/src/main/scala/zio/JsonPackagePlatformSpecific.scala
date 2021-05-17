package zio

import zio.blocking.Blocking
import zio.json.{ JsonDecoder, JsonEncoder, JsonStreamDelimiter, ast }
import zio.stream._

import java.io.{ File, IOException }
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{ Path, Paths }

trait JsonPackagePlatformSpecific {
  def readJsonAs(file: File): ZStream[Blocking, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](file)

  def readJsonAs(path: Path): ZStream[Blocking, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  def readJsonAs(path: String): ZStream[Blocking, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  def readJsonAs(url: URL): ZStream[Blocking, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](url)

  def readJsonLinesAs[A: JsonDecoder](file: File): ZStream[Blocking, Throwable, A] =
    readJsonLinesAs(file.toPath)

  def readJsonLinesAs[A: JsonDecoder](path: Path): ZStream[Blocking, Throwable, A] =
    ZStream
      .fromFile(path)
      .transduce(
        ZTransducer.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonTransducer(JsonStreamDelimiter.Newline)
      )

  def readJsonLinesAs[A: JsonDecoder](path: String): ZStream[Blocking, Throwable, A] =
    readJsonLinesAs(Paths.get(path))

  def readJsonLinesAs[A: JsonDecoder](url: URL): ZStream[Blocking, Throwable, A] = {
    val managed = ZManaged
      .fromAutoCloseable(ZIO.effect(url.openStream()))
      .refineToOrDie[IOException]

    ZStream
      .fromInputStreamManaged(managed)
      .transduce(
        ZTransducer.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonTransducer(JsonStreamDelimiter.Newline)
      )
  }

  def writeJsonLines[R <: Blocking](file: File, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(file, stream)

  def writeJsonLines[R <: Blocking](path: Path, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(path, stream)

  def writeJsonLines[R <: Blocking](path: String, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(path, stream)

  def writeJsonLinesAs[R <: Blocking, A: JsonEncoder](file: File, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    writeJsonLinesAs(file.toPath, stream)

  def writeJsonLinesAs[R <: Blocking, A: JsonEncoder](path: Path, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    stream
      .transduce(
        JsonEncoder[A].encodeJsonLinesTransducer >>>
          charsToUtf8
      )
      .run(ZSink.fromFile(path))
      .unit

  def writeJsonLinesAs[R <: Blocking, A: JsonEncoder](path: String, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    writeJsonLinesAs(Paths.get(path), stream)

  private def stringToChars: ZTransducer[Any, Nothing, String, Char] =
    ZTransducer
      .fromFunction[String, Chunk[Char]](s => Chunk.fromArray(s.toCharArray))
      .mapChunks(_.flatten)

  private def charsToUtf8: ZTransducer[Any, Nothing, Char, Byte] =
    ZTransducer.fromPush {
      case None =>
        ZIO.succeed(Chunk.empty)

      case Some(xs) =>
        ZIO.effectTotal {
          Chunk.fromArray((new String(xs.toArray)).getBytes(StandardCharsets.UTF_8))
        }
    }
}
