package zio

import zio.json.{ JsonDecoder, JsonEncoder, JsonStreamDelimiter, ast }
import zio.stream._

import java.io.{ File, IOException }
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{ Path, Paths }

trait JsonPackagePlatformSpecific {
  def readJsonAs(file: File): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](file)

  def readJsonAs(path: Path): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  def readJsonAs(path: String): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  def readJsonAs(url: URL): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](url)

  def readJsonLinesAs[A: JsonDecoder](file: File): ZStream[Any, Throwable, A] =
    readJsonLinesAs(file.toPath)

  def readJsonLinesAs[A: JsonDecoder](path: Path): ZStream[Any, Throwable, A] =
    ZStream
      .fromPath(path)
      .via(
        ZPipeline.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonPipeline(JsonStreamDelimiter.Newline)
      )

  def readJsonLinesAs[A: JsonDecoder](path: String): ZStream[Any, Throwable, A] =
    readJsonLinesAs(Paths.get(path))

  def readJsonLinesAs[A: JsonDecoder](url: URL): ZStream[Any, Throwable, A] = {
    val scoped = ZIO
      .fromAutoCloseable(ZIO.attempt(url.openStream()))
      .refineToOrDie[IOException]

    ZStream
      .fromInputStreamScoped(scoped)
      .via(
        ZPipeline.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonPipeline(JsonStreamDelimiter.Newline)
      )
  }

  def writeJsonLines[R](file: File, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(file, stream)

  def writeJsonLines[R](path: Path, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(path, stream)

  def writeJsonLines[R](path: String, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(path, stream)

  def writeJsonLinesAs[R, A: JsonEncoder](file: File, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    writeJsonLinesAs(file.toPath, stream)

  def writeJsonLinesAs[R, A: JsonEncoder](path: Path, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    stream
      .via(
        JsonEncoder[A].encodeJsonLinesPipeline >>>
          charsToUtf8
      )
      .run(ZSink.fromPath(path))
      .unit

  def writeJsonLinesAs[R, A: JsonEncoder](path: String, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    writeJsonLinesAs(Paths.get(path), stream)

  private def stringToChars: ZPipeline[Any, Nothing, String, Char] =
    ZPipeline.mapChunks[String, Char](_.flatMap(_.toCharArray))

  private def charsToUtf8: ZPipeline[Any, Nothing, Char, Byte] =
    ZPipeline.mapChunksZIO[Any, Nothing, Char, Byte] { chunk =>
      ZIO.succeed {
        Chunk.fromArray {
          new String(chunk.toArray).getBytes(StandardCharsets.UTF_8)
        }
      }
    }
}
