package zio

import zio.json.{ JsonDecoder, JsonEncoder, JsonStreamDelimiter, ast }
import zio.stream._

import java.io.{ File, IOException }
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{ Path, Paths }

trait JsonPackagePlatformSpecific {

  /**
   * Streams newline-delimited JSON values from a file.
   *
   * Each line in the file is expected to be a complete, valid JSON value. This is the "JSON Lines" (NDJSON) format. For
   * streaming elements from a JSON array file, use [[readJsonArrayAs]] instead.
   */
  def readJsonAs(file: File): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](file)

  /**
   * Streams newline-delimited JSON values from a file path.
   *
   * Each line in the file is expected to be a complete, valid JSON value. This is the "JSON Lines" (NDJSON) format. For
   * streaming elements from a JSON array file, use [[readJsonArrayAs]] instead.
   */
  def readJsonAs(path: Path): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  /**
   * Streams newline-delimited JSON values from a file path string.
   *
   * Each line in the file is expected to be a complete, valid JSON value. This is the "JSON Lines" (NDJSON) format. For
   * streaming elements from a JSON array file, use [[readJsonArrayAs]] instead.
   */
  def readJsonAs(path: String): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  /**
   * Streams newline-delimited JSON values from a URL.
   *
   * Each line is expected to be a complete, valid JSON value (JSON Lines / NDJSON format). For streaming elements from
   * a JSON array, use [[readJsonArrayAs]] instead.
   */
  def readJsonAs(url: URL): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](url)

  /**
   * Streams individual elements from a JSON array file.
   *
   * The file must contain a single top-level JSON array. Each element of the array is decoded and emitted as a separate
   * stream element, enabling constant-memory processing of large JSON array files.
   *
   * For newline-delimited JSON (JSON Lines / NDJSON), use [[readJsonAs]] instead.
   */
  def readJsonArrayAs[A: JsonDecoder](file: File): ZStream[Any, Throwable, A] =
    readJsonArrayAs(file.toPath)

  /**
   * Streams individual elements from a JSON array file path.
   *
   * The file must contain a single top-level JSON array. Each element of the array is decoded and emitted as a separate
   * stream element, enabling constant-memory processing of large JSON array files.
   *
   * For newline-delimited JSON (JSON Lines / NDJSON), use [[readJsonAs]] instead.
   */
  def readJsonArrayAs[A: JsonDecoder](path: Path): ZStream[Any, Throwable, A] =
    ZStream
      .fromPath(path)
      .via(
        ZPipeline.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonPipeline(JsonStreamDelimiter.Array)
      )

  /**
   * Streams individual elements from a JSON array file path string.
   *
   * The file must contain a single top-level JSON array. Each element is decoded and emitted as a separate stream
   * element. For newline-delimited JSON, use [[readJsonAs]].
   */
  def readJsonArrayAs[A: JsonDecoder](path: String): ZStream[Any, Throwable, A] =
    readJsonArrayAs(Paths.get(path))

  /**
   * Streams individual elements from a JSON array URL.
   *
   * The response must contain a single top-level JSON array. Each element is decoded and emitted as a separate stream
   * element. For newline-delimited JSON, use [[readJsonAs]].
   */
  def readJsonArrayAs[A: JsonDecoder](url: URL): ZStream[Any, Throwable, A] = {
    val scoped = ZIO
      .fromAutoCloseable(ZIO.attempt(url.openStream()))
      .refineToOrDie[IOException]

    ZStream
      .fromInputStreamScoped(scoped)
      .via(
        ZPipeline.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonPipeline(JsonStreamDelimiter.Array)
      )
  }

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
