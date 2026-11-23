package zio

import zio.json.{ JsonDecoder, JsonEncoder, JsonStreamDelimiter, ast }
import zio.stream._

import java.io.{ File, IOException }
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.{ Path, Paths }

/**
 * Platform-specific helpers of the `zio.json` package.
 *
 * On the JVM they read and write files and URLs as streams of JSON Lines, that is one JSON value per line. Scala.js and
 * Scala Native have none.
 *
 * {{{
 * import zio.json._
 * import zio.json.ast.Json
 * import zio.stream._
 *
 * val write = writeJsonLines("events.jsonl", ZStream(Json.Obj("id" -> Json.Num(1)), Json.Obj("id" -> Json.Num(2))))
 *
 * val read: ZStream[Any, Throwable, Json] = readJsonAs("events.jsonl")
 * }}}
 */
trait JsonPackagePlatformSpecific {
  /**
   * Reads a file of JSON Lines as a stream of [[zio.json.ast.Json]] values.
   *
   * See `readJsonLinesAs` for the format and the failures.
   */
  def readJsonAs(file: File): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](file)

  /**
   * Reads a file of JSON Lines as a stream of [[zio.json.ast.Json]] values.
   *
   * See `readJsonLinesAs` for the format and the failures.
   */
  def readJsonAs(path: Path): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  /**
   * Reads the file at `path` as a stream of [[zio.json.ast.Json]] values, one per line of JSON Lines.
   *
   * See `readJsonLinesAs` for the format and the failures.
   */
  def readJsonAs(path: String): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](path)

  /**
   * Reads the content of `url` as a stream of [[zio.json.ast.Json]] values, one per line of JSON Lines.
   *
   * See `readJsonLinesAs` for the format and the failures.
   */
  def readJsonAs(url: URL): ZStream[Any, Throwable, ast.Json] =
    readJsonLinesAs[ast.Json](url)

  /**
   * Reads a file of JSON Lines as a stream of `A` values.
   */
  def readJsonLinesAs[A: JsonDecoder](file: File): ZStream[Any, Throwable, A] =
    readJsonLinesAs(file.toPath)

  /**
   * Reads a file of JSON Lines as a stream of `A` values.
   *
   * The file is read as UTF-8 and every line is decoded with the implicit `JsonDecoder`, so a line must hold one complete JSON
   * value. Blank lines are skipped. The stream fails with the file error when the file cannot be opened, and with an exception
   * that carries the decoding error message when a line cannot be decoded.
   *
   * {{{
   * import zio.json._
   * import zio.stream._
   *
   * val numbers: ZStream[Any, Throwable, Int] = readJsonLinesAs[Int]("numbers.jsonl")
   * }}}
   */
  def readJsonLinesAs[A: JsonDecoder](path: Path): ZStream[Any, Throwable, A] =
    ZStream
      .fromPath(path)
      .via(
        ZPipeline.utf8Decode >>>
          stringToChars >>>
          JsonDecoder[A].decodeJsonPipeline(JsonStreamDelimiter.Newline)
      )

  /**
   * Reads the file at `path` as a stream of `A` values, decoding each line of JSON Lines with the implicit `JsonDecoder`.
   *
   * See the `Path` overload for the format and the failures.
   */
  def readJsonLinesAs[A: JsonDecoder](path: String): ZStream[Any, Throwable, A] =
    readJsonLinesAs(Paths.get(path))

  /**
   * Reads the content of `url` as a stream of `A` values, decoding each line of JSON Lines with the implicit `JsonDecoder`.
   *
   * The connection is closed when the stream ends or fails.
   */
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

  /**
   * Writes a stream of [[zio.json.ast.Json]] values to a file as JSON Lines.
   *
   * See `writeJsonLinesAs` for the format.
   */
  def writeJsonLines[R](file: File, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(file, stream)

  /**
   * Writes a stream of [[zio.json.ast.Json]] values to a file as JSON Lines.
   *
   * See `writeJsonLinesAs` for the format.
   */
  def writeJsonLines[R](path: Path, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(path, stream)

  /**
   * Writes a stream of [[zio.json.ast.Json]] values to the file at `path` as JSON Lines.
   *
   * See `writeJsonLinesAs` for the format.
   */
  def writeJsonLines[R](path: String, stream: ZStream[R, Throwable, ast.Json]): RIO[R, Unit] =
    writeJsonLinesAs(path, stream)

  /**
   * Writes a stream of `A` values to a file as JSON Lines.
   */
  def writeJsonLinesAs[R, A: JsonEncoder](file: File, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    writeJsonLinesAs(file.toPath, stream)

  /**
   * Writes a stream of `A` values to a file as JSON Lines.
   *
   * Every element is encoded with the implicit `JsonEncoder` on its own line, and the lines are written as UTF-8. The file is
   * created when it does not exist and replaced when it does.
   *
   * {{{
   * import zio.json._
   * import zio.stream._
   *
   * val write = writeJsonLinesAs("numbers.jsonl", ZStream(1, 2, 3))
   * }}}
   */
  def writeJsonLinesAs[R, A: JsonEncoder](path: Path, stream: ZStream[R, Throwable, A]): RIO[R, Unit] =
    stream
      .via(
        JsonEncoder[A].encodeJsonLinesPipeline >>>
          charsToUtf8
      )
      .run(ZSink.fromPath(path))
      .unit

  /**
   * Writes a stream of `A` values to the file at `path` as JSON Lines, encoding each element with the implicit `JsonEncoder`.
   *
   * See the `Path` overload for the format.
   */
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
