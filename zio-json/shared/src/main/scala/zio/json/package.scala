package zio

import java.io.{ File, IOException }
import java.net.URL
import java.nio.file.{ Path, Paths }

import zio.blocking.Blocking
import zio.stream._

package object json {
  implicit final class EncoderOps[A](private val a: A) extends AnyVal {
    def toJson(implicit A: JsonEncoder[A]): String = A.encodeJson(a, None).toString

    // Jon Pretty's better looking brother, but a bit slower
    def toJsonPretty(implicit A: JsonEncoder[A]): String = A.encodeJson(a, Some(0)).toString
  }

  implicit final class DecoderOps(private val json: CharSequence) extends AnyVal {

    /**
     * Attempts to decode the raw JSON string as an `A`.
     *
     * On failure a human readable message is returned using a jq friendly
     * format. For example the error
     * `.rows[0].elements[0].distance.value(missing)"` tells us the location of a
     * missing field named "value". We can use part of the error message in the
     * `jq` command line tool for further inspection, e.g.
     *
     * {{{jq '.rows[0].elements[0].distance' input.json}}}
     */
    def fromJson[A](implicit A: JsonDecoder[A]): Either[String, A] = A.decodeJson(json)
  }

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

  private def stringToChars: ZTransducer[Any, Nothing, String, Char] =
    ZTransducer
      .fromFunction[String, Chunk[Char]](s => Chunk.fromArray(s.toCharArray))
      .mapChunks(_.flatten)
}
