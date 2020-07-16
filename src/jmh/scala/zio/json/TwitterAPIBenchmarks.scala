package zio.json

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

import zio.json
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import zio.json.TestUtils._
import zio.json.data.twitter._
import org.openjdk.jmh.annotations._
import play.api.libs.{ json => Play }
import TwitterAPIBenchmarks._

import scala.util.Try

// reference for the format of tweets: https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class TwitterAPIBenchmarks {
  var jsonString, jsonStringCompact, jsonStringErr: String    = _
  var jsonChars, jsonCharsCompact, jsonCharsErr: CharSequence = _
  var decoded: List[Tweet]                                    = _

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("twitter_api_response.json")
    jsonChars = asChars(jsonString)
    jsonStringCompact = getResourceAsString("twitter_api_compact_response.json")
    jsonCharsCompact = asChars(jsonStringCompact)
    jsonStringErr = getResourceAsString("twitter_api_error_response.json")
    jsonCharsErr = asChars(jsonStringErr)

    decoded = circe.parser.decode[List[Tweet]](jsonString).toOption.get

    assert(decodeJsoniterSuccess1() == decodeZioSuccess1())
    assert(decodeJsoniterSuccess2() == decodeZioSuccess1())
    assert(decodeJsoniterError().isLeft)

    assert(decodeCirceSuccess1() == decodeZioSuccess1())
    assert(decodeCirceSuccess2() == decodeZioSuccess2())
    assert(decodeCirceError().isLeft)

    assert(decodePlaySuccess1() == decodeZioSuccess1())
    assert(decodePlaySuccess2() == decodeZioSuccess2())
    assert(decodePlayError().isLeft)

    assert(decodeZioError().isLeft)
  }

  @Benchmark
  def decodeJsoniterSuccess1(): Either[String, List[Tweet]] =
    Try(readFromArray(jsonString.getBytes(UTF_8)))
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeJsoniterSuccess2(): Either[String, List[Tweet]] =
    Try(readFromArray(jsonStringCompact.getBytes(UTF_8)))
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeJsoniterError(): Either[String, List[Tweet]] =
    Try(readFromArray(jsonStringErr.getBytes(UTF_8)))
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeCirceSuccess1(): Either[circe.Error, List[Tweet]] =
    circe.parser.decode[List[Tweet]](jsonString)

  @Benchmark
  def decodeCirceSuccess2(): Either[circe.Error, List[Tweet]] =
    circe.parser.decode[List[Tweet]](jsonStringCompact)

  @Benchmark
  def encodeCirce(): String = {
    import io.circe.syntax._

    decoded.asJson.noSpaces
  }

  @Benchmark
  def decodeCirceError(): Either[circe.Error, List[Tweet]] =
    circe.parser.decode[List[Tweet]](jsonStringErr)

  @Benchmark
  def decodePlaySuccess1(): Either[String, List[Tweet]] =
    Try(Play.Json.parse(jsonString).as[List[Tweet]])
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodePlaySuccess2(): Either[String, List[Tweet]] =
    Try(Play.Json.parse(jsonStringCompact).as[List[Tweet]])
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def encodePlay(): String =
    Play.Json.stringify(implicitly[Play.Writes[List[Tweet]]].writes(decoded))

  @Benchmark
  def decodePlayError(): Either[String, List[Tweet]] =
    Try(Play.Json.parse(jsonStringErr).as[List[Tweet]])
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeZioSuccess1(): Either[String, List[Tweet]] =
    json.parser.decode[List[Tweet]](jsonChars)

  @Benchmark
  def decodeZioSuccess2(): Either[String, List[Tweet]] =
    json.parser.decode[List[Tweet]](jsonCharsCompact)

  @Benchmark
  def encodeZio(): String = {
    import zio.json.syntax._

    decoded.toJson
  }

  @Benchmark
  def decodeZioError(): Either[String, List[Tweet]] =
    json.parser.decode[List[Tweet]](jsonCharsErr)

}

object TwitterAPIBenchmarks {
  // these avoid the List implicit from being recreated every time
  implicit val zListTweet: json.Decoder[List[Tweet]] =
    json.Decoder.list[Tweet]
  implicit val cListTweet: circe.Decoder[List[Tweet]] =
    circe.Decoder.decodeList[Tweet]
  implicit val codec: JsonValueCodec[List[Tweet]] =
    JsonCodecMaker.make
}
