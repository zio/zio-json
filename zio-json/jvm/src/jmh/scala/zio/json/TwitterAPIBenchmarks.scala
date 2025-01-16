package zio.json

import java.util.concurrent.TimeUnit

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import zio.json.TestUtils._
import zio.json.data.twitter._
import org.openjdk.jmh.annotations._
import zio.json.TwitterAPIBenchmarks._

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

    assert(decodeZioError().isLeft)
  }

  @Benchmark
  def decodeJsoniterSuccess1(): Either[String, List[Tweet]] =
    Try(readFromString(jsonString))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterSuccess2(): Either[String, List[Tweet]] =
    Try(readFromString(jsonStringCompact))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterError(): Either[String, List[Tweet]] =
    Try(readFromString(jsonStringErr))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def encodeJsoniter(): String = writeToString(decoded)

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
  def decodeZioSuccess1(): Either[String, List[Tweet]] =
    jsonChars.fromJson[List[Tweet]]

  @Benchmark
  def decodeZioSuccess2(): Either[String, List[Tweet]] =
    jsonCharsCompact.fromJson[List[Tweet]]

  @Benchmark
  def encodeZio(): CharSequence =
    JsonEncoder[List[Tweet]].encodeJson(decoded, None)

  @Benchmark
  def decodeZioError(): Either[String, List[Tweet]] =
    jsonCharsErr.fromJson[List[Tweet]]

}

object TwitterAPIBenchmarks {
  // these avoid the List implicit from being recreated every time
  implicit val zListTweet: JsonDecoder[List[Tweet]] =
    JsonDecoder.list[Tweet]
  implicit val cListTweet: circe.Decoder[List[Tweet]] =
    circe.Decoder.decodeList[Tweet]
  implicit val codec: JsonValueCodec[List[Tweet]] =
    JsonCodecMaker.make
}
