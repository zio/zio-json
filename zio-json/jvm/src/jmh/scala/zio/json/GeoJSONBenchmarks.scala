package zio.json

import java.nio.charset.StandardCharsets._
import java.util.concurrent.TimeUnit

import zio.json
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import zio.json.GeoJSONBenchmarks._
import testzio.json.TestUtils._
import testzio.json.data.geojson.handrolled._
import org.openjdk.jmh.annotations._
import play.api.libs.{ json => Play }

import scala.util.Try

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class GeoJSONBenchmarks {
  var jsonString1, jsonString2, jsonStringErr: String    = _
  var jsonChars1, jsonChars2, jsonCharsErr: CharSequence = _
  var decoded: GeoJSON                                   = _

  @Setup
  def setup(): Unit = {
    jsonString1 = getResourceAsString("che.geo.json")
    jsonChars1 = asChars(jsonString1)
    jsonString2 = getResourceAsString("che-2.geo.json")
    jsonChars2 = asChars(jsonString2)
    jsonStringErr = getResourceAsString("che-err.geo.json")
    jsonCharsErr = asChars(jsonStringErr)

    decoded = circe.parser.decode[GeoJSON](jsonString1).toOption.get

    assert(decodeJsoniterSuccess1() == decodeZioSuccess1())
    assert(decodeJsoniterSuccess2() == decodeZioSuccess2())
    assert(decodeJsoniterError().isLeft)

    assert(decodeCirceSuccess1() == decodeZioSuccess1())
    assert(decodeCirceSuccess2() == decodeZioSuccess2())
    assert(decodeCirceError().isLeft)

    // these are failing because of a bug in play-json, but they succeed
    // assert(decodeCirceSuccess1() == decodePlaySuccess1(), decodePlaySuccess1().toString)
    // assert(decodeCirceSuccess2() == decodePlaySuccess2())
    assert(decodePlaySuccess1().isRight)
    assert(decodePlaySuccess2().isRight)
    assert(decodePlayError().isLeft)

    assert(decodeZioError().isLeft)
  }

  @Benchmark
  def decodeJsoniterSuccess1(): Either[String, GeoJSON] =
    Try(readFromArray(jsonString1.getBytes(UTF_8)))
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeJsoniterSuccess2(): Either[String, GeoJSON] =
    Try(readFromArray(jsonString2.getBytes(UTF_8)))
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeJsoniterError(): Either[String, GeoJSON] =
    Try(readFromArray(jsonStringErr.getBytes(UTF_8)))
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeCirceSuccess1(): Either[circe.Error, GeoJSON] =
    circe.parser.decode[GeoJSON](jsonString1)

  @Benchmark
  def decodeCirceSuccess2(): Either[circe.Error, GeoJSON] =
    circe.parser.decode[GeoJSON](jsonString2)

  @Benchmark
  def encodeCirce(): String = {
    import io.circe.syntax._

    decoded.asJson.noSpaces
  }

  @Benchmark
  def decodeCirceError(): Either[circe.Error, GeoJSON] =
    circe.parser.decode[GeoJSON](jsonStringErr)

  @Benchmark
  def decodePlaySuccess1(): Either[String, GeoJSON] =
    Try(Play.Json.parse(jsonString1).as[GeoJSON])
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodePlaySuccess2(): Either[String, GeoJSON] =
    Try(Play.Json.parse(jsonString2).as[GeoJSON])
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def encodePlay(): String =
    Play.Json.stringify(implicitly[Play.Writes[GeoJSON]].writes(decoded))

  @Benchmark
  def decodePlayError(): Either[String, GeoJSON] =
    Try(Play.Json.parse(jsonStringErr).as[GeoJSON])
      .fold(t => Left(t.toString), Right.apply)

  @Benchmark
  def decodeZioSuccess1(): Either[String, GeoJSON] =
    jsonChars1.fromJson[GeoJSON]

  @Benchmark
  def decodeZioSuccess2(): Either[String, GeoJSON] =
    jsonChars2.fromJson[GeoJSON]

  @Benchmark
  def encodeZio(): String = {
    import zio.json._

    decoded.toJson
  }

  @Benchmark
  def decodeZioError(): Either[String, GeoJSON] =
    jsonCharsErr.fromJson[GeoJSON]

}

object GeoJSONBenchmarks {
  implicit val codec: JsonValueCodec[GeoJSON] =
    JsonCodecMaker.make(
      CodecMakerConfig
        .withAllowRecursiveTypes(true)
        .withRequireDiscriminatorFirst(false)
    )
}
