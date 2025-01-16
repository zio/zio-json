package zio.json

import java.util.concurrent.TimeUnit

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import zio.json.GeoJSONBenchmarks._
import zio.json.TestUtils._
import zio.json.data.geojson.handrolled._
import org.openjdk.jmh.annotations._

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

    assert(decodeZioError().isLeft)
  }

  @Benchmark
  def decodeJsoniterSuccess1(): Either[String, GeoJSON] =
    Try(readFromString(jsonString1))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterSuccess2(): Either[String, GeoJSON] =
    Try(readFromString(jsonString2))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterError(): Either[String, GeoJSON] =
    Try(readFromString(jsonStringErr))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def encodeJsoniter(): String = writeToString(decoded)

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
  def decodeZioSuccess1(): Either[String, GeoJSON] =
    jsonChars1.fromJson[GeoJSON]

  @Benchmark
  def decodeZioSuccess2(): Either[String, GeoJSON] =
    jsonChars2.fromJson[GeoJSON]

  @Benchmark
  def encodeZio(): CharSequence =
    JsonEncoder[GeoJSON].encodeJson(decoded, None)

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
