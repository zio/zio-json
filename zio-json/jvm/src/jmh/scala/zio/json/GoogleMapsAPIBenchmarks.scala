package zio.json

import java.util.concurrent.TimeUnit

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import zio.json.TestUtils._
import zio.json.data.googlemaps._
import org.openjdk.jmh.annotations._
import zio.json.GoogleMapsAPIBenchmarks._

import scala.util.Try

// To enable the yourkit agent enable a profiling mode, e.g.:
//
// set neoJmhYourkit in Jmh := Seq("sampling")
// set neoJmhYourkit in Jmh := Seq("allocsampled")
//
// more options at https://www.yourkit.com/docs/java/help/startup_options.jsp
//
// When profiling only run one longer test at a time, e.g.
//
// jmh:run -i 1 -wi 0 -r60 GoogleMaps.*decodeZioSuccess1
//
// and look for the generated snapshot in YourKit (ignore the rest)
//
// Also try the async profiler, e.g.
//
//  jmh:run -i 1 -wi 0 -r60 -prof jmh.extras.Async GoogleMaps.*encodeMagnolia
//  jmh:run -i 1 -wi 0 -r60 -prof jmh.extras.Async:event=alloc GoogleMaps.*encodeMagnolia
//
// which may require kernel permissions:
//
//   echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid
//   echo 0 | sudo tee /proc/sys/kernel/kptr_restrict
//
// and needs these projects installed, with these variables:
//
// export ASYNC_PROFILER_DIR=$HOME/Projects/async-profiler
// export FLAME_GRAPH_DIR=$HOME/Projects/FlameGraph
//
// http://malaw.ski/2017/12/10/automatic-flamegraph-generation-from-jmh-benchmarks-using-sbt-jmh-extras-plain-java-too/
// (note you need to type `make` in the async-profiler directory)
//
// to use allocation profiling, you need debugging symbols in your jvm. e.g. use
// the Zulu Java distribution.

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
//, jvmArgs=Array("-XX:-OmitStackTraceInFastThrow"))
//, jvmArgs=Array("-XX:-StackTraceInThrowable"))
class GoogleMapsAPIBenchmarks {
  var jsonString, jsonStringCompact, jsonStringErr, jsonStringErrParse, jsonStringErrNumber: String  = _
  var jsonStringAttack0, jsonStringAttack1, jsonStringAttack2, jsonStringAttack3: String             = _
  var jsonChars, jsonCharsCompact, jsonCharsErr, jsonCharsErrParse, jsonCharsErrNumber: CharSequence = _
  var jsonCharsAttack0, jsonCharsAttack1, jsonCharsAttack2, jsonCharsAttack3: CharSequence           = _
  var decoded: DistanceMatrix                                                                        = _

  @Setup
  def setup(): Unit = {
    // Distance Matrix API call for top-10 by population cities in US:
    // https://maps.googleapis.com/maps/api/distancematrix/json?origins=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose&destinations=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose
    jsonString = getResourceAsString("google_maps_api_response.json")
    jsonChars = asChars(jsonString)
    jsonStringCompact = getResourceAsString(
      "google_maps_api_compact_response.json"
    )
    jsonCharsCompact = asChars(jsonStringCompact)
    jsonStringErr = getResourceAsString("google_maps_api_error_response.json")
    jsonCharsErr = asChars(jsonStringErr)

    // jmh:run GoogleMaps.*ErrorParse
    jsonStringErrParse = getResourceAsString("google_maps_api_error_parse.json")
    jsonCharsErrParse = asChars(jsonStringErr)
    jsonStringErrNumber = getResourceAsString(
      "google_maps_api_error_number.json"
    )
    jsonCharsErrNumber = asChars(jsonStringErr)

    jsonStringAttack0 = getResourceAsString("google_maps_api_attack0.json")
    jsonCharsAttack0 = asChars(jsonStringAttack0)
    jsonStringAttack1 = getResourceAsString("google_maps_api_attack1.json")
    jsonCharsAttack1 = asChars(jsonStringAttack1)
    jsonStringAttack2 = getResourceAsString("google_maps_api_attack2.json")
    jsonCharsAttack2 = asChars(jsonStringAttack2)
    jsonStringAttack3 = getResourceAsString("google_maps_api_attack3.json")
    jsonCharsAttack3 = asChars(jsonStringAttack3)

    decoded = circe.parser.decode[DistanceMatrix](jsonString).toOption.get

    assert(decodeCirceSuccess1() == decodeZioSuccess1())
    assert(decodeCirceSuccess2() == decodeZioSuccess2())

    assert(decodeCirceSuccess1() == decodeCirceAttack0())
    assert(decodeCirceSuccess1() == decodeZioAttack0())

    assert(decodeCirceSuccess1() == decodeCirceAttack1())
    assert(decodeCirceSuccess1() == decodeZioAttack1())

    assert(decodeCirceSuccess1() == decodeCirceAttack2())
    assert(decodeCirceSuccess1() == decodeZioAttack2())
  }

  @Benchmark
  def decodeJsoniterSuccess1(): Either[String, DistanceMatrix] =
    Try(readFromString(jsonString))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterSuccess2(): Either[String, DistanceMatrix] =
    Try(readFromString(jsonStringCompact))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterError(): Either[String, DistanceMatrix] =
    Try(readFromString(jsonStringErr))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterAttack1(): Either[String, DistanceMatrix] =
    Try(readFromString(jsonStringAttack1))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterAttack2(): Either[String, DistanceMatrix] =
    Try(readFromString(jsonStringAttack2))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def decodeJsoniterAttack3(): Either[String, DistanceMatrix] =
    Try(readFromString(jsonStringAttack3))
      .fold(t => Left(t.toString), Right.apply(_))

  @Benchmark
  def encodeJsoniter(): String = writeToString(decoded)

  @Benchmark
  def decodeCirceSuccess1(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonString)

  @Benchmark
  def decodeCirceSuccess2(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringCompact)

  @Benchmark
  def encodeCirce(): String = {
    import io.circe.syntax._

    decoded.asJson.noSpaces
  }

  // @Benchmark
  // def decodeCirceError(): Either[circe.Error, DistanceMatrix] =
  //   circe.parser.decode[DistanceMatrix](jsonStringErr)

  @Benchmark
  def decodeCirceErrorParse(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringErrParse)

  @Benchmark
  def decodeCirceErrorNumber(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringErrNumber)

  @Benchmark
  def decodeCirceAttack0(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringAttack0)

  @Benchmark
  def decodeCirceAttack1(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringAttack1)

  @Benchmark
  def decodeCirceAttack2(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringAttack2)

  @Benchmark
  def decodeCirceAttack3(): Either[circe.Error, DistanceMatrix] =
    circe.parser.decode[DistanceMatrix](jsonStringAttack3)

  @Benchmark
  def decodeZioSuccess1(): Either[String, DistanceMatrix] =
    jsonChars.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioSuccess2(): Either[String, DistanceMatrix] =
    jsonCharsCompact.fromJson[DistanceMatrix]

  @Benchmark
  def encodeZio(): CharSequence =
    JsonEncoder[DistanceMatrix].encodeJson(decoded, None)

  // @Benchmark
  // def decodeZioError(): Either[String, DistanceMatrix] =
  //   jsonCharsErr.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioErrorParse(): Either[String, DistanceMatrix] =
    jsonCharsErrParse.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioErrorNumber(): Either[String, DistanceMatrix] =
    jsonCharsErrNumber.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioAttack0(): Either[String, DistanceMatrix] =
    jsonCharsAttack0.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioAttack1(): Either[String, DistanceMatrix] =
    jsonCharsAttack1.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioAttack2(): Either[String, DistanceMatrix] =
    jsonCharsAttack2.fromJson[DistanceMatrix]

  @Benchmark
  def decodeZioAttack3(): Either[String, DistanceMatrix] =
    jsonCharsAttack3.fromJson[DistanceMatrix]

}

object GoogleMapsAPIBenchmarks {
  implicit val codec: JsonValueCodec[DistanceMatrix] =
    JsonCodecMaker.make
}
