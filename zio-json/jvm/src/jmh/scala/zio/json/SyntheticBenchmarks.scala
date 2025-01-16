package zio.json

import java.util.concurrent.TimeUnit

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import io.circe
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import zio.json.SyntheticBenchmarks._
import zio.json.TestUtils._
import org.openjdk.jmh.annotations._

import scala.util.Try

final case class Nested(n: Option[Nested])
object Nested {
  implicit lazy val zioJsonJsonDecoder: JsonDecoder[Nested] =
    DeriveJsonDecoder.gen
  implicit lazy val zioJsonEncoder: JsonEncoder[Nested] =
    DeriveJsonEncoder.gen
  implicit lazy val circeCodec: Codec[Nested] =
    deriveCodec
}

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class SyntheticBenchmarks {
  // @Param(Array("100", "1000"))
  var size: Int               = 100
  var jsonString: String      = _
  var jsonChars: CharSequence = _
  var decoded: Nested         = _

  @Setup
  def setup(): Unit = {
    val obj = 1.to(size).foldLeft(Nested(None))((n, _) => Nested(Some(n)))

    jsonString = {
      import circe.syntax._

      obj.asJson.noSpaces
    }
    jsonChars = asChars(jsonString)

    decoded = circe.parser.decode[Nested](jsonString).toOption.get

    assert(decodeJsoniterSuccess() == decodeZioSuccess())

    assert(decodeCirceSuccess() == decodeZioSuccess())
  }

  @Benchmark
  def decodeJsoniterSuccess(): Either[String, Nested] =
    Try(readFromString(jsonString))
      .fold(t => Left(t.toString), Right(_))

  @Benchmark
  def encodeJsoniter(): String = writeToString(decoded)

  @Benchmark
  def decodeCirceSuccess(): Either[circe.Error, Nested] =
    circe.parser.decode[Nested](jsonString)

  @Benchmark
  def encodeCirce(): String = {
    import io.circe.syntax._

    decoded.asJson.noSpaces
  }

  @Benchmark
  def decodeZioSuccess(): Either[String, Nested] =
    jsonChars.fromJson[Nested]

  @Benchmark
  def encodeZio(): CharSequence =
    JsonEncoder[Nested].encodeJson(decoded, None)
}

object SyntheticBenchmarks {
  implicit val codec: JsonValueCodec[Nested] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
}
