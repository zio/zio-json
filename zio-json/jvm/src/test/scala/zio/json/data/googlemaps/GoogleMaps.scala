package zio.json.data.googlemaps

import com.github.ghik.silencer.silent
import com.github.plokhotnyuk.jsoniter_scala.macros.named
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import zio.json._

final case class Value(
  text: String,
  @named("value")
  @jsonField("value")
  value: Int
)
final case class Elements(distance: Value, duration: Value, status: String)
final case class Rows(elements: List[Elements])
// @jsonNoExtraFields // entirely mitigates Attack1
final case class DistanceMatrix(
  destination_addresses: List[String],
  origin_addresses: List[String],
  rows: List[Rows],
  status: String
)

@silent("Block result was adapted via implicit conversion")
object Value {
  implicit val zioJsonJsonDecoder: JsonDecoder[Value] = DeriveJsonDecoder.gen[Value]
  implicit val zioJsonEncoder: JsonEncoder[Value]     = DeriveJsonEncoder.gen[Value]

  implicit val circeCodec: Codec[Value] = deriveCodec
}
@silent("Block result was adapted via implicit conversion")
object Elements {
  implicit val zioJsonJsonDecoder: JsonDecoder[Elements] = DeriveJsonDecoder.gen[Elements]
  implicit val zioJsonEncoder: JsonEncoder[Elements]     = DeriveJsonEncoder.gen[Elements]

  implicit val circeCodec: Codec[Elements] = deriveCodec
}
@silent("Block result was adapted via implicit conversion")
object Rows {
  implicit val zioJsonJsonDecoder: JsonDecoder[Rows] = DeriveJsonDecoder.gen[Rows]
  implicit val zioJsonEncoder: JsonEncoder[Rows]     = DeriveJsonEncoder.gen[Rows]

  implicit val circeCodec: Codec[Rows] = deriveCodec
}
@silent("Block result was adapted via implicit conversion")
object DistanceMatrix {
  implicit val zioJsonJsonDecoder: JsonDecoder[DistanceMatrix] =
    DeriveJsonDecoder.gen[DistanceMatrix]
  implicit val zioJsonEncoder: JsonEncoder[DistanceMatrix] =
    DeriveJsonEncoder.gen[DistanceMatrix]

  implicit val circeCodec: Codec[DistanceMatrix] = deriveCodec
}
