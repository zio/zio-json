package testzio.json.data.googlemaps

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import io.circe
import zio.json._
import play.api.libs.{ json => Play }

final case class Value(
  text: String,
  @named("value")
  @jsonField("value")
  @circe.generic.extras.JsonKey("value")
  v: Int
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

object Value {
  implicit val zioJsonJsonDecoder: JsonDecoder[Value] = DeriveJsonDecoder.gen[Value]
  implicit val zioJsonEncoder: JsonEncoder[Value]     = DeriveJsonEncoder.gen[Value]

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Value] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Value]
  implicit val circeEncoder: circe.Encoder[Value] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Value]

  // play macros don't support custom field
  // implicit val playJsonDecoder: Play.Reads[Value] = Play.Json.reads[Value]

  implicit val playJsonDecoder: Play.Reads[Value] = {
    import play.api.libs.json._
    import play.api.libs.json.Reads._
    import play.api.libs.functional.syntax._

    ((JsPath \ "text").read[String].and((JsPath \ "value").read[Int]))(
      Value.apply _
    )
  }
  implicit val playEncoder: Play.Writes[Value] = {
    import play.api.libs.json._
    import play.api.libs.json.Writes._
    import play.api.libs.functional.syntax._

    ((JsPath \ "text").write[String].and((JsPath \ "value").write[Int]))(unlift(Value.unapply))
  }

}
object Elements {
  implicit val zioJsonJsonDecoder: JsonDecoder[Elements] = DeriveJsonDecoder.gen[Elements]
  implicit val zioJsonEncoder: JsonEncoder[Elements]     = DeriveJsonEncoder.gen[Elements]

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Elements] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Elements]
  implicit val circeEncoder: circe.Encoder[Elements] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Elements]

  implicit val playJsonDecoder: Play.Reads[Elements] = Play.Json.reads[Elements]
  implicit val playEncoder: Play.Writes[Elements]    = Play.Json.writes[Elements]

}
object Rows {
  implicit val zioJsonJsonDecoder: JsonDecoder[Rows] = DeriveJsonDecoder.gen[Rows]
  implicit val zioJsonEncoder: JsonEncoder[Rows]     = DeriveJsonEncoder.gen[Rows]

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Rows] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Rows]
  implicit val circeEncoder: circe.Encoder[Rows] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Rows]

  implicit val playJsonDecoder: Play.Reads[Rows] = Play.Json.reads[Rows]
  implicit val playEncoder: Play.Writes[Rows]    = Play.Json.writes[Rows]

}
object DistanceMatrix {
  implicit val zioJsonJsonDecoder: JsonDecoder[DistanceMatrix] =
    DeriveJsonDecoder.gen[DistanceMatrix]
  implicit val zioJsonEncoder: JsonEncoder[DistanceMatrix] =
    DeriveJsonEncoder.gen[DistanceMatrix]

  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[DistanceMatrix] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[DistanceMatrix]
  implicit val circeEncoder: circe.Encoder[DistanceMatrix] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[DistanceMatrix]

  implicit val playJsonDecoder: Play.Reads[DistanceMatrix] =
    Play.Json.reads[DistanceMatrix]
  implicit val playEncoder: Play.Writes[DistanceMatrix] =
    Play.Json.writes[DistanceMatrix]

}
