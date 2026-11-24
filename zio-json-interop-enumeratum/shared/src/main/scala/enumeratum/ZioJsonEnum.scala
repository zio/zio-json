package enumeratum

import zio.json.{ JsonDecoder, JsonEncoder }

/**
 * Helper trait that adds implicit zio-json encoders and decoders for an [[Enum]]'s members
 *
 * Example:
 *
 * {{{
 * import enumeratum._
 * import zio.json._
 *
 * sealed trait ShirtSize extends EnumEntry
 * case object ShirtSize extends Enum[ShirtSize] with ZioJsonEnum[ShirtSize] {
 *   case object Small  extends ShirtSize
 *   case object Medium extends ShirtSize
 *   case object Large  extends ShirtSize
 *   val values = findValues
 * }
 *
 * val size: ShirtSize = ShirtSize.Small
 * assert(size.toJson == "\"Small\"")
 * assert("\"Large\"".fromJson[ShirtSize] == Right(ShirtSize.Large))
 * assert("\"XLarge\"".fromJson[ShirtSize].isLeft)
 * }}}
 */
trait ZioJsonEnum[A <: EnumEntry] { this: Enum[A] =>

  /**
   * Implicit JsonEncoder for this enum
   */
  implicit val zioJsonEncoder: JsonEncoder[A] = ZioJson.encoder(this)

  /**
   * Implicit JsonDecoder for this enum
   */
  implicit val zioJsonDecoder: JsonDecoder[A] = ZioJson.decoder(this)
}
