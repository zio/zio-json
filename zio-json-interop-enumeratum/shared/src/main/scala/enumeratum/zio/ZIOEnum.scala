package enumeratum.zio

import enumeratum.{ Enum, EnumEntry }
import zio.json.{ JsonDecoder, JsonEncoder }

/**
 * Helper trait that adds implicit ZIO Json encoders and decoders for an [[Enum]]'s members
 *
 * Example:
 *
 * {{{
 * scala> import enumeratum._
 * scala> import enumeratum.zio._
 * scala> import zio.json._
 *
 * scala> sealed trait ShirtSize extends EnumEntry
 * scala> case object ShirtSize extends Enum[ShirtSize] with ZIOEnum[ShirtSize] {
 *      |  case object Small  extends ShirtSize
 *      |  case object Medium extends ShirtSize
 *      |  case object Large  extends ShirtSize
 *      |  val values = findValues
 *      | }
 *
 * scala> val size: ShirtSize = ShirtSize.Small
 *
 * scala> size.asJson
 * res0: Json = "Small"
 *
 * scala> Json.fromString("Large").as[ShirtSize]
 * res1: Decoder.Result[ShirtSize] = Right(Large)
 *
 * scala> Json.fromString("XLarge").as[ShirtSize]
 * res2: Decoder.Result[ShirtSize] = Left(DecodingFailure('XLarge' is not a member of enum ShirtSize, List()))
 * }}}
 */
trait ZIOEnum[A <: EnumEntry] { this: Enum[A] =>
  import ZIOJson._

  /**
   * Implicit Encoder for this enum
   */
  implicit val jsonEncoder: JsonEncoder[A] = encoder[A]

  /**
   * Implicit Decoder for this enum
   */
  implicit val jsonDecoder: JsonDecoder[A] = decoder[A](this)

}
