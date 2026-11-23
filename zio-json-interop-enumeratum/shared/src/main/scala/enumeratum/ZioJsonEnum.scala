package enumeratum

import zio.json.{ JsonDecoder, JsonEncoder }

/**
 * Mix into an enumeratum `Enum` to give its entry type an implicit `JsonEncoder` and `JsonDecoder`. Each entry is written as a JSON string holding its `entryName`, and reading accepts only that exact string.
 *
 * The codecs are for the entry type (`ShirtSize`), not for the type of a single entry: `ShirtSize.Small.toJson` doesn't compile, so give the value the entry type first. The enum's name in an error message is its `toString`, which is the plain name for a `case object` and the default `Object.toString` (class name and hash code) for a plain `object`.
 *
 * {{{
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
 *
 * val json = size.toJson
 * // "Small"
 * val large = "\"Large\"".fromJson[ShirtSize]
 * // Right(Large)
 * val unknown = "\"XLarge\"".fromJson[ShirtSize]
 * // Left(('XLarge' is not a member of enum ShirtSize))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/interop/enumeratum/ Enumeratum Interop]]
 */
trait ZioJsonEnum[A <: EnumEntry] { this: Enum[A] =>

  /** Writes an entry as a JSON string holding its `entryName`. */
  implicit val zioJsonEncoder: JsonEncoder[A] = ZioJson.encoder(this)

  /**
   * Reads a JSON string as the entry whose `entryName` equals it, with the same letter case. Any other string fails with a message such as `'XLarge' is not a member of enum ShirtSize`, and a JSON value that isn't a string fails with `expected string`.
   */
  implicit val zioJsonDecoder: JsonDecoder[A] = ZioJson.decoder(this)
}
