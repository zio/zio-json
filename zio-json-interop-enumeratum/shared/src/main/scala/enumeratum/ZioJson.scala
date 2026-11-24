package enumeratum

import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

/**
 * Builds zio-json codecs for an enumeratum `Enum` whose entries are written as their `entryName`. Use these when the
 * enum can't mix in `ZioJsonEnum` or `ZioJsonKeyEnum`, or to choose the letter case of the JSON.
 *
 * {{{
 * import zio.json._
 *
 * sealed trait Weekday extends EnumEntry
 * case object Weekday extends Enum[Weekday] {
 *   case object Monday  extends Weekday
 *   case object Tuesday extends Weekday
 *   val values = findValues
 * }
 *
 * implicit val weekdayEncoder: JsonEncoder[Weekday] = ZioJson.encoder(Weekday)
 * implicit val weekdayDecoder: JsonDecoder[Weekday] = ZioJson.decoder(Weekday)
 *
 * val json = (Weekday.Monday: Weekday).toJson
 * // "Monday"
 * val parsed = "\"Tuesday\"".fromJson[Weekday]
 * // Right(Tuesday)
 * val wrongCase = "\"tuesday\"".fromJson[Weekday]
 * // Left(('tuesday' is not a member of enum Weekday))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/interop/enumeratum/ Enumeratum Interop]]
 */
object ZioJson {

  /**
   * Returns an encoder that writes an entry as a JSON string holding its `entryName`. The enum argument only fixes the
   * entry type.
   */
  def encoder[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonEncoder[A] =
    stringEncoder.contramap(_.entryName)

  /**
   * Returns an encoder that writes an entry as a JSON string holding its `entryName` in lower case (`"monday"` for
   * `Monday`).
   */
  def encoderLowercase[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonEncoder[A] =
    stringEncoder.contramap(_.entryName.toLowerCase)

  /**
   * Returns an encoder that writes an entry as a JSON string holding its `entryName` in upper case (`"MONDAY"` for
   * `Monday`).
   */
  def encoderUppercase[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonEncoder[A] =
    stringEncoder.contramap(_.entryName.toUpperCase)

  /**
   * Returns a decoder that reads a JSON string as the entry whose `entryName` equals it, with the same letter case. A
   * JSON value that isn't a string fails with `expected string`.
   *
   * Any other string fails with a message such as `'tuesday' is not a member of enum Weekday`: the input, then the
   * enum's `toString`, which is the plain name for a `case object` and the default `Object.toString` (class name and
   * hash code) for a plain `object`.
   */
  def decoder[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameOption(s), s, e.toString))

  /**
   * Returns a decoder that accepts only the lower-case form of an `entryName`: `"monday"` reads as `Monday`, and
   * `"Monday"` fails with a message such as `'Monday' is not a member of enum Weekday`.
   */
  def decoderLowercaseOnly[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameLowercaseOnlyOption(s), s, e.toString))

  /**
   * Returns a decoder that accepts only the upper-case form of an `entryName`: `"MONDAY"` reads as `Monday`, and
   * `"Monday"` fails with a message such as `'Monday' is not a member of enum Weekday`.
   */
  def decoderUppercaseOnly[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameUppercaseOnlyOption(s), s, e.toString))

  /**
   * Returns a decoder that accepts an `entryName` in any letter case: `"Monday"` and `"mOnDaY"` both read as `Monday`.
   * Any other string fails with a message such as `'Mon' is not a member of enum Weekday`.
   */
  def decoderCaseInsensitive[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameInsensitiveOption(s), s, e.toString))

  /**
   * Returns a field encoder that writes an entry as an object key holding its `entryName`, for a `Map` keyed by the
   * enum.
   *
   * {{{
   * import zio.json._
   *
   * sealed trait Currency extends EnumEntry
   * case object Currency extends Enum[Currency] {
   *   case object EUR extends Currency
   *   case object USD extends Currency
   *   val values = findValues
   * }
   *
   * implicit val currencyKeyEncoder: JsonFieldEncoder[Currency] = ZioJson.keyEncoder(Currency)
   * implicit val currencyKeyDecoder: JsonFieldDecoder[Currency] = ZioJson.keyDecoder(Currency)
   *
   * val json = Map[Currency, Double](Currency.EUR -> 1.5).toJson
   * // {"EUR":1.5}
   * val parsed = """{"USD":2.25}""".fromJson[Map[Currency, Double]]
   * // Right(Map(USD -> 2.25))
   * }}}
   */
  def keyEncoder[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonFieldEncoder[A] =
    stringFieldEncoder.contramap(_.entryName)

  /**
   * Returns a field decoder that reads an object key as the entry whose `entryName` equals it, with the same letter
   * case. Any other key fails with a message such as `'usd' is not a member of enum Currency`.
   */
  def keyDecoder[A <: EnumEntry](e: Enum[A]): JsonFieldDecoder[A] =
    stringFieldDecoder.mapOrFail(s => fromOption(e.withNameOption(s), s, e.toString))

  private[enumeratum] def fromOption[A](opt: Option[A], input: String, enumName: String): Either[String, A] =
    opt.toRight(s"'$input' is not a member of enum $enumName")

  private val stringEncoder      = JsonEncoder[String]
  private val stringDecoder      = JsonDecoder[String]
  private val stringFieldEncoder = JsonFieldEncoder[String]
  private val stringFieldDecoder = JsonFieldDecoder[String]

}
