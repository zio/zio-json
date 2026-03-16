package enumeratum

import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

object ZioJson {

  def encoder[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonEncoder[A] =
    stringEncoder.contramap(_.entryName)

  def encoderLowercase[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonEncoder[A] =
    stringEncoder.contramap(_.entryName.toLowerCase)

  def encoderUppercase[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonEncoder[A] =
    stringEncoder.contramap(_.entryName.toUpperCase)

  def decoder[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameOption(s), s, e.toString))

  def decoderLowercaseOnly[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameLowercaseOnlyOption(s), s, e.toString))

  def decoderUppercaseOnly[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameUppercaseOnlyOption(s), s, e.toString))

  def decoderCaseInsensitive[A <: EnumEntry](e: Enum[A]): JsonDecoder[A] =
    stringDecoder.mapOrFail(s => fromOption(e.withNameInsensitiveOption(s), s, e.toString))

  def keyEncoder[A <: EnumEntry](@annotation.unused e: Enum[A]): JsonFieldEncoder[A] =
    stringFieldEncoder.contramap(_.entryName)

  def keyDecoder[A <: EnumEntry](e: Enum[A]): JsonFieldDecoder[A] =
    stringFieldDecoder.mapOrFail(s => fromOption(e.withNameOption(s), s, e.toString))

  private[enumeratum] def fromOption[A](opt: Option[A], input: String, enumName: String): Either[String, A] =
    opt.toRight(s"'$input' is not a member of enum $enumName")

  private val stringEncoder      = JsonEncoder[String]
  private val stringDecoder      = JsonDecoder[String]
  private val stringFieldEncoder = JsonFieldEncoder[String]
  private val stringFieldDecoder = JsonFieldDecoder[String]

}
