package enumeratum.zio

import enumeratum.{ Enum, EnumEntry }
import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

object ZIOJson {

  /**
   * Returns an Encoder for the given enum
   */
  def encoder[A <: EnumEntry]: JsonEncoder[A] = JsonEncoder.string.contramap(_.entryName)

  def encoderLowercase[A <: EnumEntry]: JsonEncoder[A] =
    JsonEncoder.string.contramap(_.entryName.toLowerCase)

  def encoderUppercase[A <: EnumEntry]: JsonEncoder[A] =
    JsonEncoder.string.contramap(_.entryName.toUpperCase)

  /**
   * Returns a Decoder for the given enum
   */
  def decoder[A <: EnumEntry](enum: Enum[A]): JsonDecoder[A] =
    JsonDecoder.string.mapOrFail[A] { s =>
      enum.withNameOption(s) match {
        case Some(value) => Right(value)
        case None        => Left(s"'$s' is not a member of enum $enum")
      }
    }

  def decoderLowercaseOnly[A <: EnumEntry](enum: Enum[A]): JsonDecoder[A] =
    JsonDecoder.string.mapOrFail[A] { s =>
      enum.withNameLowercaseOnlyOption(s) match {
        case Some(value) => Right(value)
        case None        => Left(s"'$s' is not a member of enum $enum")
      }
    }

  def decoderUppercaseOnly[A <: EnumEntry](enum: Enum[A]): JsonDecoder[A] =
    JsonDecoder.string.mapOrFail[A] { s =>
      enum.withNameUppercaseOnlyOption(s) match {
        case Some(value) => Right(value)
        case None        => Left(s"'$s' is not a member of enum $enum")
      }
    }

  def decodeCaseInsensitive[A <: EnumEntry](enum: Enum[A]): JsonDecoder[A] =
    JsonDecoder.string.mapOrFail[A] { s =>
      enum.withNameInsensitiveOption(s) match {
        case Some(value) => Right(value)
        case None        => Left(s"'$s' is not a member of enum $enum")
      }
    }

  /**
   * Returns a KeyEncoder for the given enum
   */
  def keyEncoder[A <: EnumEntry]: JsonFieldEncoder[A] = JsonFieldEncoder.string.contramap(_.entryName)

  /**
   * Returns a KeyDecoder for the given enum
   */
  def keyDecoder[A <: EnumEntry](enum: Enum[A]): JsonFieldDecoder[A] = JsonFieldDecoder.string.mapOrFail { s =>
    enum.withNameOption(s) match {
      case Some(value) => Right(value)
      case None        => Left(s"'$s' is not a member of enum $enum")
    }
  }

}
