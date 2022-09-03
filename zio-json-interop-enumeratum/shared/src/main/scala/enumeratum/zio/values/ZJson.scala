package enumeratum.zio.values

import enumeratum.values.{ ValueEnum, ValueEnumEntry }
import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

object ZJson {

  /**
   * Returns an JsonEncoder for the provided ValueEnum
   */
  def encoder[ValueType: JsonEncoder, EntryType <: ValueEnumEntry[ValueType]](
    enum: ValueEnum[ValueType, EntryType]
  ): JsonEncoder[EntryType] = {
    enum.values // to remove warning and prevent issues scala typing resolution
    implicitly[JsonEncoder[ValueType]].contramap[EntryType](_.value)
  }

  /**
   * Returns a JsonDecoder for the provided ValueEnum
   */
  def decoder[ValueType: JsonDecoder, EntryType <: ValueEnumEntry[ValueType]](
    enum: ValueEnum[ValueType, EntryType]
  ): JsonDecoder[EntryType] =
    implicitly[JsonDecoder[ValueType]].mapOrFail { v =>
      val maybeBound: Option[EntryType] = enum.withValueOpt(v)
      maybeBound match {
        case Some(member) => Right(member)
        case _ =>
          Left(s"$v is not a member of enum $enum")
      }

    }

  def keyJsonEncoder[EntryType <: ValueEnumEntry[String]](
    enum: ValueEnum[String, EntryType]
  ): JsonFieldEncoder[EntryType] = {
    enum.values // to remove warning and prevent issues scala typing resolution
    JsonFieldEncoder.string.contramap(_.value)
  }

  def keyJsonDecoder[EntryType <: ValueEnumEntry[String]](
    enum: ValueEnum[String, EntryType]
  ): JsonFieldDecoder[EntryType] =
    JsonFieldDecoder.string.mapOrFail { v =>
      enum.withValueOpt(v) match {
        case Some(member) => Right(member)
        case _ =>
          Left(s"$v is not a member of enum $enum")
      }
    }

}
