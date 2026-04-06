package enumeratum.values

import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

object ZioJson {

  def encoder[ValueType: JsonEncoder, EntryType <: ValueEnumEntry[ValueType]](
    @annotation.unused e: ValueEnum[ValueType, EntryType]
  ): JsonEncoder[EntryType] =
    JsonEncoder[ValueType].contramap(_.value)

  def decoder[ValueType: JsonDecoder, EntryType <: ValueEnumEntry[ValueType]](
    e: ValueEnum[ValueType, EntryType]
  ): JsonDecoder[EntryType] =
    JsonDecoder[ValueType].mapOrFail(v => enumeratum.ZioJson.fromOption(e.withValueOpt(v), v.toString, e.toString))

  def keyEncoder[ValueType, EntryType <: ValueEnumEntry[ValueType]](
    @annotation.unused e: ValueEnum[ValueType, EntryType]
  )(implicit fe: JsonFieldEncoder[ValueType]): JsonFieldEncoder[EntryType] =
    fe.contramap(_.value)

  def keyDecoder[ValueType, EntryType <: ValueEnumEntry[ValueType]](
    e: ValueEnum[ValueType, EntryType]
  )(implicit fd: JsonFieldDecoder[ValueType]): JsonFieldDecoder[EntryType] =
    fd.mapOrFail(v => enumeratum.ZioJson.fromOption(e.withValueOpt(v), v.toString, e.toString))

}
