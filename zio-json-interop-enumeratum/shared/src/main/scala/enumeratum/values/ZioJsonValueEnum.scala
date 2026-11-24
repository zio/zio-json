package enumeratum.values

import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

trait ZioJsonValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]] {
  this: ValueEnum[ValueType, EntryType] =>

  /**
   * Implicit JsonEncoder for this enum
   */
  implicit def zioJsonEncoder: JsonEncoder[EntryType]

  /**
   * Implicit JsonDecoder for this enum
   */
  implicit def zioJsonDecoder: JsonDecoder[EntryType]

  /**
   * Implicit JsonFieldEncoder for this enum
   */
  implicit def zioJsonKeyEncoder: JsonFieldEncoder[EntryType]

  /**
   * Implicit JsonFieldDecoder for this enum
   */
  implicit def zioJsonKeyDecoder: JsonFieldDecoder[EntryType]
}

/**
 * ZioJsonValueEnum for IntEnumEntry
 *
 * {{{
 * import enumeratum.values._
 * import zio.json._
 *
 * sealed abstract class ShirtSize(val value: Int) extends IntEnumEntry
 * case object ShirtSize extends IntEnum[ShirtSize] with IntZioJsonEnum[ShirtSize] {
 *   case object Small  extends ShirtSize(1)
 *   case object Medium extends ShirtSize(2)
 *   case object Large  extends ShirtSize(3)
 *   val values = findValues
 * }
 *
 * val size: ShirtSize = ShirtSize.Small
 * assert(size.toJson == "1")
 * assert("3".fromJson[ShirtSize] == Right(ShirtSize.Large))
 * assert("10".fromJson[ShirtSize].isLeft)
 * }}}
 */
trait IntZioJsonEnum[EntryType <: IntEnumEntry] extends ZioJsonValueEnum[Int, EntryType] {
  this: ValueEnum[Int, EntryType] =>
  implicit val zioJsonEncoder: JsonEncoder[EntryType]         = ZioJson.encoder(this)
  implicit val zioJsonDecoder: JsonDecoder[EntryType]         = ZioJson.decoder(this)
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * ZioJsonValueEnum for LongEnumEntry
 */
trait LongZioJsonEnum[EntryType <: LongEnumEntry] extends ZioJsonValueEnum[Long, EntryType] {
  this: ValueEnum[Long, EntryType] =>
  implicit val zioJsonEncoder: JsonEncoder[EntryType]         = ZioJson.encoder(this)
  implicit val zioJsonDecoder: JsonDecoder[EntryType]         = ZioJson.decoder(this)
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * ZioJsonValueEnum for ShortEnumEntry
 */
trait ShortZioJsonEnum[EntryType <: ShortEnumEntry] extends ZioJsonValueEnum[Short, EntryType] {
  this: ValueEnum[Short, EntryType] =>
  import ZioJsonFieldInstances._
  implicit val zioJsonEncoder: JsonEncoder[EntryType]         = ZioJson.encoder(this)
  implicit val zioJsonDecoder: JsonDecoder[EntryType]         = ZioJson.decoder(this)
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * ZioJsonValueEnum for StringEnumEntry
 */
trait StringZioJsonEnum[EntryType <: StringEnumEntry] extends ZioJsonValueEnum[String, EntryType] {
  this: ValueEnum[String, EntryType] =>
  implicit val zioJsonEncoder: JsonEncoder[EntryType]         = ZioJson.encoder(this)
  implicit val zioJsonDecoder: JsonDecoder[EntryType]         = ZioJson.decoder(this)
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * ZioJsonValueEnum for CharEnumEntry
 */
trait CharZioJsonEnum[EntryType <: CharEnumEntry] extends ZioJsonValueEnum[Char, EntryType] {
  this: ValueEnum[Char, EntryType] =>
  import ZioJsonFieldInstances._
  implicit val zioJsonEncoder: JsonEncoder[EntryType]         = ZioJson.encoder(this)
  implicit val zioJsonDecoder: JsonDecoder[EntryType]         = ZioJson.decoder(this)
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * ZioJsonValueEnum for ByteEnumEntry
 */
trait ByteZioJsonEnum[EntryType <: ByteEnumEntry] extends ZioJsonValueEnum[Byte, EntryType] {
  this: ValueEnum[Byte, EntryType] =>
  import ZioJsonFieldInstances._
  implicit val zioJsonEncoder: JsonEncoder[EntryType]         = ZioJson.encoder(this)
  implicit val zioJsonDecoder: JsonDecoder[EntryType]         = ZioJson.decoder(this)
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}
