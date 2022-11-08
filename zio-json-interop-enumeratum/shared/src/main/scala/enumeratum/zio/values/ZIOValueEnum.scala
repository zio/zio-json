package enumeratum.zio.values

import enumeratum.values._
import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

sealed trait ZIOValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]] {
  this: ValueEnum[ValueType, EntryType] =>

  /**
   * Implicit JsonEncoder for this enum
   */
  implicit def jsonEncoder: JsonEncoder[EntryType]

  /**
   * Implicit JsonDecoder for this enum
   */
  implicit def jsonDecoder: JsonDecoder[EntryType]
}

/**
 * ZIOEnum for IntEnumEntry
 *
 * {{{
 * scala> import enumeratum.values._
 * scala> import cats.syntax.either._
 * scala> import io.ZIO._
 * scala> import io.ZIO.syntax._
 *
 * scala> sealed abstract class ShirtSize(val value:Int) extends IntEnumEntry
 * scala> case object ShirtSize extends IntEnum[ShirtSize] with IntZIOEnum[ShirtSize] {
 *      |  case object Small  extends ShirtSize(1)
 *      |  case object Medium extends ShirtSize(2)
 *      |  case object Large  extends ShirtSize(3)
 *      |  val values = findValues
 *      | }
 *
 * scala> val size: ShirtSize = ShirtSize.Small
 *
 * scala> size.asJson
 * res0: Json = 1
 *
 * scala> Json.fromInt(3).as[ShirtSize]
 * res1: JsonDecoder.Result[ShirtSize] = Right(Large)
 *
 * scala> Json.fromInt(10).as[ShirtSize]
 * res2: JsonDecoder.Result[ShirtSize] = Left(DecodingFailure(10 is not a member of enum ShirtSize, List()))
 * }}}
 */
trait IntZIOEnum[EntryType <: IntEnumEntry] extends ZIOValueEnum[Int, EntryType] {
  this: ValueEnum[Int, EntryType] =>
  implicit val jsonEncoder: JsonEncoder[EntryType] = ZJson.encoder(this)
  implicit val jsonDecoder: JsonDecoder[EntryType] = ZJson.decoder(this)
}

/**
 * ZIOEnum for LongEnumEntry
 */
trait LongZIOEnum[EntryType <: LongEnumEntry] extends ZIOValueEnum[Long, EntryType] {
  this: ValueEnum[Long, EntryType] =>
  implicit val jsonEncoder: JsonEncoder[EntryType] = ZJson.encoder(this)
  implicit val jsonDecoder: JsonDecoder[EntryType] = ZJson.decoder(this)
}

/**
 * ZIOEnum for ShortEnumEntry
 */
trait ShortZIOEnum[EntryType <: ShortEnumEntry] extends ZIOValueEnum[Short, EntryType] {
  this: ValueEnum[Short, EntryType] =>
  implicit val jsonEncoder: JsonEncoder[EntryType] = ZJson.encoder(this)
  implicit val jsonDecoder: JsonDecoder[EntryType] = ZJson.decoder(this)
}

/**
 * ZIOEnum for StringEnumEntry
 */
trait StringZIOEnum[EntryType <: StringEnumEntry] extends ZIOValueEnum[String, EntryType] {
  this: ValueEnum[String, EntryType] =>
  implicit val jsonEncoder: JsonEncoder[EntryType] = ZJson.encoder(this)
  implicit val jsonDecoder: JsonDecoder[EntryType] = ZJson.decoder(this)

  implicit val keyJsonEncoder: JsonFieldEncoder[EntryType] = ZJson.keyJsonEncoder(this)
  implicit val keyJsonDecoder: JsonFieldDecoder[EntryType] = ZJson.keyJsonDecoder(this)
}

/**
 * ZIOEnum for CharEnumEntry
 */
trait CharZIOEnum[EntryType <: CharEnumEntry] extends ZIOValueEnum[Char, EntryType] {
  this: ValueEnum[Char, EntryType] =>
  implicit val jsonEncoder: JsonEncoder[EntryType] = ZJson.encoder(this)
  implicit val jsonDecoder: JsonDecoder[EntryType] = ZJson.decoder(this)
}

/**
 * ZIOEnum for ByteEnumEntry
 */
trait ByteZIOEnum[EntryType <: ByteEnumEntry] extends ZIOValueEnum[Byte, EntryType] {
  this: ValueEnum[Byte, EntryType] =>
  implicit val jsonEncoder: JsonEncoder[EntryType] = ZJson.encoder(this)
  implicit val jsonDecoder: JsonDecoder[EntryType] = ZJson.decoder(this)
}
