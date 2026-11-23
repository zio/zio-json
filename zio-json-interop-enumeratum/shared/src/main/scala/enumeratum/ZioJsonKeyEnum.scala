package enumeratum

import zio.json.{ JsonFieldDecoder, JsonFieldEncoder }

/**
 * Mix into an enumeratum `Enum` to give its entry type an implicit `JsonFieldEncoder` and `JsonFieldDecoder`, so that
 * entries can be the keys of a JSON object, for example of a `Map` keyed by the enum. Each key holds the entry's
 * `entryName`. Mix in `ZioJsonEnum` as well to read and write entries as values.
 *
 * {{{
 * import zio.json._
 *
 * sealed trait Fruit extends EnumEntry
 * case object Fruit extends Enum[Fruit] with ZioJsonKeyEnum[Fruit] {
 *   case object Apple  extends Fruit
 *   case object Banana extends Fruit
 *   val values = findValues
 * }
 *
 * val stock: Map[Fruit, Int] = Map(Fruit.Apple -> 3, Fruit.Banana -> 5)
 *
 * val json = stock.toJson
 * // {"Apple":3,"Banana":5}
 * val parsed = """{"Banana":2}""".fromJson[Map[Fruit, Int]]
 * // Right(Map(Banana -> 2))
 * val unknown = """{"Cherry":1}""".fromJson[Map[Fruit, Int]]
 * // Left(.Cherry('Cherry' is not a member of enum Fruit))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/interop/enumeratum/ Enumeratum Interop]]
 */
trait ZioJsonKeyEnum[A <: EnumEntry] { this: Enum[A] =>

  /** Writes an entry as an object key holding its `entryName`. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[A] = ZioJson.keyEncoder(this)

  /**
   * Reads an object key as the entry whose `entryName` equals it, with the same letter case. Any other key fails with a
   * message such as `'Cherry' is not a member of enum Fruit`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[A] = ZioJson.keyDecoder(this)
}
