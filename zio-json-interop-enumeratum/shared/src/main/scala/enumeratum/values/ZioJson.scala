package enumeratum.values

import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

/**
 * Builds zio-json codecs for an enumeratum `ValueEnum` whose entries are written as their `value`, with the codec of
 * the value type. Use these when the enum can't mix in one of the traits such as `IntZioJsonEnum`.
 *
 * On Scala 3, enumeratum's `findValues` in a value enum doesn't compile without the `-Yretain-trees` compiler option,
 * so the example lists `values` by hand.
 *
 * {{{
 * import zio.json._
 *
 * sealed abstract class HttpStatus(val value: Int) extends IntEnumEntry
 * case object HttpStatus extends IntEnum[HttpStatus] {
 *   case object Ok       extends HttpStatus(200)
 *   case object NotFound extends HttpStatus(404)
 *   val values: IndexedSeq[HttpStatus] = IndexedSeq(Ok, NotFound)
 * }
 *
 * implicit val statusEncoder: JsonEncoder[HttpStatus] = ZioJson.encoder(HttpStatus)
 * implicit val statusDecoder: JsonDecoder[HttpStatus] = ZioJson.decoder(HttpStatus)
 *
 * val json = (HttpStatus.NotFound: HttpStatus).toJson
 * // 404
 * val parsed = "200".fromJson[HttpStatus]
 * // Right(Ok)
 * val unknown = "500".fromJson[HttpStatus]
 * // Left(('500' is not a member of enum HttpStatus))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/interop/enumeratum/ Enumeratum Interop]]
 */
object ZioJson {

  /**
   * Returns an encoder that writes an entry as its `value`, with the `JsonEncoder` of the value type. The enum argument
   * only fixes the types.
   */
  def encoder[ValueType: JsonEncoder, EntryType <: ValueEnumEntry[ValueType]](
    @annotation.unused e: ValueEnum[ValueType, EntryType]
  ): JsonEncoder[EntryType] =
    JsonEncoder[ValueType].contramap(_.value)

  /**
   * Returns a decoder that reads a value with the `JsonDecoder` of the value type and returns the entry that has that
   * value. Input that the value decoder rejects fails with that decoder's error (`expected an Int` for `"x"` and an
   * `Int` enum).
   *
   * A value that no entry has fails with a message such as `'500' is not a member of enum HttpStatus`: the value, then
   * the enum's `toString`, which is the plain name for a `case object` and the default `Object.toString` (class name
   * and hash code) for a plain `object`.
   */
  def decoder[ValueType: JsonDecoder, EntryType <: ValueEnumEntry[ValueType]](
    e: ValueEnum[ValueType, EntryType]
  ): JsonDecoder[EntryType] =
    JsonDecoder[ValueType].mapOrFail(v => enumeratum.ZioJson.fromOption(e.withValueOpt(v), v.toString, e.toString))

  /**
   * Returns a field encoder that writes an entry as an object key holding its `value`, with the implicit
   * `JsonFieldEncoder` of the value type.
   *
   * zio-json has field codecs for `Int`, `Long` and `String` but not for `Short`, `Byte` or `Char`. For those value
   * types, provide one, or mix in `ShortZioJsonEnum`, `ByteZioJsonEnum` or `CharZioJsonEnum`, which bring their own.
   *
   * {{{
   * import zio.json._
   *
   * sealed abstract class Floor(val value: Long) extends LongEnumEntry
   * case object Floor extends LongEnum[Floor] {
   *   case object Ground extends Floor(0L)
   *   case object Roof   extends Floor(12L)
   *   val values: IndexedSeq[Floor] = IndexedSeq(Ground, Roof)
   * }
   *
   * implicit val floorKeyEncoder: JsonFieldEncoder[Floor] = ZioJson.keyEncoder(Floor)
   * implicit val floorKeyDecoder: JsonFieldDecoder[Floor] = ZioJson.keyDecoder(Floor)
   *
   * val json = Map[Floor, String](Floor.Roof -> "garden").toJson
   * // {"12":"garden"}
   * val parsed = """{"0":"lobby"}""".fromJson[Map[Floor, String]]
   * // Right(Map(Ground -> lobby))
   * val unknown = """{"3":"office"}""".fromJson[Map[Floor, String]]
   * // Left(.3('3' is not a member of enum Floor))
   * }}}
   */
  def keyEncoder[ValueType, EntryType <: ValueEnumEntry[ValueType]](
    @annotation.unused e: ValueEnum[ValueType, EntryType]
  )(implicit fe: JsonFieldEncoder[ValueType]): JsonFieldEncoder[EntryType] =
    fe.contramap(_.value)

  /**
   * Returns a field decoder that reads an object key with the implicit `JsonFieldDecoder` of the value type and returns
   * the entry that has that value. A key that no entry has fails with a message such as
   * `'3' is not a member of enum Floor`.
   */
  def keyDecoder[ValueType, EntryType <: ValueEnumEntry[ValueType]](
    e: ValueEnum[ValueType, EntryType]
  )(implicit fd: JsonFieldDecoder[ValueType]): JsonFieldDecoder[EntryType] =
    fd.mapOrFail(v => enumeratum.ZioJson.fromOption(e.withValueOpt(v), v.toString, e.toString))

}
