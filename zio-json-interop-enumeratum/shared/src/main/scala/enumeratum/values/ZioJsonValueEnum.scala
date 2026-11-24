package enumeratum.values

import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

/**
 * The implicit zio-json codecs of an enumeratum `ValueEnum`: a `JsonEncoder` and a `JsonDecoder` for the entries, and a
 * `JsonFieldEncoder` and a `JsonFieldDecoder` for using entries as object keys. Mix in the trait for the value type,
 * [[IntZioJsonEnum]], [[LongZioJsonEnum]], [[ShortZioJsonEnum]], [[StringZioJsonEnum]], [[CharZioJsonEnum]] or
 * [[ByteZioJsonEnum]], each of which writes an entry as its `value`.
 *
 * On Scala 3, enumeratum's `findValues` in a value enum doesn't compile without the `-Yretain-trees` compiler option,
 * so the example lists `values` by hand.
 *
 * {{{
 * import zio.json._
 *
 * sealed abstract class Country(val value: String) extends StringEnumEntry
 * case object Country extends StringEnum[Country] with StringZioJsonEnum[Country] {
 *   case object France extends Country("fr")
 *   case object Japan  extends Country("jp")
 *   val values: IndexedSeq[Country] = IndexedSeq(France, Japan)
 * }
 *
 * val json = (Country.Japan: Country).toJson
 * // "jp"
 * val parsed = "\"fr\"".fromJson[Country]
 * // Right(France)
 * val capitals = Map[Country, String](Country.France -> "Paris").toJson
 * // {"fr":"Paris"}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/interop/enumeratum/ Enumeratum Interop]]
 */
trait ZioJsonValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]] {
  this: ValueEnum[ValueType, EntryType] =>

  /** The implicit encoder of the entries. */
  implicit def zioJsonEncoder: JsonEncoder[EntryType]

  /** The implicit decoder of the entries. */
  implicit def zioJsonDecoder: JsonDecoder[EntryType]

  /** The implicit encoder of the entries as object keys. */
  implicit def zioJsonKeyEncoder: JsonFieldEncoder[EntryType]

  /** The implicit decoder of the entries as object keys. */
  implicit def zioJsonKeyDecoder: JsonFieldDecoder[EntryType]
}

/**
 * A [[ZioJsonValueEnum]] for an `IntEnum`: an entry is written as its `Int` value, as a JSON number and as an object
 * key.
 *
 * On Scala 3, enumeratum's `findValues` in a value enum doesn't compile without the `-Yretain-trees` compiler option,
 * so the example lists `values` by hand.
 *
 * {{{
 * import zio.json._
 *
 * sealed abstract class ShirtSize(val value: Int) extends IntEnumEntry
 * case object ShirtSize extends IntEnum[ShirtSize] with IntZioJsonEnum[ShirtSize] {
 *   case object Small  extends ShirtSize(1)
 *   case object Medium extends ShirtSize(2)
 *   case object Large  extends ShirtSize(3)
 *   val values: IndexedSeq[ShirtSize] = IndexedSeq(Small, Medium, Large)
 * }
 *
 * val size: ShirtSize = ShirtSize.Small
 *
 * val json = size.toJson
 * // 1
 * val large = "3".fromJson[ShirtSize]
 * // Right(Large)
 * val unknown = "10".fromJson[ShirtSize]
 * // Left(('10' is not a member of enum ShirtSize))
 * }}}
 */
trait IntZioJsonEnum[EntryType <: IntEnumEntry] extends ZioJsonValueEnum[Int, EntryType] {
  this: ValueEnum[Int, EntryType] =>

  /** Writes an entry as its `Int` value, as a JSON number. */
  implicit val zioJsonEncoder: JsonEncoder[EntryType] = ZioJson.encoder(this)

  /**
   * Reads an `Int` and returns the entry that has that value. A number that no entry has fails with a message such as
   * `'10' is not a member of enum ShirtSize`, and input that the `Int` decoder rejects, such as `"x"`, fails with
   * `expected an Int`.
   */
  implicit val zioJsonDecoder: JsonDecoder[EntryType] = ZioJson.decoder(this)

  /** Writes an entry as an object key holding its `Int` value, such as `"3"`. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)

  /**
   * Reads an object key as an `Int` and returns the entry that has that value. A key that isn't an `Int` fails with
   * `Invalid Int: x` for the key `x`, and one that no entry has fails with a message such as
   * `'9' is not a member of enum ShirtSize`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * A [[ZioJsonValueEnum]] for a `LongEnum`: an entry is written as its `Long` value, as a JSON number and as an object
 * key.
 */
trait LongZioJsonEnum[EntryType <: LongEnumEntry] extends ZioJsonValueEnum[Long, EntryType] {
  this: ValueEnum[Long, EntryType] =>

  /** Writes an entry as its `Long` value, as a JSON number. */
  implicit val zioJsonEncoder: JsonEncoder[EntryType] = ZioJson.encoder(this)

  /**
   * Reads a `Long` and returns the entry that has that value. A number that no entry has fails with a message such as
   * `'11' is not a member of enum Priority`, and input that the `Long` decoder rejects, such as `"x"`, fails with
   * `expected a Long`.
   */
  implicit val zioJsonDecoder: JsonDecoder[EntryType] = ZioJson.decoder(this)

  /** Writes an entry as an object key holding its `Long` value, such as `"10"`. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)

  /**
   * Reads an object key as a `Long` and returns the entry that has that value. A key that isn't a `Long` fails with
   * `Invalid Long: x` for the key `x`, and one that no entry has fails with a message such as
   * `'21' is not a member of enum Priority`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * A [[ZioJsonValueEnum]] for a `ShortEnum`: an entry is written as its `Short` value, as a JSON number and as an object
 * key. It brings its own field codecs for `Short`, which zio-json doesn't have.
 */
trait ShortZioJsonEnum[EntryType <: ShortEnumEntry] extends ZioJsonValueEnum[Short, EntryType] {
  this: ValueEnum[Short, EntryType] =>
  import ZioJsonFieldInstances._

  /** Writes an entry as its `Short` value, as a JSON number. */
  implicit val zioJsonEncoder: JsonEncoder[EntryType] = ZioJson.encoder(this)

  /**
   * Reads a `Short` and returns the entry that has that value. A number that no entry has fails with a message such as
   * `'7' is not a member of enum Level`, and one outside the `Short` range, such as 70000, fails with
   * `expected a Short`.
   */
  implicit val zioJsonDecoder: JsonDecoder[EntryType] = ZioJson.decoder(this)

  /** Writes an entry as an object key holding its `Short` value, such as `"2"`. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)

  /**
   * Reads an object key as a `Short` and returns the entry that has that value. A key that isn't a `Short` fails with
   * `Invalid Short: 70000` for the key `70000`, and one that no entry has fails with a message such as
   * `'7' is not a member of enum Level`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * A [[ZioJsonValueEnum]] for a `StringEnum`: an entry is written as its `String` value, as a JSON string and as an
 * object key.
 */
trait StringZioJsonEnum[EntryType <: StringEnumEntry] extends ZioJsonValueEnum[String, EntryType] {
  this: ValueEnum[String, EntryType] =>

  /** Writes an entry as its `String` value, as a JSON string. */
  implicit val zioJsonEncoder: JsonEncoder[EntryType] = ZioJson.encoder(this)

  /**
   * Reads a JSON string and returns the entry that has that value, with the same letter case. A string that no entry
   * has fails with a message such as `'de' is not a member of enum Country`, and a JSON value that isn't a string fails
   * with `expected string`.
   */
  implicit val zioJsonDecoder: JsonDecoder[EntryType] = ZioJson.decoder(this)

  /** Writes an entry as an object key holding its `String` value. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)

  /**
   * Reads an object key as the entry whose `String` value equals it. Any other key fails with a message such as
   * `'de' is not a member of enum Country`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * A [[ZioJsonValueEnum]] for a `CharEnum`: an entry is written as its `Char` value, as a one-character JSON string and
 * as an object key. It brings its own field codecs for `Char`, which zio-json doesn't have.
 */
trait CharZioJsonEnum[EntryType <: CharEnumEntry] extends ZioJsonValueEnum[Char, EntryType] {
  this: ValueEnum[Char, EntryType] =>
  import ZioJsonFieldInstances._

  /** Writes an entry as its `Char` value, as a one-character JSON string. */
  implicit val zioJsonEncoder: JsonEncoder[EntryType] = ZioJson.encoder(this)

  /**
   * Reads a one-character JSON string and returns the entry that has that character. A character that no entry has
   * fails with a message such as `'c' is not a member of enum Grade`, and a string of any other length fails with
   * `expected single character string`.
   */
  implicit val zioJsonDecoder: JsonDecoder[EntryType] = ZioJson.decoder(this)

  /** Writes an entry as an object key holding its `Char` value, such as `"a"`. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)

  /**
   * Reads a one-character object key and returns the entry that has that character. A key of any other length fails
   * with `Invalid Char: bb` for the key `bb`, and a character that no entry has fails with a message such as
   * `'c' is not a member of enum Grade`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}

/**
 * A [[ZioJsonValueEnum]] for a `ByteEnum`: an entry is written as its `Byte` value, as a JSON number and as an object
 * key. It brings its own field codecs for `Byte`, which zio-json doesn't have.
 */
trait ByteZioJsonEnum[EntryType <: ByteEnumEntry] extends ZioJsonValueEnum[Byte, EntryType] {
  this: ValueEnum[Byte, EntryType] =>
  import ZioJsonFieldInstances._

  /** Writes an entry as its `Byte` value, as a JSON number. */
  implicit val zioJsonEncoder: JsonEncoder[EntryType] = ZioJson.encoder(this)

  /**
   * Reads a `Byte` and returns the entry that has that value. A number that no entry has fails with a message such as
   * `'5' is not a member of enum Bit`, and one outside the `Byte` range, such as 300, fails with `expected a Byte`.
   */
  implicit val zioJsonDecoder: JsonDecoder[EntryType] = ZioJson.decoder(this)

  /** Writes an entry as an object key holding its `Byte` value, such as `"1"`. */
  implicit val zioJsonKeyEncoder: JsonFieldEncoder[EntryType] = ZioJson.keyEncoder(this)

  /**
   * Reads an object key as a `Byte` and returns the entry that has that value. A key that isn't a `Byte` fails with
   * `Invalid Byte: 300` for the key `300`, and one that no entry has fails with a message such as
   * `'5' is not a member of enum Bit`.
   */
  implicit val zioJsonKeyDecoder: JsonFieldDecoder[EntryType] = ZioJson.keyDecoder(this)
}
