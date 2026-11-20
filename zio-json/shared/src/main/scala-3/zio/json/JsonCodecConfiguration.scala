package zio.json

import zio.json.JsonCodecConfiguration.SumTypeHandling
import zio.json.JsonCodecConfiguration.SumTypeHandling.WrapperWithClassNameField

/**
 * Sets whether derived codecs write and require fields that hold an empty collection. It is the
 * `explicitEmptyCollections` option of [[JsonCodecConfiguration]]; [[jsonExplicitEmptyCollections]] overrides it for
 * one case class or field.
 *
 * With `encoding = false`, the derived encoder leaves out a field whose collection is empty, such as an empty `List` or
 * `Map`. On Scala 2 it also leaves out a field whose type is a case class without fields; Scala 3 writes it as `{}`.
 * With `decoding = false`, the derived decoder reads a missing collection field as an empty collection. With the
 * defaults, `true` for both, empty collections are written, and a missing collection field fails to decode with
 * `.items(missing)`, where `items` is the field name.
 *
 * {{{
 * final case class Cart(owner: String, items: List[String])
 *
 * object Cart {
 *   implicit val config: JsonCodecConfiguration =
 *     JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(encoding = false))
 *
 *   implicit val codec: JsonCodec[Cart] = DeriveJsonCodec.gen[Cart]
 * }
 *
 * Cart("ann", Nil).toJson              // {"owner":"ann"}
 * """{"owner":"ann"}""".fromJson[Cart] // Left(.items(missing))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
case class ExplicitEmptyCollections(encoding: Boolean = true, decoding: Boolean = true)

/**
 * The options that `DeriveJsonEncoder.gen`, `DeriveJsonDecoder.gen` and `DeriveJsonCodec.gen` read when they derive an
 * instance. Put an implicit value in scope (a `given` also works on Scala 3) where the instance is derived; without
 * one, `JsonCodecConfiguration.default` applies. Annotations on a type or a field, such as [[jsonMemberNames]],
 * override the configuration for that type or field.
 *
 * The default of `enumValuesAsStrings` differs between Scala versions: `false` on Scala 2 and `true` on Scala 3.
 *
 * {{{
 * final case class Account(accountId: Int, ownerName: String)
 *
 * object Account {
 *   implicit val config: JsonCodecConfiguration =
 *     JsonCodecConfiguration(fieldNameMapping = SnakeCase, allowExtraFields = false)
 *
 *   implicit val codec: JsonCodec[Account] = DeriveJsonCodec.gen[Account]
 * }
 *
 * Account(7, "Ada").toJson // {"account_id":7,"owner_name":"Ada"}
 * """{"account_id":7,"owner_name":"Ada","admin":true}""".fromJson[Account] // Left((invalid extra field))
 * }}}
 *
 * @param sumTypeHandling
 *   how a case of a sealed trait is tagged: `WrapperWithClassNameField` (the default) wraps it in an object whose only
 *   key is the type hint, `DiscriminatorField(name)` writes the hint into a field `name` of the object of the case.
 *   [[jsonDiscriminator]] overrides it for one sealed type.
 * @param fieldNameMapping
 *   the format that turns case class field names into JSON names, `IdentityFormat` by default. [[jsonMemberNames]]
 *   overrides it for one class, and [[jsonField]] for one field.
 * @param allowExtraFields
 *   when `false`, a derived decoder of a case class fails with `(invalid extra field)` at a key that is neither a JSON
 *   field name nor an alias of the class, as with [[jsonNoExtraFields]]. `true` by default.
 * @param sumTypeMapping
 *   the format that turns the short type names of the cases of a sealed trait into type hints, `IdentityFormat` by
 *   default. [[jsonHintNames]] overrides it for one sealed type, and [[jsonHint]] for one case.
 * @param explicitNulls
 *   when `true`, the derived encoder writes an `Option` field that is `None` as `null` instead of leaving the key out,
 *   as with [[jsonExplicitNull]]. `false` by default. Decoding is not affected.
 * @param explicitEmptyCollections
 *   whether fields that hold an empty collection are written and required, see [[ExplicitEmptyCollections]].
 *   [[jsonExplicitEmptyCollections]] overrides it for one class or field.
 * @param enumValuesAsStrings
 *   when `true`, the cases of a sealed trait whose cases all have no fields are written as JSON strings, such as
 *   `"Red"`, instead of objects such as `{"Red":{}}`. On Scala 2 a case class with an empty parameter list counts as
 *   such a case, and the string form is skipped when a case has a codec of its own; on Scala 3 only case objects and
 *   enum cases without parameters count. It has no effect when a discriminator is set. `false` by default on Scala 2
 *   and `true` on Scala 3.
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class JsonCodecConfiguration(
  sumTypeHandling: SumTypeHandling = WrapperWithClassNameField,
  fieldNameMapping: JsonMemberFormat = IdentityFormat,
  allowExtraFields: Boolean = true,
  sumTypeMapping: JsonMemberFormat = IdentityFormat,
  explicitNulls: Boolean = false,
  explicitEmptyCollections: ExplicitEmptyCollections = ExplicitEmptyCollections(),
  enumValuesAsStrings: Boolean = true
) {

  /**
   * Creates a configuration from the first six options. `enumValuesAsStrings` gets its default: `false` on Scala 2 and
   * `true` on Scala 3.
   */
  def this(
    sumTypeHandling: SumTypeHandling,
    fieldNameMapping: JsonMemberFormat,
    allowExtraFields: Boolean,
    sumTypeMapping: JsonMemberFormat,
    explicitNulls: Boolean,
    explicitEmptyCollections: ExplicitEmptyCollections
  ) = this(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    explicitEmptyCollections,
    true
  )

  /**
   * Creates a configuration from the first five options. `explicitEmptyCollections` is `ExplicitEmptyCollections()`,
   * and `enumValuesAsStrings` gets its default: `false` on Scala 2 and `true` on Scala 3.
   */
  def this(
    sumTypeHandling: SumTypeHandling,
    fieldNameMapping: JsonMemberFormat,
    allowExtraFields: Boolean,
    sumTypeMapping: JsonMemberFormat,
    explicitNulls: Boolean
  ) = this(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    ExplicitEmptyCollections(),
    true
  )

  /**
   * Returns a configuration with the given options. Unlike the `copy` of a case class, each parameter defaults to the
   * library default, not to the value in this configuration, so every option that is not named is reset:
   * `JsonCodecConfiguration(fieldNameMapping = SnakeCase).copy(allowExtraFields = false)` has `IdentityFormat` as its
   * `fieldNameMapping`. It ignores the configuration it is called on, so name every option whose value should be kept.
   */
  def copy(
    sumTypeHandling: SumTypeHandling = WrapperWithClassNameField.asInstanceOf[SumTypeHandling],
    fieldNameMapping: JsonMemberFormat = IdentityFormat.asInstanceOf[JsonMemberFormat],
    allowExtraFields: Boolean = true,
    sumTypeMapping: JsonMemberFormat = IdentityFormat.asInstanceOf[JsonMemberFormat],
    explicitNulls: Boolean = false,
    explicitEmptyCollections: ExplicitEmptyCollections = ExplicitEmptyCollections(),
    enumValuesAsStrings: Boolean = true
  ) = new JsonCodecConfiguration(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    explicitEmptyCollections,
    enumValuesAsStrings
  )

  /** Returns a configuration with the first six options replaced. `enumValuesAsStrings` keeps its value. */
  def copy(
    sumTypeHandling: SumTypeHandling,
    fieldNameMapping: JsonMemberFormat,
    allowExtraFields: Boolean,
    sumTypeMapping: JsonMemberFormat,
    explicitNulls: Boolean,
    explicitEmptyCollections: ExplicitEmptyCollections
  ) = new JsonCodecConfiguration(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    explicitEmptyCollections,
    this.enumValuesAsStrings
  )

  /**
   * Returns a configuration with the first five options replaced. `explicitEmptyCollections` and `enumValuesAsStrings`
   * keep their values.
   */
  def copy(
    sumTypeHandling: SumTypeHandling,
    fieldNameMapping: JsonMemberFormat,
    allowExtraFields: Boolean,
    sumTypeMapping: JsonMemberFormat,
    explicitNulls: Boolean
  ) = new JsonCodecConfiguration(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    this.explicitEmptyCollections,
    this.enumValuesAsStrings
  )
}

/**
 * Factory methods for [[JsonCodecConfiguration]], the default configuration, and the ways a case of a sealed trait is
 * tagged.
 */
object JsonCodecConfiguration {

  /**
   * Creates a configuration from the first six options. `enumValuesAsStrings` gets its default: `false` on Scala 2 and
   * `true` on Scala 3.
   */
  def apply(
    sumTypeHandling: SumTypeHandling,
    fieldNameMapping: JsonMemberFormat,
    allowExtraFields: Boolean,
    sumTypeMapping: JsonMemberFormat,
    explicitNulls: Boolean,
    explicitEmptyCollections: ExplicitEmptyCollections
  ) = new JsonCodecConfiguration(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    explicitEmptyCollections,
    true
  )

  /**
   * Creates a configuration from the first five options. `explicitEmptyCollections` is `ExplicitEmptyCollections()`,
   * and `enumValuesAsStrings` gets its default: `false` on Scala 2 and `true` on Scala 3.
   */
  def apply(
    sumTypeHandling: SumTypeHandling,
    fieldNameMapping: JsonMemberFormat,
    allowExtraFields: Boolean,
    sumTypeMapping: JsonMemberFormat,
    explicitNulls: Boolean
  ) = new JsonCodecConfiguration(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    ExplicitEmptyCollections(),
    true
  )

  /**
   * The configuration with every option at its default, which derivation uses when no other configuration is in
   * implicit scope. Its `enumValuesAsStrings` is `false` on Scala 2 and `true` on Scala 3.
   */
  implicit val default: JsonCodecConfiguration = JsonCodecConfiguration()

  /**
   * How derivation tags the case of a sealed trait in JSON: `WrapperWithClassNameField` (the default) or
   * `DiscriminatorField`.
   */
  sealed trait SumTypeHandling {

    /**
     * The name of the field that holds the type hint: `Some(name)` for `DiscriminatorField(name)`, `None` for
     * `WrapperWithClassNameField`.
     */
    def discriminatorField: Option[String]
  }

  /** The two kinds of [[SumTypeHandling]]. */
  object SumTypeHandling {

    /**
     * Wraps each case in a JSON object whose only key is the type hint of the case, by default its short type name:
     * `{"Circle":{"radius":1.5}}`. The default `sumTypeHandling`.
     */
    case object WrapperWithClassNameField extends SumTypeHandling {
      override def discriminatorField: Option[String] = None
    }

    /**
     * Writes the type hint as a field `name` inside the object of each case, for every sealed trait derived with this
     * configuration: `{"type":"Circle","radius":1.5}`. [[jsonDiscriminator]] on a sealed type overrides it.
     *
     * Do not combine it with `allowExtraFields = false`: the decoder of each case sees the hint field as an extra
     * field, so decoding fails with `(Circle)(invalid extra field)`, where `Circle` is the type hint.
     *
     * Note that using a discriminator is less performant, uses more memory, and may be prone to DOS attacks that are
     * impossible with the default encoding. A hand-written encoder of a case must write a JSON object: for any other
     * JSON, `toJson` writes invalid JSON and `toJsonAST` returns a `Left`. Use it when an external schema requires a
     * discriminator field.
     */
    final case class DiscriminatorField(name: String) extends SumTypeHandling {
      override def discriminatorField: Option[String] = Some(name)
    }
  }
}
