package zio.json

import zio.json.JsonCodecConfiguration.SumTypeHandling
import zio.json.JsonCodecConfiguration.SumTypeHandling.WrapperWithClassNameField

/**
 * Implicit codec derivation configuration.
 *
 * @param sumTypeHandling
 *   see [[jsonDiscriminator]]
 * @param fieldNameMapping
 *   see [[jsonMemberNames]]
 * @param allowExtraFields
 *   see [[jsonNoExtraFields]]
 * @param sumTypeMapping
 *   see [[jsonHintNames]]
 */
final case class JsonCodecConfiguration(
  sumTypeHandling: SumTypeHandling = WrapperWithClassNameField,
  fieldNameMapping: JsonMemberFormat = IdentityFormat,
  allowExtraFields: Boolean = true,
  sumTypeMapping: JsonMemberFormat = IdentityFormat,
  explicitNulls: Boolean = false,
  explicitEmptyCollections: Boolean = true
) {
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
    true
  )

  def copy(
    sumTypeHandling: SumTypeHandling = WrapperWithClassNameField.asInstanceOf[SumTypeHandling],
    fieldNameMapping: JsonMemberFormat = IdentityFormat.asInstanceOf[JsonMemberFormat],
    allowExtraFields: Boolean = true,
    sumTypeMapping: JsonMemberFormat = IdentityFormat.asInstanceOf[JsonMemberFormat],
    explicitNulls: Boolean = false,
    explicitEmptyCollections: Boolean = true
  ) = new JsonCodecConfiguration(
    sumTypeHandling,
    fieldNameMapping,
    allowExtraFields,
    sumTypeMapping,
    explicitNulls,
    explicitEmptyCollections
  )

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
    true
  )
}

object JsonCodecConfiguration {
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
    true
  )

  implicit val default: JsonCodecConfiguration = JsonCodecConfiguration()

  sealed trait SumTypeHandling {
    def discriminatorField: Option[String]
  }

  object SumTypeHandling {

    /**
     * Use an object with a single key that is the class name.
     */
    case object WrapperWithClassNameField extends SumTypeHandling {
      override def discriminatorField: Option[String] = None
    }

    /**
     * For sealed classes, will determine the name of the field for disambiguating classes.
     *
     * The default is to not use a typehint field and instead have an object with a single key that is the class name.
     * See [[WrapperWithClassNameField]].
     *
     * Note that using a discriminator is less performant, uses more memory, and may be prone to DOS attacks that are
     * impossible with the default encoding. In addition, there is slightly less type safety when using custom product
     * encoders (which must write an unenforced object type). Only use this option if you must model an externally
     * defined schema.
     */
    final case class DiscriminatorField(name: String) extends SumTypeHandling {
      override def discriminatorField: Option[String] = Some(name)
    }
  }
}
