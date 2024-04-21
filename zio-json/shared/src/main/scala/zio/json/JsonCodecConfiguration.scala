package zio.json

/**
 * Implicit codec derivation configuration.
 *
 * @param sumTypeHandling see [[jsonDiscriminator]]
 * @param fieldNameMapping see [[jsonMemberNames]]
 * @param allowExtraFields see [[jsonNoExtraFields]]
 */
final case class JsonCodecConfiguration(
  sumTypeHandling: JsonCodecConfiguration.SumTypeHandling =
    JsonCodecConfiguration.SumTypeHandling.WrapperWithClassNameField,
  fieldNameMapping: JsonMemberFormat = IdentityFormat,
  allowExtraFields: Boolean = true
)

object JsonCodecConfiguration {
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
     * For sealed classes, will determine the name of the field for
     * disambiguating classes.
     *
     * The default is to not use a typehint field and instead
     * have an object with a single key that is the class name.
     * See [[WrapperWithClassNameField]].
     *
     * Note that using a discriminator is less performant, uses more memory, and may
     * be prone to DOS attacks that are impossible with the default encoding. In
     * addition, there is slightly less type safety when using custom product
     * encoders (which must write an unenforced object type). Only use this option
     * if you must model an externally defined schema.
     */
    final case class DiscriminatorField(name: String) extends SumTypeHandling {
      override def discriminatorField: Option[String] = Some(name)
    }
  }
}
