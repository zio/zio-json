package zio.json

sealed trait JsonMemberFormat extends (String => String)

private[json] object JsonMemberFormat {

  /**
   * ~~Stolen~~ Borrowed from jsoniter-scala by Andriy Plokhotnyuk
   * (he even granted permission for this, imagine that!)
   */

  import java.lang.Character._

  def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String =
    if (s.indexOf('_') == -1 && s.indexOf('-') == -1) {
      if (s.isEmpty) s
      else {
        val ch = s.charAt(0)
        val fixedCh =
          if (toPascal) toUpperCase(ch)
          else toLowerCase(ch)
        s"$fixedCh${s.substring(1)}"
      }
    } else {
      val len             = s.length
      val sb              = new StringBuilder(len)
      var i               = 0
      var isPrecedingDash = toPascal
      while (i < len) isPrecedingDash = {
        val ch = s.charAt(i)
        i += 1
        (ch == '_' || ch == '-') || {
          val fixedCh =
            if (isPrecedingDash) toUpperCase(ch)
            else toLowerCase(ch)
          sb.append(fixedCh)
          false
        }
      }
      sb.toString
    }

  def enforceSnakeOrKebabCase(s: String, separator: Char): String = {
    val len                      = s.length
    val sb                       = new StringBuilder(len << 1)
    var i                        = 0
    var isPrecedingNotUpperCased = false
    while (i < len) isPrecedingNotUpperCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (!isUpperCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingNotUpperCased || i > 1 && i < len && !isUpperCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

  def enforceSnakeOrKebabCaseSeparateNumbers(s: String, separator: Char): String = {
    val len                   = s.length
    val sb                    = new StringBuilder(len << 1)
    var i                     = 0
    var isPrecedingLowerCased = false
    while (i < len) isPrecedingLowerCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (isLowerCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingLowerCased || i > 1 && i < len && isLowerCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }
}

case class CustomCase(f: String => String) extends JsonMemberFormat {
  override def apply(memberName: String): String = f(memberName)
}
case object SnakeCase extends JsonMemberFormat {
  override def apply(memberName: String): String = JsonMemberFormat.enforceSnakeOrKebabCase(memberName, '_')
}
case object CamelCase extends JsonMemberFormat {
  override def apply(memberName: String): String =
    JsonMemberFormat.enforceCamelOrPascalCase(memberName, toPascal = false)
}
case object PascalCase extends JsonMemberFormat {
  override def apply(memberName: String): String =
    JsonMemberFormat.enforceCamelOrPascalCase(memberName, toPascal = true)
}
case object KebabCase extends JsonMemberFormat {
  override def apply(memberName: String): String = JsonMemberFormat.enforceSnakeOrKebabCase(memberName, '-')
}
case object IdentityFormat extends JsonMemberFormat {
  override def apply(memberName: String): String = memberName
}

/** zio-json version 0.3.0 formats. abc123Def -> abc_123_def */
object ziojson_03 {
  case object SnakeCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      JsonMemberFormat.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '_')
  }
  case object KebabCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      JsonMemberFormat.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '-')
  }
}

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
