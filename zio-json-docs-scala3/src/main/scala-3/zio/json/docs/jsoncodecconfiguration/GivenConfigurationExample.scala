package zio.json.docs.jsoncodecconfiguration

import zio.json.*

/**
 * JsonCodecConfiguration reference — supply a configuration as a `given` on Scala 3.
 */
object GivenConfigurationExample {

  // #region given
  given JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)

  final case class User(firstName: String, lastName: String) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println("encoded: " + User("Ada", "Lovelace").toJson)
    println("decoded: " + """{"first_name":"Ada","last_name":"Lovelace"}""".fromJson[User])
  }
}
