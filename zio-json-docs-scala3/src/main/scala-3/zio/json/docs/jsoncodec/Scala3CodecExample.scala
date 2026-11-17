package zio.json.docs.jsoncodec

import zio.json._

/**
 * JsonCodec on Scala 3: `derives JsonCodec` with the default configuration and with a given of your own.
 */
object Scala3CodecExample {

  // #region derives
  final case class Person(name: String, age: Int) derives JsonCodec
  // #endregion

  object SnakeCaseModel {
    // #region config
    given JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)

    final case class Account(accountId: Int, displayName: String) derives JsonCodec
    // #endregion
  }

  def main(args: Array[String]): Unit = {
    import SnakeCaseModel.Account

    println("default configuration: " + Person("Ada", 36).toJson)
    println("decoded: " + """{"name":"Ada","age":36}""".fromJson[Person])
    println("snake_case given: " + Account(1, "Ada").toJson)
  }
}
