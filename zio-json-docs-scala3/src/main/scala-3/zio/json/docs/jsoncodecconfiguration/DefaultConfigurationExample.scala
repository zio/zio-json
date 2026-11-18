package zio.json.docs.jsoncodecconfiguration

import zio.json.*

/**
 * JsonCodecConfiguration reference — `derives` without a given uses `JsonCodecConfiguration.default`.
 */
object DefaultConfigurationExample {

  // #region default
  final case class Account(accountId: Long, ownerName: String) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit =
    println("encoded: " + Account(7L, "Ada").toJson)
}
