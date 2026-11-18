package zio.json.docs.jsoncodecconfiguration

import zio.json.*

/**
 * JsonCodecConfiguration reference — on Scala 3, field types without an instance are derived with the outer
 * configuration.
 */
object NestedDerivationExample {

  // #region nested
  given JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)

  final case class Address(streetName: String)
  final case class Customer(fullName: String, homeAddress: Address) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit =
    println("encoded: " + Customer("Ada", Address("Main St")).toJson)
}
