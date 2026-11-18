package zio.json.docs.gettingstarted

import zio.json.*

/**
 * Getting started tutorial — the Scala 3 `derives` form of the tutorial's codecs.
 */
object DerivesExample {

  // #region address
  final case class Address(street: String, city: String) derives JsonCodec
  // #endregion

  // #region enum
  enum Contact derives JsonCodec {
    case Email(address: String)
    case Phone(number: String)
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("address: " + Address("1 Main St", "Springfield").toJson)
    println("enum: " + (Contact.Phone("555-0100"): Contact).toJson)
    println("decoded: " + """{"Email":{"address":"a@example.com"}}""".fromJson[Contact])
  }
}
