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
  enum Fruit derives JsonCodec {
    case Banana(curvature: Double)
    case Apple(poison: Boolean)
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("address: " + Address("1 Main St", "Springfield").toJson)
    println("enum: " + (Fruit.Banana(0.5): Fruit).toJson)
    println("decoded: " + """{"Apple":{"poison":false}}""".fromJson[Fruit])
  }
}
