package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — derive with a `derives` clause.
 */
object Step1DerivesExample {

  // #region derives
  final case class Banana(curvature: Double) derives JsonCodec
  // #endregion

  // #region encoder-decoder
  final case class Receipt(id: Long, total: BigDecimal) derives JsonEncoder
  final case class Refund(receiptId: Long, reason: String) derives JsonDecoder
  // #endregion

  // #region explicit-given
  final case class Apple(poison: Boolean)

  object Apple {
    given JsonCodec[Apple] = DeriveJsonCodec.gen[Apple]
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Banana encoded: " + Banana(0.5).toJson)
    println("Banana decoded: " + """{"curvature":0.5}""".fromJson[Banana])
    println("Receipt encoded: " + Receipt(1L, BigDecimal("9.99")).toJson)
    println("Refund decoded: " + """{"receiptId":1,"reason":"damaged"}""".fromJson[Refund])
    println("Apple encoded: " + Apple(false).toJson)
  }
}
