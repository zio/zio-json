package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — string-literal union types as field values and as map keys.
 */
object Step5UnionTypesExample {

  // #region union
  type Currency = "EUR" | "USD" | "GBP"

  final case class Price(amount: BigDecimal, currency: Currency) derives JsonCodec
  // #endregion

  // #region union-keys
  final case class Rates(base: Currency, rates: Map[Currency, BigDecimal]) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Price encoded: " + Price(BigDecimal("9.99"), "EUR").toJson)
    println("Price decoded: " + """{"amount":5,"currency":"USD"}""".fromJson[Price])
    println("Price bad value: " + """{"amount":5,"currency":"JPY"}""".fromJson[Price])
    println("Rates encoded: " + Rates("EUR", Map("USD" -> BigDecimal("1.08"))).toJson)
    println("Rates bad key: " + """{"base":"EUR","rates":{"JPY":160}}""".fromJson[Rates])
    // #region union-alone
    val currency: Currency = "GBP"
    println("Currency encoded: " + currency.toJson)
    println("Currency decoded: " + "\"USD\"".fromJson[Currency])
    // #endregion
  }
}
