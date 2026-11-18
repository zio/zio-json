package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — where a given `JsonCodecConfiguration` must be to affect `derives`.
 */
object Step2ConfigurationExample {

  // #region default
  final case class Visitor(firstName: String, lastVisit: String) derives JsonCodec
  // #endregion

  // #region companion
  final case class Customer(firstName: String, lastName: String) derives JsonCodec

  object Customer {
    given JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)
  }
  // #endregion

  // #region enclosing
  object billing {
    given JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = KebabCase)

    final case class Invoice(invoiceNumber: Long, dueDate: String) derives JsonCodec
    final case class Payment(invoiceNumber: Long, amountPaid: BigDecimal) derives JsonCodec
  }
  // #endregion

  // #region import
  object JsonConfig {
    given strict: JsonCodecConfiguration = JsonCodecConfiguration(allowExtraFields = false)
  }

  object orders {
    import JsonConfig.given

    final case class LineItem(sku: String, quantity: Int) derives JsonCodec
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Visitor (default): " + Visitor("Ada", "2026-11-17").toJson)
    println("Customer (companion given): " + Customer("Ada", "Lovelace").toJson)
    println("Invoice (enclosing given): " + billing.Invoice(42L, "2026-12-01").toJson)
    println("Payment (enclosing given): " + billing.Payment(42L, BigDecimal(10)).toJson)
    println("LineItem (imported given): " + """{"sku":"A1","quantity":2,"note":"x"}""".fromJson[orders.LineItem])
  }
}
