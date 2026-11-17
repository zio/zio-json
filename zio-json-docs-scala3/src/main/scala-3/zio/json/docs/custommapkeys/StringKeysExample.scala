package zio.json.docs.custommapkeys

import zio.json._

/**
 * Use your own type as a Map key — Scala 3 key types that are subtypes of `String` need no key decoder.
 */
object StringKeysExample {

  // #region union-keys
  type Channel = "web" | "store" | "phone"

  final case class ChannelSales(byChannel: Map[Channel, Int]) derives JsonCodec
  // #endregion

  // #region opaque-keys
  object Skus {
    opaque type Sku <: String = String

    def apply(code: String): Either[String, Sku] =
      if (code.matches("[A-Z]{3}-[0-9]{3}")) Right(code) else Left(s"not a SKU: $code")

    given JsonDecoder[Sku] = JsonDecoder.string.mapOrFail(apply)
  }
  import Skus.Sku

  final case class Stock(bySku: Map[Sku, Int]) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println("union encode: " + ChannelSales(Map("web" -> 13, "store" -> 8)).toJson)
    println("union decode: " + """{"byChannel":{"web":13,"store":8}}""".fromJson[ChannelSales])
    println("union bad key: " + """{"byChannel":{"fax":1}}""".fromJson[ChannelSales])
    val sku = Skus("ABC-123").toOption.get
    println("opaque encode: " + Stock(Map(sku -> 4)).toJson)
    println("opaque decode: " + """{"bySku":{"ABC-123":4}}""".fromJson[Stock])
    println("opaque bad key: " + """{"bySku":{"abc":4}}""".fromJson[Stock])
  }
}
