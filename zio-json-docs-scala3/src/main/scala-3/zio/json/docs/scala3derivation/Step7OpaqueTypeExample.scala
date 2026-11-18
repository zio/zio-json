package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — opaque types need a hand-written codec.
 */
object Step7OpaqueTypeExample {

  // #region opaque
  object ids {
    opaque type UserId = String

    object UserId {
      def apply(value: String): UserId = value

      extension (id: UserId) def value: String = id

      given JsonCodec[UserId] = JsonCodec.string.transform(UserId(_), _.value)
    }
  }

  import ids.UserId

  final case class User(id: UserId, name: String) derives JsonCodec
  // #endregion

  // #region validated
  object quantities {
    opaque type Quantity = Int

    object Quantity {
      def make(n: Int): Either[String, Quantity] =
        if (n > 0) Right(n) else Left(s"quantity must be positive, got $n")

      extension (q: Quantity) def toInt: Int = q

      given JsonCodec[Quantity] = JsonCodec.int.transformOrFail(make, _.toInt)
    }
  }

  import quantities.Quantity

  final case class OrderLine(sku: String, quantity: Quantity) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println("User encoded: " + User(UserId("u-1"), "Ada").toJson)
    println("User decoded: " + """{"id":"u-2","name":"Grace"}""".fromJson[User])
    println("OrderLine decoded: " + """{"sku":"A1","quantity":3}""".fromJson[OrderLine])
    println("OrderLine invalid: " + """{"sku":"A1","quantity":0}""".fromJson[OrderLine])
  }
}
