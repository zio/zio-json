package zio.json.docs.scala3derivation

import zio.json.*

/**
 * Derive codecs on Scala 3 — type-parameterised case classes.
 */
object Step6GenericExample {

  // #region derives-generic
  final case class Box[A](value: A) derives JsonCodec
  // #endregion

  // #region given-generic
  final case class Envelope[A](id: String, payload: A)

  object Envelope {
    given [A: JsonEncoder: JsonDecoder]: JsonCodec[Envelope[A]] = DeriveJsonCodec.gen
  }
  // #endregion

  // #region separate-instances
  final case class Sku(code: String)

  object Sku {
    given JsonEncoder[Sku] = DeriveJsonEncoder.gen
    given JsonDecoder[Sku] = DeriveJsonDecoder.gen
  }
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Box[Int] encoded: " + Box(1).toJson)
    println("Box[List[String]] decoded: " + """{"value":["a","b"]}""".fromJson[Box[List[String]]])
    println("Envelope[Sku] encoded: " + Envelope("e1", Sku("A-1")).toJson)
    println("Envelope[Sku] decoded: " + """{"id":"e1","payload":{"code":"A-1"}}""".fromJson[Envelope[Sku]])
  }
}
