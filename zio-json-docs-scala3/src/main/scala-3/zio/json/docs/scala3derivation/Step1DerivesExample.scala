package zio.json.docs.scala3derivation

import zio.json._

/**
 * Derive codecs on Scala 3 — derive with a `derives` clause.
 */
object Step1DerivesExample {

  // #region derives
  final case class Banana(curvature: Double) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println(Banana(0.5).toJson)
    println("""{"curvature":0.5}""".fromJson[Banana])
  }
}
