package zio.json.docs.ziojsonmacros

import zio.json.*

/**
 * zio-json-macros reference — the Scala 3 replacement for `@jsonDerive` is a `derives` clause.
 */
object DerivesInsteadExample {

  // #region derives
  final case class Watermelon(pips: Int) derives JsonCodec
  final case class Melon(pips: Int) derives JsonEncoder
  final case class Gourd(pips: Int) derives JsonDecoder
  final case class Box[+A](a: A) derives JsonCodec
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Watermelon: " + Watermelon(3).toJson)
    println("Melon:      " + Melon(2).toJson)
    println("Gourd:      " + """{"pips":1}""".fromJson[Gourd])
    println("Box:        " + Box(List(1, 2)).toJson + " " + """{"a":"x"}""".fromJson[Box[String]])
  }
}
