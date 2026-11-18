package zio.json.docs.jsonencoder

import zio.json._

/**
 * JsonEncoder on Scala 3: `derives`, `IArray` and string-literal unions.
 */
object Scala3EncoderExample {

  // #region derives
  final case class Person(name: String, age: Int) derives JsonEncoder
  // #endregion

  // #region union
  type Color = "red" | "green" | "blue"

  final case class Paint(color: Color, stock: Map[Color, Int]) derives JsonEncoder
  // #endregion

  def main(args: Array[String]): Unit = {
    println("derives: " + Person("Ada", 36).toJson)

    // #region iarray
    val primes: IArray[Int] = IArray(2, 3, 5, 7)
    println("IArray: " + primes.toJson)
    // #endregion

    val red: Color = "red"
    println("union: " + red.toJson)
    println("union in a case class: " + Paint("green", Map("red" -> 1, "blue" -> 2)).toJson)
  }
}
