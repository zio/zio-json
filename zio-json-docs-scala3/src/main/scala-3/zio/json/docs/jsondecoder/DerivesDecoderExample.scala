package zio.json.docs.jsondecoder

import zio.json._
import zio.json.ast.Json

/**
 * JsonDecoder reference — derive a decoder with a `derives` clause, with and without a given configuration.
 */
object DerivesDecoderExample {

  // #region derives
  final case class Person(name: String, age: Int) derives JsonDecoder
  // #endregion

  object Custom {
    // #region given-config
    given JsonCodecConfiguration = JsonCodecConfiguration(allowExtraFields = false)

    final case class Strict(name: String) derives JsonDecoder
    // #endregion
  }

  // #region sum-type
  sealed trait Fruit derives JsonDecoder
  final case class Banana(curvature: Double) extends Fruit
  final case class Apple(poison: Boolean)    extends Fruit
  // #endregion

  def main(args: Array[String]): Unit = {
    println("person: " + """{"name":"Ada","age":36}""".fromJson[Person])
    println("strict: " + """{"name":"Ada","age":36}""".fromJson[Custom.Strict])
    println("fruit from AST: " + Json.Obj("Banana" -> Json.Obj(), "Apple" -> Json.Obj()).as[Fruit])
  }
}
