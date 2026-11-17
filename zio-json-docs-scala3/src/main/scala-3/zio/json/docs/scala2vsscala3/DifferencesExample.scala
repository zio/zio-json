package zio.json.docs.scala2vsscala3

import zio.json.*
import zio.json.ast.Json

/**
 * Scala 2 and Scala 3 differences — the Scala 3 side of each difference.
 */
object DifferencesExample {

  // #region nested
  final case class Address(street: String)
  final case class Person(name: String, address: Address) derives JsonCodec
  // #endregion

  // #region enum-default
  sealed trait Color derives JsonCodec
  case object Red   extends Color
  case object Green extends Color
  // #endregion

  object strings {
    // #region empty-case-class
    given JsonCodecConfiguration = JsonCodecConfiguration(enumValuesAsStrings = true)

    sealed trait Signal derives JsonCodec
    case object Go          extends Signal
    final case class Stop() extends Signal
    // #endregion
  }

  object handwritten {
    // #region handwritten
    given JsonCodecConfiguration = JsonCodecConfiguration(enumValuesAsStrings = true)

    sealed trait Light
    case object Amber extends Light
    case object Blue  extends Light

    given JsonCodec[Amber.type] = JsonCodec.string.transform(_ => Amber, _ => "AMBER")

    object Light {
      given JsonCodec[Light] = DeriveJsonCodec.gen
    }
    // #endregion
  }

  object emptyfield {
    // #region empty-field
    given JsonCodecConfiguration =
      JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(encoding = false))

    final case class Marker() derives JsonCodec
    final case class Holder(name: String, marker: Marker) derives JsonCodec
    // #endregion
  }

  // #region recursive
  final case class Node(name: String, children: List[Node]) derives JsonCodec
  // #endregion

  // #region multi-key
  sealed trait Fruit derives JsonCodec
  final case class Banana(curvature: Double) extends Fruit
  final case class Apple(poison: Boolean)    extends Fruit
  // #endregion

  def main(args: Array[String]): Unit = {
    println("nested: " + Person("Ada", Address("Main St")).toJson)
    println("case objects: " + (Red: Color).toJson)
    println("Go: " + (strings.Go: strings.Signal).toJson)
    println("Stop(): " + (strings.Stop(): strings.Signal).toJson)
    println("Amber: " + (handwritten.Amber: handwritten.Light).toJson)
    println("Blue: " + (handwritten.Blue: handwritten.Light).toJson)
    println("empty field: " + emptyfield.Holder("a", emptyfield.Marker()).toJson)
    println("empty field round trip: " + emptyfield.Holder("a", emptyfield.Marker()).toJson.fromJson[emptyfield.Holder])
    println("recursive: " + Node("root", List(Node("leaf", Nil))).toJson)
    println("multi-key: " + Json.Obj("Banana" -> Json.Obj(), "Apple" -> Json.Obj()).as[Fruit])
  }
}
