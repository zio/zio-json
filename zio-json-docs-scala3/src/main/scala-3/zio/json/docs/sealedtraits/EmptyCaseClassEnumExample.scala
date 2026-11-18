package zio.json.docs.sealedtraits

import zio.json.*

/**
 * Sealed traits how-to — on Scala 3, `enumValuesAsStrings` (on by default) writes a family as strings only when every
 * case is a case object or a parameterless enum case. A case class with an empty parameter list keeps the whole family
 * in the object form.
 */
object EmptyCaseClassEnumExample {

  // #region case-objects
  sealed trait Color derives JsonCodec
  case object Red   extends Color
  case object Green extends Color
  // #endregion

  // #region empty-case-class
  sealed trait Signal derives JsonCodec
  case object On         extends Signal
  final case class Off() extends Signal
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Color: " + (Red: Color).toJson)
    println("Signal, case object: " + (On: Signal).toJson)
    println("Signal, empty case class: " + (Off(): Signal).toJson)
  }
}
