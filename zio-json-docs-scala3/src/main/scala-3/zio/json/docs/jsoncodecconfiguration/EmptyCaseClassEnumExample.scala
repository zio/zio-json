package zio.json.docs.jsoncodecconfiguration

import zio.json.*

/**
 * JsonCodecConfiguration reference — on Scala 3, `enumValuesAsStrings` ignores case classes with no fields.
 */
object EmptyCaseClassEnumExample {

  // #region empty-case-class
  given JsonCodecConfiguration = JsonCodecConfiguration(enumValuesAsStrings = true)

  sealed trait Signal derives JsonCodec
  case class Stop()  extends Signal
  case object Go     extends Signal
  // #endregion

  def main(args: Array[String]): Unit = {
    println("Stop(): " + (Stop(): Signal).toJson)
    println("Go:     " + (Go: Signal).toJson)
  }
}
