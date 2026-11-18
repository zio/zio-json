package zio.json.docs.wireformat

import zio.json.*

/**
 * JSON Shapes and Round Trips — how each kind of sum-type case is written on Scala 3, under the default configuration
 * (where `enumValuesAsStrings` is on), with `enumValuesAsStrings = false`, and with a discriminator field.
 */
object WireFormatExample {

  // #region defaults
  object Defaults {
    sealed trait Shape derives JsonCodec
    final case class Circle(r: Double) extends Shape
    case object Unknown                extends Shape

    sealed trait Color derives JsonCodec
    case object Red   extends Color
    case object Green extends Color

    sealed trait Signal derives JsonCodec
    case object On         extends Signal
    final case class Off() extends Signal

    enum Fruit derives JsonCodec {
      case Banana(curvature: Double)
      case Apple
    }

    enum Size derives JsonCodec {
      case Small, Large
    }
  }
  // #endregion

  // #region strings-off
  object StringsOff {
    given JsonCodecConfiguration = JsonCodecConfiguration(enumValuesAsStrings = false)

    sealed trait Color derives JsonCodec
    case object Red   extends Color
    case object Green extends Color

    enum Size derives JsonCodec {
      case Small, Large
    }
  }
  // #endregion

  // #region discriminator
  object Discriminated {
    @jsonDiscriminator("type")
    sealed trait Signal derives JsonCodec
    case object On         extends Signal
    final case class Off() extends Signal

    @jsonDiscriminator("type")
    enum Fruit derives JsonCodec {
      case Banana(curvature: Double)
      case Apple
    }

    @jsonDiscriminator("type")
    enum Size derives JsonCodec {
      case Small, Large
    }
  }
  // #endregion

  private def show[A: JsonCodec](label: String, value: A): Unit = {
    val json = value.toJson
    println(s"$label: $json, decodes back to ${json.fromJson[A]}")
  }

  def main(args: Array[String]): Unit = {
    import Defaults.*
    show[Shape]("Defaults, case class", Circle(1.0))
    show[Shape]("Defaults, case object next to a case class", Unknown)
    show[Color]("Defaults, case object in an all-object family", Red)
    show[Signal]("Defaults, case object next to an empty case class", On)
    show[Signal]("Defaults, empty case class", Off())
    show[Fruit]("Defaults, enum case with fields", Fruit.Banana(0.5))
    show[Fruit]("Defaults, parameterless case in a mixed enum", Fruit.Apple)
    show[Size]("Defaults, parameterless case in an all-parameterless enum", Size.Small)

    show[StringsOff.Color]("StringsOff, case object", StringsOff.Red)
    show[StringsOff.Size]("StringsOff, parameterless enum case", StringsOff.Size.Small)

    show[Discriminated.Signal]("Discriminated, case object", Discriminated.On)
    show[Discriminated.Signal]("Discriminated, empty case class", Discriminated.Off())
    show[Discriminated.Fruit]("Discriminated, enum case with fields", Discriminated.Fruit.Banana(0.5))
    show[Discriminated.Fruit]("Discriminated, parameterless case in a mixed enum", Discriminated.Fruit.Apple)
    show[Discriminated.Size]("Discriminated, parameterless enum case", Discriminated.Size.Small)
  }
}
