package enumeratum.values

sealed abstract class ZioJsonOperatingSystem(val value: String) extends StringEnumEntry

case object ZioJsonOperatingSystem
    extends StringEnum[ZioJsonOperatingSystem]
    with StringZioJsonEnum[ZioJsonOperatingSystem] {

  case object Linux   extends ZioJsonOperatingSystem("linux")
  case object OSX     extends ZioJsonOperatingSystem("osx")
  case object Windows extends ZioJsonOperatingSystem("windows")
  case object Android extends ZioJsonOperatingSystem("android")

  val values = findValues

}
