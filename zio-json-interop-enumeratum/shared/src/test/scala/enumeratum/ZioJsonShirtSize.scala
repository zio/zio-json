package enumeratum

sealed trait ZioJsonShirtSize extends EnumEntry

case object ZioJsonShirtSize
    extends ZioJsonEnum[ZioJsonShirtSize]
    with ZioJsonKeyEnum[ZioJsonShirtSize]
    with Enum[ZioJsonShirtSize] {

  case object Small  extends ZioJsonShirtSize
  case object Medium extends ZioJsonShirtSize
  case object Large  extends ZioJsonShirtSize

  val values = findValues

}
