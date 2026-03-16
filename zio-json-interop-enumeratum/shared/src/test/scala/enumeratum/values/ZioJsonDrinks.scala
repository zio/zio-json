package enumeratum.values

sealed abstract class ZioJsonDrinks(val value: Short, @annotation.unused name: String) extends ShortEnumEntry

case object ZioJsonDrinks extends ShortEnum[ZioJsonDrinks] with ShortZioJsonEnum[ZioJsonDrinks] {

  case object OrangeJuice extends ZioJsonDrinks(value = 1, name = "oj")
  case object AppleJuice  extends ZioJsonDrinks(value = 2, name = "aj")
  case object Cola        extends ZioJsonDrinks(value = 3, name = "cola")
  case object Beer        extends ZioJsonDrinks(value = 4, name = "beer")

  val values = findValues

}
