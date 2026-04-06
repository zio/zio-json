package enumeratum.values

sealed abstract class ZioJsonAlphabet(val value: Char) extends CharEnumEntry

case object ZioJsonAlphabet extends CharEnum[ZioJsonAlphabet] with CharZioJsonEnum[ZioJsonAlphabet] {

  case object A extends ZioJsonAlphabet('A')
  case object B extends ZioJsonAlphabet('B')
  case object C extends ZioJsonAlphabet('C')
  case object D extends ZioJsonAlphabet('D')

  val values = findValues

}
