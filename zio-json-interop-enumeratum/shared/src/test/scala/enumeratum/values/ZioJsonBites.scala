package enumeratum.values

sealed abstract class ZioJsonBites(val value: Byte) extends ByteEnumEntry

object ZioJsonBites extends ByteEnum[ZioJsonBites] with ByteZioJsonEnum[ZioJsonBites] {
  val values = findValues

  case object OneByte   extends ZioJsonBites(1)
  case object TwoByte   extends ZioJsonBites(2)
  case object ThreeByte extends ZioJsonBites(3)
  case object FourByte  extends ZioJsonBites(4)
}
