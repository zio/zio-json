package enumeratum.zio

import enumeratum.{ Enum, EnumEntry }

sealed trait ShirtSize extends EnumEntry with Product with Serializable

case object ShirtSize extends ZIOEnum[ShirtSize] with ZIOKeyEnum[ShirtSize] with Enum[ShirtSize] {

  case object Small  extends ShirtSize
  case object Medium extends ShirtSize
  case object Large  extends ShirtSize

  val values: IndexedSeq[ShirtSize] = findValues

}
