package enumeratum.values

sealed abstract class ZioJsonLibraryItem(val value: Int, val name: String) extends IntEnumEntry

case object ZioJsonLibraryItem extends IntEnum[ZioJsonLibraryItem] with IntZioJsonEnum[ZioJsonLibraryItem] {

  case object Book     extends ZioJsonLibraryItem(value = 1, name = "book")
  case object Movie    extends ZioJsonLibraryItem(name = "movie", value = 2)
  case object Magazine extends ZioJsonLibraryItem(3, "magazine")
  case object CD       extends ZioJsonLibraryItem(4, name = "cd")

  val values = findValues

}
