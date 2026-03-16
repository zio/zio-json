package enumeratum.values

sealed abstract class ZioJsonMovieGenre extends IntEnumEntry

case object ZioJsonMovieGenre extends IntEnum[ZioJsonMovieGenre] with IntZioJsonEnum[ZioJsonMovieGenre] {

  case object Action extends ZioJsonMovieGenre {
    val value = 1
  }
  case object Comedy extends ZioJsonMovieGenre {
    val value: Int = 2
  }
  case object Romance extends ZioJsonMovieGenre {
    val value = 3
  }

  val values = findValues

}
