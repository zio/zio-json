package enumeratum.values

sealed abstract class ZioJsonContentType(val value: Long, @annotation.unused name: String) extends LongEnumEntry

case object ZioJsonContentType extends LongEnum[ZioJsonContentType] with LongZioJsonEnum[ZioJsonContentType] {

  val values = findValues

  case object Text  extends ZioJsonContentType(value = 1L, name = "text")
  case object Image extends ZioJsonContentType(value = 2L, name = "image")
  case object Video extends ZioJsonContentType(value = 3L, name = "video")
  case object Audio extends ZioJsonContentType(value = 4L, name = "audio")

}
