package zio.json.internal

import zio.json._
import scala.annotation.switch

private[json] class FieldEncoder[T, P](
  val p: P,
  val encoder: JsonEncoder[T],
  val encodedName: String,
  val prettyEncodedName: String,
  val name: String,
  private[this] val flags: Int
) {
  def skip(t: T): Boolean = (flags: @switch) match {
    case 0 => encoder.isEmpty(t) || encoder.isNothing(t)
    case 1 => encoder.isNothing(t)
    case 2 => encoder.isEmpty(t)
    case _ => false
  }
}

private[json] object FieldEncoder {
  def apply[T, P](
    p: P,
    name: String,
    encoder: JsonEncoder[T],
    withExplicitNulls: Boolean,
    withExplicitEmptyCollections: Boolean
  ): FieldEncoder[T, P] = {
    val encodedName = JsonEncoder.string.encodeJson(name, None).toString
    new FieldEncoder(
      p,
      encoder,
      encodedName + ':',
      encodedName + " : ",
      name, {
        if (withExplicitNulls) {
          if (withExplicitEmptyCollections) 3 else 2
        } else {
          if (withExplicitEmptyCollections) 1 else 0
        }
      }
    )
  }
}
