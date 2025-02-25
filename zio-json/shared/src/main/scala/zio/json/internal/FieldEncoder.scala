package zio.json.internal

import zio.Chunk
import zio.json._
import zio.json.ast.Json
import scala.annotation.switch

private[json] class FieldEncoder[T, P](
  val p: P,
  val name: String,
  val encoder: JsonEncoder[T],
  val flags: Int
) {
  val encodedName: String = JsonEncoder.string.encodeJson(name, None).toString

  def encodeOrDefault(t: T)(
    encode: () => Either[String, Chunk[(String, Json)]],
    default: Either[String, Chunk[(String, Json)]]
  ): Either[String, Chunk[(String, Json)]] =
    (flags: @switch) match {
      case 0 => if (encoder.isEmpty(t) || encoder.isNothing(t)) default else encode()
      case 1 => if (encoder.isNothing(t)) default else encode()
      case 2 => if (encoder.isEmpty(t)) default else encode()
      case _ => encode()
    }
}

private[json] object FieldEncoder {
  def apply[T, P](
    p: P,
    name: String,
    encoder: JsonEncoder[T],
    withExplicitNulls: Boolean,
    withExplicitEmptyCollections: Boolean
  ): FieldEncoder[T, P] =
    new FieldEncoder(
      p,
      name,
      encoder, {
        if (withExplicitNulls) {
          if (withExplicitEmptyCollections) 3 else 2
        } else {
          if (withExplicitEmptyCollections) 1 else 0
        }
      }
    )
}
