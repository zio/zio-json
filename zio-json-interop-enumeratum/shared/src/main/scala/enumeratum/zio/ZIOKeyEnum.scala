package enumeratum.zio

import enumeratum.{ Enum, EnumEntry }
import zio.json.{ JsonFieldDecoder, JsonFieldEncoder }

/**
 * Helper trait that adds implicit ZIO JsonFieldEncoder/JsonFieldDecoder for an [[Enum]]'s members.
 */
trait ZIOKeyEnum[A <: EnumEntry] { this: Enum[A] =>
  implicit val jsonKeyEncoder: JsonFieldEncoder[A] = ZIOJson.keyEncoder
  implicit val jsonKeyDecoder: JsonFieldDecoder[A] = ZIOJson.keyDecoder(this)
}
