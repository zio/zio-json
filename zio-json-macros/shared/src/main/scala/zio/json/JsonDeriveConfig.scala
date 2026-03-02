package zio.json

/**
 * Define a config for derivation macro
 */
sealed abstract class JsonDeriveConfig

object JsonDeriveConfig {
  // Derive a JsonCodec
  case object Codec extends JsonDeriveConfig

  // Derive only a JsonEncoder
  case object Encoder extends JsonDeriveConfig

  // Derive only a JsonDecoder
  case object Decoder extends JsonDeriveConfig
}
