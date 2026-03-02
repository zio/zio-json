package zio.json

/**
 * Scala 3 does not support Scala 2 macro annotations used by zio-json-macros.
 *
 * Keep this annotation for source compatibility and derive codecs manually in
 * companion objects with `DeriveJsonCodec.gen`, `DeriveJsonEncoder.gen`, or
 * `DeriveJsonDecoder.gen`.
 */
class jsonDerive(
  val config: JsonDeriveConfig = JsonDeriveConfig.Codec
) extends scala.annotation.StaticAnnotation
