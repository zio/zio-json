package zio.json

/**
 * The platform-specific methods of [[JsonEncoder]]. On the JVM they encode values to a `zio.stream.ZStream` of
 * characters: `encodeJsonStream`, `encodeJsonLinesPipeline` and `encodeJsonArrayPipeline`. On Scala.js and Scala Native
 * the trait has no methods.
 *
 * @see
 *   [[https://zio.dev/zio-json/streaming/ Streaming]]
 */
trait JsonEncoderPlatformSpecific[A] { self: JsonEncoder[A] => }
