package zio.json

/**
 * The platform-specific methods of [[JsonDecoder]]. On the JVM they decode from ZIO streams: `decodeJsonStream`,
 * `decodeJsonStreamInput` and `decodeJsonPipeline`. On Scala.js and Scala Native the trait is empty.
 *
 * @see
 *   [[https://zio.dev/zio-json/streaming/ Streaming]]
 */
trait JsonDecoderPlatformSpecific[A] { self: JsonDecoder[A] => }
