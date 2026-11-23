package zio.json

/**
 * Codec instances that depend on the Scala version, mixed into the `JsonCodec` companion. Scala 2.13 and Scala 3 add an
 * instance for `immutable.ArraySeq`, and Scala 3 also adds `derived` and an instance for `IArray`. Scala 2.12 adds
 * none.
 */
trait JsonCodecVersionSpecific
