package zio.json

import scala.collection.immutable

/**
 * Codec instances that depend on the Scala version, mixed into the `JsonCodec` companion. Scala 2.13 and Scala 3 add an
 * instance for `immutable.ArraySeq`, and Scala 3 also adds `derived` and an instance for `IArray`. Scala 2.12 adds
 * none.
 */
trait JsonCodecVersionSpecific {

  /**
   * Returns a codec for `immutable.ArraySeq[A]` that reads and writes a JSON array, built from `JsonEncoder.arraySeq`
   * and `JsonDecoder.arraySeq`. Needs a `ClassTag` for `A`.
   */
  implicit def arraySeq[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[immutable.ArraySeq[A]] =
    JsonCodec(JsonEncoder.arraySeq[A], JsonDecoder.arraySeq[A])
}
