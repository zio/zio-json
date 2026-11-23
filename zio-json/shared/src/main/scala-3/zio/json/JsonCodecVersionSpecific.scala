package zio.json

import scala.collection.immutable

/**
 * Codec instances that depend on the Scala version, mixed into the `JsonCodec` companion. Scala 2.13 and Scala 3 add an
 * instance for `immutable.ArraySeq`, and Scala 3 also adds `derived` and an instance for `IArray`. Scala 2.12 adds
 * none.
 */
private[json] trait JsonCodecVersionSpecific {

  /**
   * Returns a codec for `immutable.ArraySeq[A]` that reads and writes a JSON array, built from `JsonEncoder.arraySeq`
   * and `JsonDecoder.arraySeq`. Needs a `ClassTag` for `A`.
   */
  implicit def arraySeq[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[immutable.ArraySeq[A]] =
    JsonCodec(JsonEncoder.arraySeq[A], JsonDecoder.arraySeq[A])

  /**
   * Derives a codec for the case class or sealed trait `A` with `DeriveJsonCodec.gen`, using the
   * `JsonCodecConfiguration` given in scope. It also makes `derives JsonCodec` work on a Scala 3 definition.
   */
  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonCodec[A] = DeriveJsonCodec.gen[A]

  /**
   * Returns a codec for `IArray[A]` that reads and writes a JSON array, built from `JsonEncoder.iArray` and
   * `JsonDecoder.iArray`. Needs a `ClassTag` for `A`.
   */
  implicit def iArray[A: JsonEncoder: JsonDecoder: reflect.ClassTag]: JsonCodec[IArray[A]] =
    JsonCodec(JsonEncoder.iArray[A], JsonDecoder.iArray[A])

}
