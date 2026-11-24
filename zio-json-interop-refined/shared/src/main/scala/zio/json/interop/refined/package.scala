package zio.json.interop

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.refineV
import zio.json._

package object refined {

  /**
   * Encodes a refined value as its underlying value: the JSON text is the same as for the plain `A`, and no trace of
   * the `Refined` wrapper or its predicate appears in it.
   *
   * {{{
   * import eu.timepit.refined.api.Refined
   * import eu.timepit.refined.numeric.Positive
   * import eu.timepit.refined.refineV
   * import zio.json._
   * import zio.json.interop.refined._
   *
   * val age: Either[String, Int Refined Positive] = refineV[Positive](30)
   * val json: Either[String, String]              = age.map(_.toJson)
   * // Right(30)
   * }}}
   */
  implicit def encodeRefined[A: JsonEncoder, B]: JsonEncoder[A Refined B] =
    JsonEncoder[A].contramap(_.value)

  /**
   * Decodes the underlying `A` and then checks it against the predicate `P`. A value that fails the check fails the
   * decoding with the message of the `refined` library, for example `Predicate failed: (-3 > 0).` for `-3` as an
   * `Int Refined Positive`.
   *
   * {{{
   * import eu.timepit.refined.api.Refined
   * import eu.timepit.refined.numeric.Positive
   * import zio.json._
   * import zio.json.interop.refined._
   *
   * val ok  = "5".fromJson[Int Refined Positive]
   * // Right(5)
   * val bad = "-3".fromJson[Int Refined Positive]
   * // Left((Predicate failed: (-3 > 0).))
   * }}}
   */
  implicit def decodeRefined[A: JsonDecoder, P](implicit V: Validate[A, P]): JsonDecoder[A Refined P] =
    JsonDecoder[A].mapOrFail(refineV[P](_))

  /** Encodes a refined value as a JSON object key, using the field encoder of the underlying `A`. */
  implicit def encodeFieldRefined[A: JsonFieldEncoder, B]: JsonFieldEncoder[A Refined B] =
    JsonFieldEncoder[A].contramap(_.value)

  /**
   * Decodes a JSON object key with the field decoder of the underlying `A` and then checks it against the predicate
   * `P`. A key that fails the check fails the decoding with the message of the `refined` library.
   */
  implicit def decodeFieldRefined[A: JsonFieldDecoder, P](implicit V: Validate[A, P]): JsonFieldDecoder[A Refined P] =
    JsonFieldDecoder[A].mapOrFail(refineV[P](_))
}
