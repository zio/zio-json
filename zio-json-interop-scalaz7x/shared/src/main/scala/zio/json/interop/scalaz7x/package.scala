package zio.json.interop

import scalaz._
import zio.json._

package object scalaz7x {

  /**
   * Encodes a Scalaz `IList` as a JSON array, in the same way as a `List` (`IList(1, 2, 3)` gives `[1,2,3]`, the empty
   * list gives `[]`).
   */
  implicit def ilistEncoder[A: JsonEncoder]: JsonEncoder[IList[A]] =
    JsonEncoder.list[A].contramap(_.toList)

  /**
   * Decodes a JSON array into a Scalaz `IList`, using the decoder of `A` for each element (`[1,2,3]` gives
   * `IList(1, 2, 3)`, `[]` gives the empty list).
   */
  implicit def ilistJsonDecoder[A: JsonDecoder]: JsonDecoder[IList[A]] =
    JsonDecoder.list[A].map(IList.fromList(_))
}
