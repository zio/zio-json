package zio.json.interop

import scalaz._

import zio.json._

package object scalaz7x {
  implicit def ilistEncoder[A: JsonEncoder]: JsonEncoder[IList[A]] =
    JsonEncoder.list[A].contramap(_.toList)

  implicit def ilistJsonDecoder[A: JsonDecoder]: JsonDecoder[IList[A]] =
    JsonDecoder.list[A].map(IList.fromList(_))
}
