package zio.json.compat

import _root_.scalaz._

import zio.json._

object scalaz {

  implicit def ilistEncoder[A: JsonEncoder]: JsonEncoder[IList[A]] =
    JsonEncoder.list[A].contramap(_.toList)
  implicit def ilistJsonDecoder[A: JsonDecoder]: JsonDecoder[IList[A]] =
    JsonDecoder.list[A].map(IList.fromList(_))

}
