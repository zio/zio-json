package zio.json.compat

import _root_.scalaz._, Scalaz._
import zio.json

object scalaz {

  implicit def ilistEncoder[A: json.JsonEncoder]: json.JsonEncoder[IList[A]] =
    json.JsonEncoder.list[A].contramap(_.toList)
  implicit def ilistJsonDecoder[A: json.JsonDecoder]: json.JsonDecoder[IList[A]] =
    json.JsonDecoder.list[A].map(IList.fromList(_))

}
