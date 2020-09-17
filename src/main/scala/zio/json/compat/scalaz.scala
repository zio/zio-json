package zio.json.compat

import _root_.scalaz._, Scalaz._
import zio.json

object scalaz {

  implicit def ilistEncoder[A: json.Encoder]: json.Encoder[IList[A]] =
    json.Encoder.list[A].contramap(_.toList)
  implicit def ilistJsonDecoder[A: json.JsonDecoder]: json.JsonDecoder[IList[A]] =
    json.JsonDecoder.list[A].map(IList.fromList(_))

}
