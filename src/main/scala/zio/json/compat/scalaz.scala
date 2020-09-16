package zio.json.compat

import _root_.scalaz._, Scalaz._
import zio.json

object scalaz {

  implicit def ilistEncoder[A: json.Encoder]: json.Encoder[IList[A]] =
    json.Encoder.list[A].contramap(_.toList)
  implicit def ilistDecoder[A: json.Decoder]: json.Decoder[IList[A]] =
    json.Decoder.list[A].map(IList.fromList(_))

}
