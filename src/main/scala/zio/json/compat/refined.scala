package zio.json.compat

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.{ refineV }

import zio.json

object refined {
  implicit def encodeRefined[A: json.JsonEncoder, B]: json.JsonEncoder[A Refined B] =
    json.JsonEncoder[A].contramap(_.value)

  implicit def decodeRefined[A: json.JsonDecoder, P](implicit V: Validate[A, P]): json.JsonDecoder[A Refined P] =
    json.JsonDecoder[A].mapOrFail(refineV[P](_))

}
