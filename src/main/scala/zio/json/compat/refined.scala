package zio.json.compat

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.{ refineV }

import zio.json._

object refined {
  implicit def encodeRefined[A: JsonEncoder, B]: JsonEncoder[A Refined B] =
    JsonEncoder[A].contramap(_.value)

  implicit def decodeRefined[A: JsonDecoder, P](implicit V: Validate[A, P]): JsonDecoder[A Refined P] =
    JsonDecoder[A].mapOrFail(refineV[P](_))

}
