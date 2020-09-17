package zio.json.compat

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.{ refineV }

import zio.json

object refined {
  implicit def encodeRefined[A: json.Encoder, B]: json.Encoder[A Refined B] =
    json.Encoder[A].contramap(_.value)

  implicit def decodeRefined[A: json.Decoder, P](implicit V: Validate[A, P]): json.Decoder[A Refined P] =
    json.Decoder[A].mapOrFail(refineV[P](_))

}
