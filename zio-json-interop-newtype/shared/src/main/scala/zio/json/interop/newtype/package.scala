package zio.json.interop

import io.estatico.newtype.Coercible
import io.estatico.newtype.ops._
import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

package object newtype {
  implicit def coercibleDecoder[A: Coercible[B, *], B: JsonDecoder]: JsonDecoder[A] =
    JsonDecoder[B].map(_.coerce[A])

  implicit def coercibleEncoder[A: Coercible[*, B], B: JsonEncoder]: JsonEncoder[A] =
    JsonEncoder[B].contramap(_.coerce[B])

  implicit def coercibleKeyDecoder[A: Coercible[B, *], B: JsonFieldDecoder]: JsonFieldDecoder[A] =
    JsonFieldDecoder[B].map(_.coerce[A])

  implicit def coercibleKeyEncoder[A: Coercible[*, B], B: JsonFieldEncoder]: JsonFieldEncoder[A] =
    JsonFieldEncoder[B].contramap(_.coerce[B])
}
