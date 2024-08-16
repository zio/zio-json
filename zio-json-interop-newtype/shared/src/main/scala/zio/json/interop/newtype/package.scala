package zio.json.interop

import io.estatico.newtype.Coercible
import io.estatico.newtype.ops._
import zio.json.{ JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }

package object newtype {
  implicit def coercibleDecoder[B: JsonDecoder, A](implicit coerce: Coercible[B, A]): JsonDecoder[A] =
    JsonDecoder[B].map(_.coerce[A])

  implicit def coercibleEncoder[B: JsonEncoder, A](implicit coerce: Coercible[A, B]): JsonEncoder[A] =
    JsonEncoder[B].contramap(_.coerce[B])

  implicit def coercibleKeyDecoder[B: JsonFieldDecoder, A](implicit coerce: Coercible[B, A]): JsonFieldDecoder[A] =
    JsonFieldDecoder[B].map(_.coerce[A])

  implicit def coercibleKeyEncoder[B: JsonFieldEncoder, A](implicit coerce: Coercible[A, B]): JsonFieldEncoder[A] =
    JsonFieldEncoder[B].contramap(_.coerce[B])
}
