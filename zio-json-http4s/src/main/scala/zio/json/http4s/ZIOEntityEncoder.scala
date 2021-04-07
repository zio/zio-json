package zio.json.http4s

import org.http4s.EntityEncoder

import zio.json.JsonEncoder

trait ZIOEntityEncoder {
  implicit def zioEntityEncoder[F[_], A: JsonEncoder]: EntityEncoder[F, A] = jsonEncoderOf[F, A]
}

object ZIOEntityEncoder extends ZIOEntityEncoder
