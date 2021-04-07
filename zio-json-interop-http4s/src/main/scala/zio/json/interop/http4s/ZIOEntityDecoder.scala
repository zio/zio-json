package zio.json.interop.http4s

import cats.effect.Concurrent
import org.http4s.EntityDecoder

import zio.json.JsonDecoder

trait ZIOEntityDecoder {
  implicit def zioEntityDecoder[F[_]: Concurrent, A: JsonDecoder]: EntityDecoder[F, A] =
    jsonOf[F, A]
}

object ZIOEntityDecoder extends ZIOEntityDecoder
