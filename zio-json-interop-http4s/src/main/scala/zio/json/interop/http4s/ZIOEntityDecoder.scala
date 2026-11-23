package zio.json.interop.http4s

import cats.effect.Concurrent
import org.http4s.EntityDecoder
import zio.json.JsonDecoder

/**
 * Provides an implicit http4s `EntityDecoder[F, A]` for every `A` that has a `zio.json.JsonDecoder`. Use it through the
 * companion (`import ZIOEntityDecoder._`) or by mixing the trait into your own object.
 *
 * {{{
 * import cats.effect.IO
 * import org.http4s.Request
 * import zio.json._
 *
 * case class User(name: String, age: Int)
 * object User {
 *   implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen[User]
 * }
 *
 * import ZIOEntityDecoder._
 *
 * val request: Request[IO] = Request[IO]().withEntity("{\"name\":\"Ann\",\"age\":30}")
 * val user: IO[User]       = request.as[User]
 * // running `user` gives User(Ann,30)
 * }}}
 */
trait ZIOEntityDecoder {

  /**
   * Returns an `EntityDecoder[F, A]` for the media type `application/json`; it is the decoder that `jsonOf[F, A]`
   * builds.
   */
  implicit def zioEntityDecoder[F[_]: Concurrent, A: JsonDecoder]: EntityDecoder[F, A] =
    jsonOf[F, A]
}

/** The instance as an object, ready to import. */
object ZIOEntityDecoder extends ZIOEntityDecoder
