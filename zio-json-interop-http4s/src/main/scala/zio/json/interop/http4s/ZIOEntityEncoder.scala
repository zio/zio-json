package zio.json.interop.http4s

import org.http4s.EntityEncoder
import zio.json.JsonEncoder

/**
 * Provides an implicit http4s `EntityEncoder[F, A]` for every `A` that has a `zio.json.JsonEncoder`. Use it through
 * the companion (`import ZIOEntityEncoder._`) or by mixing the trait into your own object.
 *
 * {{{
 * import cats.effect.IO
 * import org.http4s.Response
 * import zio.json._
 *
 * case class User(name: String, age: Int)
 * object User {
 *   implicit val encoder: JsonEncoder[User] = DeriveJsonEncoder.gen[User]
 * }
 *
 * import ZIOEntityEncoder._
 *
 * val response: Response[IO] = Response[IO]().withEntity(User("Bob", 41))
 * // the body is {"name":"Bob","age":41} and the Content-Type is application/json
 * }}}
 */
trait ZIOEntityEncoder {
  /**
   * Returns an `EntityEncoder[F, A]` that writes the value as JSON text with the `Content-Type` `application/json`; it
   * is the encoder that `jsonEncoderOf[F, A]` builds.
   */
  implicit def zioEntityEncoder[F[_], A: JsonEncoder]: EntityEncoder[F, A] = jsonEncoderOf[F, A]
}

/** The instance as an object, ready to import. */
object ZIOEntityEncoder extends ZIOEntityEncoder
