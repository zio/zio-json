package zio.json.interop.http4s

import cats.effect.Concurrent
import org.http4s.headers.`Content-Type`
import org.http4s.{ EntityDecoder, EntityEncoder, MalformedMessageBodyFailure, MediaType }
import zio.json._

import java.nio.charset.StandardCharsets

/**
 * Builders for the http4s entity decoder and encoder of a type that has a `zio.json.JsonDecoder` or a
 * `zio.json.JsonEncoder`. The package object of `zio.json.interop.http4s` extends this trait.
 */
trait ZIOJsonInstances {

  /**
   * Returns an `EntityDecoder[F, A]` for the media type `application/json`. It decodes the body bytes as UTF-8 (whatever
   * charset the request declares) and parses them with the `JsonDecoder` of `A`.
   *
   * Decoding fails with a `MalformedMessageBodyFailure`: its details are `Invalid JSON: empty body` for an empty body
   * and the zio-json error text for a body that is not valid JSON for `A`. With `strict = true`, a message whose
   * `Content-Type` is not `application/json` fails with a `MediaTypeMismatch` instead, and a message with no `Content-Type`
   * fails with a `MediaTypeMissing`.
   *
   * {{{
   * import cats.effect.IO
   * import org.http4s.{ DecodeResult, Request }
   * import zio.json._
   *
   * case class User(name: String, age: Int)
   * object User {
   *   implicit val decoder: JsonDecoder[User] = DeriveJsonDecoder.gen[User]
   * }
   *
   * val request: Request[IO]           = Request[IO]().withEntity("{\"name\":\"Ann\",\"age\":30}")
   * val result: DecodeResult[IO, User] = jsonOf[IO, User].decode(request, strict = false)
   * // running `result.value` gives Right(User(Ann,30))
   * }}}
   */
  def jsonOf[F[_]: Concurrent, A: JsonDecoder]: EntityDecoder[F, A] =
    EntityDecoder.decodeBy[F, A](MediaType.application.json) { m =>
      EntityDecoder.collectBinary(m).subflatMap { chunk =>
        val str = new String(chunk.toArray, StandardCharsets.UTF_8)
        if (str.nonEmpty)
          str.fromJson.fold(e => Left(MalformedMessageBodyFailure(e, None)), Right(_))
        else
          Left(MalformedMessageBodyFailure("Invalid JSON: empty body"))
      }
    }

  /**
   * Returns an `EntityEncoder[F, A]` that writes the value as JSON text (UTF-8) with the `Content-Type`
   * `application/json`.
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
   * val response: Response[IO] = Response[IO]().withEntity(User("Bob", 41))(jsonEncoderOf[IO, User])
   * // the body is {"name":"Bob","age":41} and the Content-Type is application/json
   * }}}
   */
  def jsonEncoderOf[F[_], A: JsonEncoder]: EntityEncoder[F, A] = EntityEncoder
    .stringEncoder[F]
    .contramap[A](_.toJson)
    .withContentType(`Content-Type`(MediaType.application.json))
}
