package example

import cats.Applicative
import cats.implicits._
import org.http4s.EntityEncoder

import zio.json._
import zio.json.interop.http4s._

trait HelloWorld[F[_]]{
  def hello(n: HelloWorld.Name): F[HelloWorld.Greeting]
}

object HelloWorld {
  implicit def apply[F[_]](implicit ev: HelloWorld[F]): HelloWorld[F] = ev

  final case class Name(name: String)
  final case class Greeting(greeting: String)

  object Greeting {
    implicit val encoder: JsonCodec[Greeting] =
      DeriveJsonCodec.gen

    implicit def entityEncoder[F[_]: Applicative]: EntityEncoder[F, Greeting] =
      jsonEncoderOf[F, Greeting]
  }

  def impl[F[_]: Applicative]: HelloWorld[F] = new HelloWorld[F]{
    def hello(n: HelloWorld.Name): F[HelloWorld.Greeting] =
      Greeting("Hello, " + n.name).pure[F]
  }
}
