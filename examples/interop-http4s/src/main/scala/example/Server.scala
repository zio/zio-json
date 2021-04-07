package example

import cats.effect.{ConcurrentEffect, Timer}

import fs2.Stream

import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext.global

object Server {
  def stream[F[_]: ConcurrentEffect](implicit T: Timer[F]): Stream[F, Nothing] = {
    val httpApp =
      HelloWorldRoutes.routes[F](HelloWorld.impl[F]).orNotFound

    BlazeServerBuilder[F](global)
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(httpApp)
      .serve
  }.drain
}
