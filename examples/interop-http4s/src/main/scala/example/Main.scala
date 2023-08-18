package example

import cats.effect.{ ExitCode, IO, IOApp }

object Main extends IOApp {
  def run(args: List[String]) =
    Server.stream[IO].compile.drain.as(ExitCode.Success)
}
