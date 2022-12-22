---
id: http4s
title: "HTTP4s Interop"
---

An interop module is provided for [HTTP4s v0.21](https://http4s.org/v0.21)

## Installation

```scala
libraryDependencies ++= Seq(
  "dev.zio" % "zio-json-interop-http4s" % "@RELEASE_VERSION@"
)
```

## Usage

```scala
import zio.json._
import zio.json.interop.http4s._

final case class Greeting(greeting: String)

object Greeting {
  // This JsonCodec[A] will be picked up by the `jsonEncoderOf` method provided by zio-json-interop-http4s
  implicit val encoder: JsonCodec[Greeting] =
    DeriveJsonCodec.gen

  implicit def entityEncoder[F[_]: Applicative]: EntityEncoder[F, Greeting] =
    jsonEncoderOf[F, Greeting]
}
```

## Example project

A fully working example project [can be found here](https://github.com/zio/zio-json/tree/develop/examples/interop-http4s)
