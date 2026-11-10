---
id: http4s
title: "HTTP4s Interop"
---

An interop module is provided for [HTTP4s v0.23](https://http4s.org/v0.23)

## Installation

```scala
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-json-interop-http4s" % "@VERSION@"
)
```

## Usage

```scala
import cats.Applicative
import org.http4s.EntityEncoder
import zio.json._
import zio.json.interop.http4s._

final case class Greeting(greeting: String)

object Greeting {
  // This JsonCodec[A] provides the JsonEncoder[A] used by the `jsonEncoderOf` method provided by zio-json-interop-http4s
  implicit val codec: JsonCodec[Greeting] =
    DeriveJsonCodec.gen

  implicit def entityEncoder[F[_]: Applicative]: EntityEncoder[F, Greeting] =
    jsonEncoderOf[F, Greeting]
}
```

## Example project

A fully working example project [can be found here](https://github.com/zio/zio-json/tree/develop/examples/interop-http4s)
