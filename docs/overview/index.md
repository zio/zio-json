---
id: overview_index
title: "Summary"
---

ZIO JSON

Say we want to be able to read some JSON like

```json
{ "curvature": 0.5 }
```

into a Scala `case class`

```scala mdoc
case class Banana(curvature: Double)
```

To do this, we create an *instance* of the `JsonDecoder` typeclass for `Banana` using the `zio-json` code generator. It is best practice to put it on the companion of `Banana`, like so

```scala mdoc
import zio.json._

object Banana {
  implicit val decoder: JsonDecoder[Banana] =
    DeriveJsonDecoder.gen[Banana]
}
```

Now we can parse JSON into our object

```scala mdoc
"""{ "curvature": 0.5 }""".fromJson[Banana]
```

Likewise, to produce JSON from our data we define a `JsonEncoder`

```scala mdoc:reset
import zio.json._
case class Banana(curvature: Double)
```

```scala mdoc
object Banana {
  implicit val encoder: JsonEncoder[Banana] =
    DeriveJsonEncoder.gen[Banana]
}

Banana(0.5).toJson
```

## Installation

`zio-json` is in beta and available as snapshot release:

```scala
resolvers += Resolver.sonatypeRepo("snapshots")

// Latest snapshot version is shown in badge here: https://github.com/zio/zio-json/#zio-json
libraryDependencies += "dev.zio" %% "zio-json" % "0.0.0+28-e548a5ac-SNAPSHOT"

scalaVersion in ThisBuild := "2.13.3"
```
