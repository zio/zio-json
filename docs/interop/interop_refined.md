---
id: interop_refined
title: "Refined Interop"
---

## Installation

```scala
libraryDependencies ++= Seq(
  "@ORG@" % "zio-json-interop-refined" % "@RELEASE_VERSION@"
)
```

## Usage

```scala mdoc
import zio.json._
import zio.json.interop.refined._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

case class Person(name: String Refined NonEmpty)

object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen
}
```

```scala mdoc
"""{ "name": "" }""".fromJson[Person]
```

```scala mdoc
"""{ "name": "Aurora" }""".fromJson[Person]
```
