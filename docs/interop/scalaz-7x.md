---
id: scalaz-7x
title: "Scalaz 7.x Interop"
---

## Installation

```scala
libraryDependencies ++= Seq(
  "dev.zio" % "zio-json-interop-scalaz" % "@VERSION@"
)
```

## Usage

```scala mdoc
import zio.json._
import zio.json.interop.scalaz7x._

import scalaz._

IList(1, 2, 3).toJson
```
