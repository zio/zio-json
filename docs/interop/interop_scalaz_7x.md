---
id: interop_scalaz_7x
title: "Scalaz 7.x Interop"
---

## Installation

```scala
libraryDependencies ++= Seq(
  "@ORG@" % "zio-json-interop-scalaz" % "@RELEASE_VERSION@"
)
```

## Usage

```scala mdoc
import zio.json._
import zio.json.interop.scalaz7x._

import scalaz._

IList(1, 2, 3).toJson
```
