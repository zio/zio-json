---
id: index
title: "ZIO JSON"
sidebar_label: "ZIO JSON"
---

[ZIO Json](https://github.com/zio/zio-json) is a fast and secure JSON library with tight ZIO integration.

@PROJECT_BADGES@

## Introduction

The goal of this project is to create the best all-round JSON library for Scala:

- **Performance** to handle more requests per second than the incumbents, i.e. reduced operational costs.
- **Security** to mitigate against adversarial JSON payloads that threaten the capacity of the server.
- **Fast Compilation** no shapeless, no type astronautics.
- **Future-Proof**, prepared for Scala 3 and next-generation Java.
- **Simple** small codebase, concise documentation that covers everything.
- **Helpful errors** are readable by humans and machines.
- **ZIO Integration** so nothing more is required.

## Installation

In order to use this library, we need to add the following line in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

For cross-platform projects with Scala.js and Scala Native need to replace `%%` operator by `%%%`, 
and optionally when using `java.time.ZoneId` and `java.time.ZonedDateTime` types need to add 
the dependency on the latest version of Timezone DB:

```scala
libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "latest.integration"
```

Continue with the [Getting Started](getting-started.mdx) guide for a walkthrough of encoding and decoding JSON with ZIO JSON.
