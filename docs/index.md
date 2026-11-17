---
id: index
title: "Introduction to ZIO JSON"
sidebar_label: "Introduction"
---

[ZIO JSON](https://github.com/zio/zio-json) is a fast and secure JSON library with tight ZIO integration.

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

It is published for the JVM, Scala.js and Scala Native, for Scala 2.12, 2.13 and 3. [Getting Started](getting-started.md) covers the platforms, the add-on modules and a first program.

## Documentation

The documentation is split by what you want to do:

- **[Getting Started](getting-started.md)** installs ZIO JSON and walks through encoding and decoding your first types.
- **[Reference](reference/index.md)** pages list every type, method, annotation and option. Its **Concepts** pages describe the design: [How ZIO JSON Works](explanation/how-it-works.md), [Scala 2 and Scala 3 Differences](explanation/scala-2-vs-scala-3.md), [How Strict Is the Decoder](explanation/decoder-strictness.md), [JSON Shapes and Round Trips](explanation/wire-format.md), [Performance](explanation/performance.md) and [Security](explanation/security.md).
- **[Integrations](interop/index.md)** connect ZIO JSON to http4s, refined, Scalaz 7 and enumeratum.
- **Tutorials** teach a larger example step by step: [Model a Real API Payload](tutorial/model-an-api-payload.md).
- **[How-to Guides](how-to/index.md)** solve one task each, such as renaming fields, encoding sealed traits or streaming large JSON documents.
