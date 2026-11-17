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

## Modules

ZIO JSON is published as the core `zio-json` artifact plus add-on modules. Each one is a separate dependency with the same version number, and not every module is available on every platform or Scala version:

| Artifact                      | What it is for                                                        | Platforms             | Scala versions | Documentation                                   |
| ----------------------------- | --------------------------------------------------------------------- | --------------------- | -------------- | ----------------------------------------------- |
| `zio-json`                    | Codecs, derivation, the `Json` AST and, on the JVM, streaming         | JVM, Scala.js, Native | 2.12, 2.13, 3  | [Getting Started](getting-started.md) |
| `zio-json-yaml`               | Reading and writing YAML with your JSON codecs (through SnakeYAML)    | JVM                   | 2.12, 2.13, 3  | [YAML](reference/yaml.md)                       |
| `zio-json-golden`             | Golden (snapshot) tests that catch unintended changes to encoded JSON | JVM                   | 2.12, 2.13, 3  | [Golden Testing API](reference/golden-testing.md)          |
| `zio-json-macros`             | The `@jsonDerive` annotation, which writes codec instances for you    | JVM, Scala.js, Native | 2.12, 2.13     | [@jsonDerive Macro Annotation](reference/zio-json-macros.md) |
| `zio-json-interop-http4s`     | http4s 0.23 entity encoders and decoders built from ZIO JSON codecs   | JVM                   | 2.12, 2.13, 3  | [HTTP4s Interop](interop/http4s.md)             |
| `zio-json-interop-refined`    | Codecs and map-key codecs for refined types                           | JVM, Scala.js, Native | 2.12, 2.13, 3  | [Refined Interop](interop/refined.md)           |
| `zio-json-interop-scalaz7x`   | Codecs for Scalaz 7 `IList`                                           | JVM, Scala.js, Native | 2.12, 2.13     | [Scalaz 7.x Interop](interop/scalaz-7x.md)      |
| `zio-json-interop-enumeratum` | Codecs and map-key codecs for enumeratum enums                        | JVM, Scala.js, Native | 2.12, 2.13, 3  | [Enumeratum Interop](interop/enumeratum.md)     |

Add a module the same way as the core artifact, using `%%%` for a cross-built module in a Scala.js or Scala Native project. Golden tests only run in tests, so that module usually goes in the `Test` configuration:

```scala
libraryDependencies += "dev.zio" %% "zio-json-golden" % "@VERSION@" % Test
```

## Documentation

The documentation is split by what you want to do:

- **[Getting Started](getting-started.md)** installs ZIO JSON and walks through encoding and decoding your first types.
- **[Reference](reference/index.md)** pages list every type, method, annotation and option. Its **Concepts** pages describe the design: [How ZIO JSON Works](explanation/how-it-works.md), [Scala 2 and Scala 3 Differences](explanation/scala-2-vs-scala-3.md), [How Strict Is the Decoder](explanation/decoder-strictness.md), [JSON Shapes and Round Trips](explanation/wire-format.md), [Performance](explanation/performance.md) and [Security](explanation/security.md).
- **[Integrations](interop/index.md)** connect ZIO JSON to http4s, refined, Scalaz 7 and enumeratum.
- **Tutorials** teach a larger example step by step: [Model a Real API Payload](tutorial/model-an-api-payload.md).
- **[How-to Guides](how-to/index.md)** solve one task each, such as renaming fields, encoding sealed traits or streaming large JSON documents.
