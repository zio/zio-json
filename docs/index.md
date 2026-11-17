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

ZIO JSON is published as the core `zio-json` artifact plus add-on modules, all with the same version number. Every module is published for Scala 2.12, 2.13 and 3, except `zio-json-macros` and `zio-json-interop-scalaz7x`, which are published for Scala 2.12 and 2.13 only.

On the JVM, add the core module and any add-on modules you need to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  // Core: codecs, derivation, the Json AST and, on the JVM, streaming (JVM, Scala.js, Scala Native)
  "dev.zio" %% "zio-json"                    % "@VERSION@",
  // Read and write YAML with your JSON codecs, through SnakeYAML (JVM)
  "dev.zio" %% "zio-json-yaml"               % "@VERSION@",
  // The @jsonDerive annotation, which writes codec instances for you (JVM, Scala.js, Scala Native; Scala 2 only)
  "dev.zio" %% "zio-json-macros"             % "@VERSION@",
  // http4s 0.23 entity encoders and decoders built from ZIO JSON codecs (JVM)
  "dev.zio" %% "zio-json-interop-http4s"     % "@VERSION@",
  // Codecs and map-key codecs for refined types (JVM, Scala.js, Scala Native)
  "dev.zio" %% "zio-json-interop-refined"    % "@VERSION@",
  // Codecs for Scalaz 7 IList (JVM, Scala.js, Scala Native; Scala 2 only)
  "dev.zio" %% "zio-json-interop-scalaz7x"   % "@VERSION@",
  // Codecs and map-key codecs for enumeratum enums (JVM, Scala.js, Scala Native)
  "dev.zio" %% "zio-json-interop-enumeratum" % "@VERSION@",
  // Golden (snapshot) tests that catch unintended changes to encoded JSON (JVM, tests only)
  "dev.zio" %% "zio-json-golden"             % "@VERSION@" % Test
)
```

In a Scala.js or Scala Native project, use `%%%`. Only these modules are built for those platforms:

```scala
libraryDependencies ++= Seq(
  // Core: codecs, derivation and the Json AST
  "dev.zio" %%% "zio-json"                    % "@VERSION@",
  // The @jsonDerive annotation (Scala 2 only)
  "dev.zio" %%% "zio-json-macros"             % "@VERSION@",
  // Codecs and map-key codecs for refined types
  "dev.zio" %%% "zio-json-interop-refined"    % "@VERSION@",
  // Codecs for Scalaz 7 IList (Scala 2 only)
  "dev.zio" %%% "zio-json-interop-scalaz7x"   % "@VERSION@",
  // Codecs and map-key codecs for enumeratum enums
  "dev.zio" %%% "zio-json-interop-enumeratum" % "@VERSION@"
)
```

Each module is documented on its own page:

- `zio-json`: [Getting Started](getting-started.md) and the [Reference](reference/index.md)
- `zio-json-yaml`: [YAML](reference/yaml.md)
- `zio-json-macros`: [@jsonDerive Macro Annotation](reference/zio-json-macros.md)
- `zio-json-interop-http4s`: [HTTP4s Interop](interop/http4s.md)
- `zio-json-interop-refined`: [Refined Interop](interop/refined.md)
- `zio-json-interop-scalaz7x`: [Scalaz 7.x Interop](interop/scalaz-7x.md)
- `zio-json-interop-enumeratum`: [Enumeratum Interop](interop/enumeratum.md)
- `zio-json-golden`: [Golden Testing API](reference/golden-testing.md)

## Documentation

The documentation is split by what you want to do:

- **[Getting Started](getting-started.md)** installs ZIO JSON and walks through encoding and decoding your first types.
- **[Concepts](explanation/index.md)** explain the design and the trade-offs: how derivation and decoding work, Scala 2 and Scala 3 differences, decoder strictness, JSON shapes, performance and security.
- **[Reference](reference/index.md)** pages list every type, method, annotation and option.
- **[Integrations](interop/index.md)** connect ZIO JSON to http4s, refined, Scalaz 7 and enumeratum.
- **Tutorials** teach a larger example step by step: [Model a Real API Payload](tutorial/model-an-api-payload.md).
- **[How-to Guides](how-to/index.md)** solve one task each, such as renaming fields, encoding sealed traits or streaming large JSON documents.
