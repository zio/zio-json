---
id: index
title: "Reference"
---

Reference pages list every type, method, annotation and option in ZIO JSON. Use them when you know what you want to call and need the exact signature or default. For step-by-step guides, see the [How-to guides](../how-to/index.md).

## Core types

- [JsonEncoder](json-encoder.md): turns a Scala value into JSON.
- [JsonDecoder](json-decoder.md): reads JSON into a Scala value.
- [JsonCodec](json-codec.md): an encoder and a decoder packed together.
- [JsonFieldEncoder and JsonFieldDecoder](json-field-codecs.md): the map-key type classes, the built-in key types, and how to use your own type as a `Map` key.
- [Built-in Instances](built-in-instances.md): every type ZIO JSON encodes and decodes out of the box, with its JSON shape and decoder leniencies.
- [JsonCodecConfiguration](json-codec-configuration.md): options that change how derived codecs behave.
- [Errors](errors.md): the error values and the error message format.

## Derivation

- [Annotations](annotations.md): every annotation used to change derived codecs.
- [zio-json-macros (@jsonDerive)](zio-json-macros.md): the Scala 2 macro annotation that writes codecs into companion objects.

## Data and modules

- [JSON AST](json-ast.md): the `Json` value type, cursors and tree transforms.
- [YAML](yaml.md): the optional YAML module.

## Modules

Besides the core `zio-json` artifact, the project publishes add-on modules. Each one is a separate dependency with the same version number, and not every module is available on every platform or Scala version:

| Artifact                      | What it is for                                                        | Platforms             | Scala versions | Documentation                                   |
| ----------------------------- | --------------------------------------------------------------------- | --------------------- | -------------- | ----------------------------------------------- |
| `zio-json`                    | Codecs, derivation, the `Json` AST and, on the JVM, streaming         | JVM, Scala.js, Native | 2.12, 2.13, 3  | [Getting Started](../getting-started.md) |
| `zio-json-yaml`               | Reading and writing YAML with your JSON codecs (through SnakeYAML)    | JVM                   | 2.12, 2.13, 3  | [YAML](yaml.md)                       |
| `zio-json-golden`             | Golden (snapshot) tests that catch unintended changes to encoded JSON | JVM                   | 2.12, 2.13, 3  | [Golden Testing](../how-to/golden-testing.md)      |
| `zio-json-macros`             | The `@jsonDerive` annotation, which writes codec instances for you    | JVM, Scala.js, Native | 2.12, 2.13     | [zio-json-macros](zio-json-macros.md) |
| `zio-json-interop-http4s`     | http4s 0.23 entity encoders and decoders built from ZIO JSON codecs   | JVM                   | 2.12, 2.13, 3  | [HTTP4s Interop](../interop/http4s.md)             |
| `zio-json-interop-refined`    | Codecs and map-key codecs for refined types                           | JVM, Scala.js, Native | 2.12, 2.13, 3  | [Refined Interop](../interop/refined.md)           |
| `zio-json-interop-scalaz7x`   | Codecs for Scalaz 7 `IList`                                           | JVM, Scala.js, Native | 2.12, 2.13     | [Scalaz 7.x Interop](../interop/scalaz-7x.md)      |
| `zio-json-interop-enumeratum` | Codecs and map-key codecs for enumeratum enums                        | JVM, Scala.js, Native | 2.12, 2.13, 3  | [Enumeratum Interop](../interop/enumeratum.md)     |

Add a module the same way as the core artifact, using `%%%` for a cross-built module in a Scala.js or Scala Native project. Golden tests only run in tests, so that module usually goes in the `Test` configuration:

```scala
libraryDependencies += "dev.zio" %% "zio-json-golden" % "@VERSION@" % Test
```

## Concepts

These pages explain how ZIO JSON works and why it behaves the way it does:

- [How ZIO JSON Works](../explanation/how-it-works.md): the design that makes it fast and safe, and how derivation finds instances.
- [Scala 2 and Scala 3 Differences](../explanation/scala-2-vs-scala-3.md): every behaviour that differs between the two versions, and a migration checklist.
- [How Strict Is the Decoder](../explanation/decoder-strictness.md): what the decoders accept beyond strict JSON, and how to add stricter checks.
- [JSON Shapes and Round Trips](../explanation/wire-format.md): how each kind of sum-type case is written, and which values do not survive an encode then decode.
- [Performance](../explanation/performance.md): benchmarks and tips for fast encoding and decoding.
- [Security](../explanation/security.md): how ZIO JSON handles hostile input, and what you still need to limit yourself.
