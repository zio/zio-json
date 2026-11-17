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

- [Derivation](derivation/index.md): how codecs are derived, and the three ways to change what derivation produces.
- [Annotations](annotations.md): every annotation used to change derived codecs.
- [@jsonDerive Macro Annotation](zio-json-macros.md): the Scala 2 macro annotation that writes codecs into companion objects.

## Data and modules

The [Modules](../index.md#modules) table on the Introduction page lists every published artifact with its platforms and Scala versions.


- [JSON AST](json-ast.md): the `Json` value type, cursors and tree transforms.
- [YAML](yaml.md): the optional YAML module.
- [Streaming API](streaming.md): the JVM-only ZIO Streams encoders, decoders, pipelines, file helpers and their error behaviour.
- [Golden Testing API](golden-testing.md): `goldenTest`, `GoldenConfiguration`, the golden file lifecycle and format, and module root lookup.

## Concepts

These pages explain how ZIO JSON works and why it behaves the way it does:

- [How ZIO JSON Works](../explanation/how-it-works.md): the design that makes it fast and safe, and how derivation finds instances.
- [Scala 2 and Scala 3 Differences](../explanation/scala-2-vs-scala-3.md): every behaviour that differs between the two versions, and a migration checklist.
- [How Strict Is the Decoder](../explanation/decoder-strictness.md): what the decoders accept beyond strict JSON, and how to add stricter checks.
- [JSON Shapes and Round Trips](../explanation/wire-format.md): how each kind of sum-type case is written, and which values do not survive an encode then decode.
- [Performance](../explanation/performance.md): benchmarks and tips for fast encoding and decoding.
- [Security](../explanation/security.md): how ZIO JSON handles hostile input, and what you still need to limit yourself.
