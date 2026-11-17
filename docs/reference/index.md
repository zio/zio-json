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
