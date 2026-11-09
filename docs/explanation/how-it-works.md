---
id: how-it-works
title: "How ZIO JSON works"
description: "Why ZIO JSON is fast and safe: a reader-based decoder, compile-time derivation without runtime reflection, and early exit on bad input."
keywords:
  - "JSON Decoder Design"
  - "Compile-Time Derivation"
  - "Early Exit Parsing"
  - "ZIO JSON"
---

This page explains the ideas behind ZIO JSON. You do not need any of it to use the library. It helps when you wonder why the API looks the way it does, or why a decode failed the way it did.

## Decoding reads straight into your types

Most JSON libraries parse the whole text into a tree of JSON nodes first, then convert the tree into your types. ZIO JSON skips the tree for normal decoding. A decoder reads characters from the input and builds your values as it goes.

The input is wrapped in a small reader. The reader hands out one character at a time and can step back by one character (`retract`). Stepping back matters for numbers: a number has no end marker, so the decoder only learns it has finished when it reads the next character. It then pushes that character back for the next step to see.

Several readers exist for different inputs:

- `FastStringReader` for a `String` or other `CharSequence`.
- `Utf8ChunkReader` for UTF-8 bytes in a `Chunk[Byte]`, read without first copying them into a `String`.
- `WithRetractReader` for any `java.io.Reader`, which is how streaming input is handled.

All of them are internal and live in `zio.json.internal`. You reach them through `fromJson` and the decoding methods on `JsonDecoder`.

The `Json` tree type does exist, and you can decode to it. See the [JSON AST reference](../reference/json-ast.md). It is a choice you make, not a step the library forces on you.

## No reflection at runtime

A `JsonEncoder` or `JsonDecoder` for a case class or sealed trait is plain code. That code is written for you at compile time, when you call `DeriveJsonCodec.gen` (or write `derives` on Scala 3).

- On Scala 2, `DeriveJsonEncoder.gen`, `DeriveJsonDecoder.gen` and `DeriveJsonCodec.gen` are macros. The encoder and decoder derivation is built on the Magnolia library, which reads the shape of your type during compilation. The codec derivation is a macro that builds on the other two.
- On Scala 3, derivation uses native Scala 3 macros together with the compiler-provided `Mirror` of your type.

Neither route uses Shapeless. After compilation, the derived instance does not inspect classes, field names, or annotations by reflection. It reads the fields it was generated to read.

This has two visible effects:

1. Missing codecs are compile errors, not runtime surprises.
2. Derivation is done once when your code is compiled, not on every call.

## Failing early

A decoder reports a problem by throwing a small exception that has no stack trace. Building a stack trace is slow, and a flood of bad requests would make a server pay that cost again and again. The exception carries a short list of path steps instead, such as `.address.city`, followed by a message.

`fromJson` and `decodeJson` catch that exception at the top and turn it into a `Left` with the rendered path. That is why every error looks like `.address.city(...)` and why you never see an exception from decoding.

Decoding stops at the first problem. It does not collect a list of errors, and it does not read the rest of the payload. This is the *early exit* strategy: a malformed or hostile payload is rejected as soon as it is known to be bad.

## Performance and security goals

Reading directly into your types allocates less and does less work than building a tree first. Early exit keeps a bad payload from costing more than the part that was read. The two ideas support each other.

Measurements and attack scenarios are covered in their own pages:

- [Performance](./performance.md) has the benchmarks.
- [Security](./security.md) walks through denial-of-service attacks and how the library resists them.

## Helpful errors and simple patterns

The error path follows the `jq` query style, so you can paste a path into a JSON tool to find the spot. The design relies on the well-known typeclass pattern, where `JsonEncoder[A]` and `JsonDecoder[A]` are values you pass implicitly. To define your own, see [Manual instances](../how-to/manual-instances.md).
