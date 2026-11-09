---
id: errors
title: "Errors"
description: "Reference for zio-json decoding errors: JsonError variants, the path render format, common messages and JVM streaming behavior."
keywords:
  - "Decoding Errors"
  - "Error Path"
  - "Error Messages"
  - "JsonError"
---

A failed decode returns a `String` that says what went wrong and where.
zio-json builds it from a list of `JsonError` values called the trace.
This page lists the error types, the string format, and the messages you will meet most often.

## JsonError

`JsonError` is a sealed class with four variants.
Three of them are steps on the path to the failure, and one is the failure itself.

| Variant               | Rendered as | Meaning                   |
|-----------------------|-------------|---------------------------|
| `ObjectAccess(field)` | `.field`    | Went into an object key   |
| `ArrayAccess(i)`      | `[i]`       | Went into an array index  |
| `SumType(cons)`       | `{cons}`    | Went into a sum type case |
| `Message(txt)`        | `(txt)`     | The failure itself        |

`JsonError.render(trace: List[JsonError]): String` joins the pieces.
The decoder keeps the trace with the innermost step first, and `render` reverses it, so the string reads from the outside in.
The message comes last.

```scala mdoc
import zio.json._

JsonError.render(
  List(JsonError.Message("missing"), JsonError.ArrayAccess(1), JsonError.ObjectAccess("items"))
)
```

No decoder in the library adds a `SumType` step today.
Errors inside a sum type case show the case name as an ordinary `.Name` object key, because the default encoding wraps the case in an object.

## Where errors appear

`decodeJson`, `fromJson` and `fromJsonAST` return `Either[String, A]`.
The string is the rendered trace.
Two failures are not part of the trace and return a fixed string.

- `Unexpected end of input` when the input stops in the middle of a value.
- `Unexpected structure` when the decoder runs out of stack on deeply nested input.

Code that implements `unsafeDecode` by hand reports an error with `Lexer.error(msg, trace)`.
It throws `JsonDecoder.UnsafeJson(trace)`, and the public methods catch it and render it.
Application code should not call `unsafeDecode` directly.

### Streaming on the JVM

The streaming decoders live in the JVM module only.
They fail the `ZIO` or `ZStream` with a plain `java.lang.Exception` instead of returning a `Left`.
The exception message is the rendered trace, so it looks the same as the string from `decodeJson`.

- `decodeJsonStream` and `decodeJsonStreamInput` fail with `new Exception(JsonError.render(trace))`.
- `decodeJsonPipeline` does the same for each element it fails on.
- When `decodeJsonStream` or `decodeJsonStreamInput` hits the end of the input, the message is `unexpected end of input` in lower case, unlike `Unexpected end of input` from `decodeJson`.

See [streaming](../how-to/streaming.md) for how to decode streams.

## Common messages

The table lists the messages in the order a decoder tends to meet them.
Each one is shown in the parentheses of a full error, for example `.age(expected an Int)`.
When the failure is at the top level, the path is empty and only the parentheses remain.

| Message                        | Produced when                                                                     |
|--------------------------------|-----------------------------------------------------------------------------------|
| `missing`                      | A required field is absent. Appears as `.field(missing)`.                         |
| `duplicate`                    | The same key appears twice in an object.                                          |
| `invalid extra field`          | An unknown key arrives and extra fields are not allowed.                          |
| `invalid disambiguator`        | The sum type tag names no known case.                                             |
| `invalid enumeration value`    | A case object string names no known case.                                         |
| `expected non-empty object`    | A sum type is read from `{}`.                                                     |
| `expected single field object` | Same sum type read from a `Json` AST that is not a one-key object.                |
| `expected an Int`              | A number or string is not an `Int`. Same shape for `Long`, `Double`, and so on.   |
| `expected a Boolean`           | The value is not `true` or `false`.                                               |
| `expected string`              | A string was needed and something else arrived.                                   |
| `expected a UUID`              | A string is not a valid `UUID`. Same shape for `Instant`, `LocalDate`, and so on. |
| `expected '{' got '['`         | The wrong opening character. The `got` part is the character read.                |
| `expected ',' or '}' got '"'`  | Two members are not separated by a comma.                                         |
| `expected ',' or ']' got '2'`  | Two array elements are not separated by a comma.                                  |
| `invalid control in string`    | A raw control character such as a newline sits inside a string.                   |
| `invalid '\q' in string`       | A string has an unknown escape sequence.                                          |
| `expected one of: A, B`        | A Scala 3 union of string literals gets any other string.                         |
| `Unexpected end of input`      | The input ends in the middle of a value.                                          |
| `Unexpected structure`         | Nesting is so deep that the decoder overflows the stack.                          |

`missing`, `duplicate`, `invalid extra field` and the enumeration messages come from derived codecs.
The `expected ...` messages come from the lexer and the built-in decoders.
A message you return from `mapOrFail` ends up in the same position, in parentheses.

## Why the path matters

A message alone is rarely enough.
`expected an Int` does not say which of a hundred fields was wrong.
The path points at the exact value, so you can find it in the input without a debugger.

```scala mdoc:silent:reset
import zio.json._

case class Item(name: String, qty: Int)
case class Order(id: Int, items: List[Item])

object Item {
  implicit val codec: JsonCodec[Item] = DeriveJsonCodec.gen[Item]
}

object Order {
  implicit val codec: JsonCodec[Order] = DeriveJsonCodec.gen[Order]
}
```

The second item has a string where a number belongs.

```scala mdoc
"""{"id":1,"items":[{"name":"a","qty":1},{"name":"b","qty":"two"}]}""".fromJson[Order]
```

Read `.items[1].qty(expected an Int)` from left to right: the `items` field, its element at index 1, the `qty` field, and then the failure.

A missing field is reported the same way.

```scala mdoc
"""{"id":1,"items":[{"name":"a"}]}""".fromJson[Order]
```

Syntax errors have no useful path, because the input is not valid JSON at that point.

```scala mdoc
"""{"id":1 "items":[]}""".fromJson[Order]

"""{"id":1,"items":[""".fromJson[Order]
```

The decoder stops at the first error.
It does not collect every problem in the input, so fix one and decode again.
