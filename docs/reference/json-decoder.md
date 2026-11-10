---
id: json-decoder
title: "JsonDecoder"
description: "Reference for the JsonDecoder type class: decodeJson, the fromJson extension methods, error handling, and the combinators that build new decoders."
keywords:
  - "JSON Decoding"
  - "Type Class"
  - "Decoder Combinators"
  - "JsonDecoder"
---

`JsonDecoder[A]` is the type class that reads JSON text and produces a value of type `A`, or a human-readable error.
Use it directly when you write a decoder by hand or build one from another decoder.
Most code calls `fromJson` and lets the compiler find the decoder in the implicit scope.
Instances for the standard types live in the `JsonDecoder` companion object, and `JsonDecoder.apply[A]` returns the one in scope.
Derived instances for your own types are covered in [Decoding](../how-to/decoding.md).

| Method                                                        | What it does                                             | Details                                       |
|---------------------------------------------------------------|----------------------------------------------------------|-----------------------------------------------|
| `decodeJson(...)`                                             | Decodes a `CharSequence`, `Chunk[Byte]` or `Array[Byte]` | [decodeJson](#decodejson)                     |
| `fromJson`                                                    | Extension: decodes a string or bytes                     | [Extension methods](#extension-methods)       |
| `fromJsonAST`                                                 | Decodes from a `Json` tree                               | [fromJsonAST](#fromjsonast)                   |
| `map`, `mapOrFail`                                            | Transforms a decoded value, optionally failing           | [map and mapOrFail](#map-and-maporfail)       |
| `widen`                                                       | Retypes a decoder for a supertype                        | [widen](#widen)                               |
| `zip` (`<*>`), `zipLeft` (`<*`), `zipRight` (`*>`), `zipWith` | Decodes a JSON array of two values                       | [zip](#zip-zipleft-zipright-and-zipwith)      |
| `both`, `bothLeft`, `bothRight`, `bothWith`                   | Decodes the same value twice                             | [both](#both-bothleft-bothright-and-bothwith) |
| `orElse` (`<>`), `orElseEither` (`<+>`)                       | Falls back to another decoder on failure                 | [orElse](#orelse-and-orelseeither)            |
| `JsonDecoder.peekChar`                                        | Chooses a decoder by the next character                  | [peekChar](#peekchar)                         |
| `JsonDecoder.suspend`                                         | Defers construction for recursive types                  | [Recursive Decoders](#recursive-decoders)     |
| `unsafeDecodeMissing`                                         | Value used when a field is absent                        | [Missing Fields](#missing-fields)             |

## Decoding Values

### `decodeJson`

```scala
final def decodeJson(str: CharSequence): Either[String, A]
final def decodeJson(bytes: Chunk[Byte]): Either[String, A]
final def decodeJson(bytes: Array[Byte]): Either[String, A]
```

All three overloads return `Left(message)` on failure, never throw.
The two byte overloads read UTF-8 bytes directly, without first copying them into a `String`.
The `Array[Byte]` overload wraps the array in a `Chunk` and calls the `Chunk` one.
None of them requires the input to be consumed entirely: trailing content after the first value is not reported.

```scala mdoc:silent
import zio.Chunk
import zio.json._

final case class Person(name: String, age: Int)

object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
}
```

```scala mdoc
JsonDecoder[Person].decodeJson("""{"name":"Ada","age":36}""")

JsonDecoder[Int].decodeJson(Chunk.fromArray("42".getBytes("UTF-8")))

JsonDecoder[Int].decodeJson("42".getBytes("UTF-8"))
```

### Extension methods

`import zio.json._` adds `fromJson[A]` to `CharSequence`, `Chunk[Byte]` and `Array[Byte]`.
They are defined in `DecoderOps`, `DecoderBytesOps` and `DecoderByteArrayOps` in the package object, and each delegates to the matching `decodeJson` overload.

```scala mdoc
"""{"name":"Ada","age":36}""".fromJson[Person]

"""{"name":"Ada","age":36}""".getBytes("UTF-8").fromJson[Person]
```

### Errors

The error type is a plain `String`.
It is rendered from a path to the failure, in a format close to `jq`.
Input that ends too early gives `Unexpected end of input`, and input nested too deeply gives `Unexpected structure`.
See [Errors](errors.md) for the full format.

```scala mdoc
"""{"name":"Ada"}""".fromJson[Person]

"""{"name":"Ada","age":"old"}""".fromJson[Person]
```

### `fromJsonAST`

```scala
final def fromJsonAST(json: Json): Either[String, A]
```

Decodes from an already parsed [`Json`](json-ast.md) tree instead of text.
Errors have the same shape as `decodeJson`.

```scala mdoc
import zio.json.ast.Json

JsonDecoder[Int].fromJsonAST(Json.Num(1))

JsonDecoder[Int].fromJsonAST(Json.Str("one"))
```

## Transforming a Decoder

### `map` and `mapOrFail`

```scala
final def map[B](f: A => B): JsonDecoder[B]
final def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B]
```

Use `map` when the conversion cannot fail and `mapOrFail` when it can.
A `Left` from `mapOrFail` becomes the decode error at the position of the value.
The walkthrough is in [Manual Instances](../how-to/manual-instances.md).

```scala mdoc:silent
final case class UserId(value: Int)

val userId: JsonDecoder[UserId] = JsonDecoder[Int].map(UserId(_))

val positive: JsonDecoder[Int] =
  JsonDecoder[Int].mapOrFail(n => if (n > 0) Right(n) else Left("must be positive"))
```

```scala mdoc
"7".fromJson[UserId](userId)

"-1".fromJson[Int](positive)
```

### `widen`

```scala
final def widen[B >: A]: JsonDecoder[B]
```

Returns the same decoder typed for a supertype `B`. It is an unchecked cast and creates no new decoder.

## Combining Decoders

### `zip`, `zipLeft`, `zipRight` and `zipWith`

```scala
final def zip[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)]
final def zipLeft[B](that: => JsonDecoder[B]): JsonDecoder[A]
final def zipRight[B](that: => JsonDecoder[B]): JsonDecoder[B]
final def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C]
```

The operators are aliases: `<*>` is `zip`, `<*` is `zipLeft`, and `*>` is `zipRight`.
Zipping reads the same positional layout that the tuple instances use: a JSON array with one element per decoder.
`zipLeft` and `zipRight` still decode both elements, then discard one.

```scala mdoc:silent
val nameAndCount = JsonDecoder[String] <*> JsonDecoder[Int]
```

```scala mdoc
nameAndCount.decodeJson("""["apples",3]""")

(JsonDecoder[String] *> JsonDecoder[Int]).decodeJson("""["apples",3]""")

(JsonDecoder[String] <* JsonDecoder[Int]).decodeJson("""["apples",3]""")

JsonDecoder[String].zipWith(JsonDecoder[Int])((s, n) => s * n).decodeJson("""["ab",3]""")
```

### `both`, `bothLeft`, `bothRight` and `bothWith`

```scala
final def both[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)]
final def bothLeft[B](that: => JsonDecoder[B]): JsonDecoder[A]
final def bothRight[B](that: => JsonDecoder[B]): JsonDecoder[B]
final def bothWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C]
```

The `both` family runs two decoders over the same JSON value, rewinding between them, instead of reading one value after another as `zip` does.
Use it to decode two views of one object.
The input is buffered while the first decoder runs, so this costs memory on large values.

```scala mdoc:silent
final case class Name(name: String)
final case class Age(age: Int)

val nameDecoder: JsonDecoder[Name] = DeriveJsonDecoder.gen[Name]
val ageDecoder: JsonDecoder[Age]   = DeriveJsonDecoder.gen[Age]
```

```scala mdoc
nameDecoder.both(ageDecoder).decodeJson("""{"name":"Ada","age":36}""")
```

### `orElse` and `orElseEither`

```scala
final def orElse[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1]
final def orElseEither[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]]
```

If this decoder fails, the input is rewound and `that` is tried.
`<>` is an alias for `orElse`, and `<+>` is an alias for `orElseEither`.
`orElseEither` wraps the result in `Left` or `Right`, so the two result types can differ.

```scala mdoc:silent
val intOrString = JsonDecoder[Int] <+> JsonDecoder[String]
```

```scala mdoc
intOrString.decodeJson("1")

intOrString.decodeJson("\"one\"")
```

:::warning[Fallback can use unbounded memory]
Because the decoder rewinds, it must buffer everything it has read.
The source documents this as unsafe against denial of service. Prefer `peekChar` when one character tells the cases apart.
:::

### `peekChar`

```scala
def peekChar[A](partialFunction: PartialFunction[Char, JsonDecoder[A]]): JsonDecoder[A]
```

`JsonDecoder.peekChar` is on the companion object.
It reads the next non-whitespace character, puts it back, and picks the decoder for that character.
If no case matches, decoding fails with a `missing case in peekChar` message.

```scala mdoc:silent
val boolOrInt: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
  case 't' | 'f' => JsonDecoder[Boolean].widen
  case _         => JsonDecoder[Int].widen
}
```

```scala mdoc
boolOrInt.decodeJson("true")

boolOrInt.decodeJson("5")
```

## Missing Fields

```scala
def unsafeDecodeMissing(trace: List[JsonError]): A
```

The decoder is asked for a value when a field is absent from a JSON object.
The default fails with `missing`.
The `Option` decoder overrides it to return `None`, which is why an `Option` field may be left out.
`map`, `mapOrFail`, `orElse` and `suspend` forward this call to the decoder they wrap.
See [Annotations](annotations.md) and [JsonCodecConfiguration](json-codec-configuration.md) for field defaults and null handling.

## Recursive Decoders

```scala
def suspend[A](decoder0: => JsonDecoder[A]): JsonDecoder[A]
```

`JsonDecoder.suspend` defers building a decoder until first use, so a decoder can refer to itself.

```scala mdoc:silent
final case class Tree(value: Int, children: List[Tree])

object Tree {
  implicit val decoder: JsonDecoder[Tree] =
    JsonDecoder.suspend(DeriveJsonDecoder.gen[Tree])
}
```

## Scala 2 vs Scala 3

`DeriveJsonDecoder.gen` works on both.
Scala 3 also has `JsonDecoder.derived`, which is what `derives JsonDecoder` calls. It needs a given `JsonCodecConfiguration`.

```scala
// Scala 3 only
import zio.json.*

final case class Person(name: String, age: Int) derives JsonDecoder
```

## Streaming

`decodeJsonStream`, `decodeJsonStreamInput` and `decodeJsonPipeline` exist only on the JVM, on the `JsonDecoderPlatformSpecific` trait.
See [Streaming](../how-to/streaming.md).

## See Also

- [Decoding](../how-to/decoding.md) for derivation, ADTs and custom JSON shapes
- [Manual Instances](../how-to/manual-instances.md) for `map` and `mapOrFail` in practice
- [JsonEncoder](json-encoder.md) and [JsonCodec](json-codec.md)
- [Errors](errors.md) for the error message format
- [JSON AST](json-ast.md) for the `Json` type accepted by `fromJsonAST`
