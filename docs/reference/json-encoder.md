---
id: json-encoder
title: "JsonEncoder"
description: "Reference for the JsonEncoder type class: encodeJson, the toJson extension methods, contramap, and the combinators that build new encoders."
keywords:
  - "JSON Encoding"
  - "Type Class"
  - "Encoder Combinators"
  - "JsonEncoder"
---

`JsonEncoder[A]` is the type class that turns a value of type `A` into JSON text.
Use it directly when you write an encoder by hand or build one from another encoder.
Most code never calls it directly: it calls `toJson` or `toJsonPretty`, and the compiler finds the encoder in the implicit scope.
Instances for the standard types live in the `JsonEncoder` companion object, so no import is needed to find them.
Derived instances for your own types are covered in [Encoding](../how-to/encoding.md).

| Method                                 | What it does                                           | Details                                               |
|----------------------------------------|--------------------------------------------------------|-------------------------------------------------------|
| `encodeJson(a, indent)`                | Encodes a value to a `CharSequence`, compact or pretty | [Encoding Values](#encodejson)                        |
| `toJson`                               | Extension: compact JSON `String`                       | [Extension methods](#extension-methods)               |
| `toJsonPretty`                         | Extension: pretty JSON `String`                        | [Extension methods](#extension-methods)               |
| `toJsonAST`                            | Extension: encodes to a `Json` tree                    | [Extension methods](#extension-methods)               |
| `contramap`                            | Builds an encoder for `B` from one for `A`             | [contramap](#contramap)                               |
| `narrow`                               | Retypes an encoder for a subtype                       | [narrow](#narrow)                                     |
| `zip`, `zipWith`                       | Encodes a pair as a JSON array                         | [zip and zipWith](#zip-and-zipwith)                   |
| `either`, `orElseEither`, `eitherWith` | Encodes an `Either`, tagged or untagged                | [either](#either-orelseeither-and-eitherwith)         |
| `isNothing`, `isEmpty`                 | Tell derived encoders to omit a field                  | [Missing and Empty Values](#missing-and-empty-values) |
| `JsonEncoder.suspend`                  | Defers construction for recursive types                | [Recursive Encoders](#recursive-encoders)             |

## Encoding Values

### `encodeJson`

```scala
final def encodeJson(a: A, indent: Option[Int] = None): CharSequence
```

`indent` controls pretty printing.
`None` produces compact output.
`Some(n)` produces pretty output that starts at nesting level `n`, with two spaces per level, so `Some(0)` is the usual choice.
The result is a `CharSequence`. Call `toString` to get a `String`.

```scala mdoc:silent
import zio.json._

final case class Person(name: String, age: Int)

object Person {
  implicit val encoder: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
}
```

```scala mdoc
JsonEncoder[Person].encodeJson(Person("Ada", 36)).toString

println(JsonEncoder[Person].encodeJson(Person("Ada", 36), Some(0)))
```

### Extension methods

`import zio.json._` adds three methods to every value that has a `JsonEncoder`. They are defined in `EncoderOps` in the package object.

| Method         | Result                 | Same as                           |
|----------------|------------------------|-----------------------------------|
| `toJson`       | `String`               | `encodeJson(a, None).toString`    |
| `toJsonPretty` | `String`               | `encodeJson(a, Some(0)).toString` |
| `toJsonAST`    | `Either[String, Json]` | `toJsonAST(a)` on the encoder     |

```scala mdoc
Person("Ada", 36).toJson

Person("Ada", 36).toJsonAST
```

`toJsonAST` returns a [`Json`](json-ast.md) tree.
Its default implementation encodes to text and parses the text back, so a custom encoder that needs a fast path can override `toJsonAST`.

## Transforming an Encoder

### `contramap`

```scala
final def contramap[B](f: B => A): JsonEncoder[B]
```

Builds an encoder for `B` from an encoder for `A` and a function from `B` to `A`.
`isNothing` and `isEmpty` of the new encoder defer to the original one.
The walkthrough is in [Manual Instances](../how-to/manual-instances.md).

```scala mdoc:silent
final case class UserId(value: Int)

implicit val userIdEncoder: JsonEncoder[UserId] = JsonEncoder[Int].contramap(_.value)
```

```scala mdoc
UserId(7).toJson
```

### `narrow`

```scala
final def narrow[B <: A]: JsonEncoder[B]
```

Returns the same encoder typed for a subtype `B` of `A`.
It is an unchecked cast: it does not create a new encoder.

```scala mdoc:compile-only
val chars: JsonEncoder[CharSequence] = JsonEncoder[String].contramap(_.toString)

val strings: JsonEncoder[String] = chars.narrow[String]
```

## Combining Encoders

- `zip`: `def zip[B](that: => JsonEncoder[B]): JsonEncoder[(A, B)]`
- `zipWith`: `def zipWith[B, C](that: => JsonEncoder[B])(f: C => (A, B)): JsonEncoder[C]`
- `either`: `def either[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]]`
- `orElseEither`: `def orElseEither[B](that: => JsonEncoder[B]): JsonEncoder[Either[A, B]]`
- `eitherWith`: `def eitherWith[B, C](that: => JsonEncoder[B])(f: C => Either[A, B]): JsonEncoder[C]`

All five are `final`, and `that` is passed by name.

### `zip` and `zipWith`

`zip` encodes a tuple as a JSON array with one element per encoder.
`zipWith` first converts your own type to a tuple with `f`.

```scala mdoc:silent
val pair = JsonEncoder[String].zip(JsonEncoder[Int])

final case class Entry(name: String, count: Int)

val entry = JsonEncoder[String].zipWith(JsonEncoder[Int])((e: Entry) => (e.name, e.count))
```

```scala mdoc
pair.encodeJson(("apples", 3))

entry.encodeJson(Entry("apples", 3))
```

### `either`, `orElseEither` and `eitherWith`

`either` tags the value with `Left` or `Right`.
`orElseEither` writes the value without a tag, so the JSON carries no trace of which side it came from.
`eitherWith` is `either` preceded by `contramap`: you supply `f` to split your own type into `Either[A, B]`.

```scala mdoc:silent
val tagged = JsonEncoder[Int].either(JsonEncoder[String])

val untagged = JsonEncoder[Int].orElseEither(JsonEncoder[String])

val byShape = JsonEncoder[Int].eitherWith(JsonEncoder[String])((s: String) => s.toIntOption.toLeft(s))
```

```scala mdoc
tagged.encodeJson(Left(1))

untagged.encodeJson(Left(1))

byShape.encodeJson("12")
```

The untagged form is only safe to decode if the decoder can tell the two shapes apart.
See `orElseEither` in [JsonDecoder](json-decoder.md#orelse-and-orelseeither).

## Missing and Empty Values

```scala
def isNothing(a: A): Boolean = false

def isEmpty(a: A): Boolean = false
```

Derived encoders ask each field's encoder these two questions to decide whether to leave the field out of the JSON object.
The default for both is `false`.
The `Option` encoder overrides `isNothing`, so a `None` field is omitted.
The collection encoders override `isEmpty`.
Which fields are actually skipped depends on the configuration. See [JsonCodecConfiguration](json-codec-configuration.md) and [Annotations](annotations.md).

Override these methods only when you write an encoder class by hand and want it to behave like `Option` or a collection.
`contramap` forwards both methods to the wrapped encoder.

## Recursive Encoders

```scala
def suspend[A](encoder0: => JsonEncoder[A]): JsonEncoder[A]
```

`JsonEncoder.suspend` defers building an encoder until it is first used.
It lets an encoder refer to itself, for example for a tree type.
It is on the companion object, not on the trait.

```scala mdoc:silent
final case class Tree(value: Int, children: List[Tree])

object Tree {
  implicit val encoder: JsonEncoder[Tree] =
    JsonEncoder.suspend(DeriveJsonEncoder.gen[Tree])
}
```

## Instances and Summoning

`JsonEncoder.apply[A](implicit a: JsonEncoder[A])` returns the implicit instance, so `JsonEncoder[Int]` reads as a lookup.
A `JsonCodec[A]` in scope also provides a `JsonEncoder[A]`: the companion has `fromCodec`.

## Scala 2 vs Scala 3

`DeriveJsonEncoder.gen` works on both.
Scala 3 also has `JsonEncoder.derived`, which is what `derives JsonEncoder` calls.
It needs a given `JsonCodecConfiguration`.

```scala
// Scala 3 only
import zio.json.*

final case class Person(name: String, age: Int) derives JsonEncoder
```

## Streaming

`encodeJsonStream`, which returns a `ZStream[Any, Throwable, Char]`, and the other streaming encoders exist only on the JVM.
They are on the `JsonEncoderPlatformSpecific` trait that `JsonEncoder` extends. The JS and Native builds do not have them.
See [Streaming](../how-to/streaming.md).

## See Also

- [Encoding](../how-to/encoding.md) for derivation, ADTs and manual instances
- [Manual Instances](../how-to/manual-instances.md) for `contramap` in practice
- [JsonDecoder](json-decoder.md) and [JsonCodec](json-codec.md)
- [JSON AST](json-ast.md) for the `Json` type returned by `toJsonAST`
- [Streaming](../how-to/streaming.md) for the JVM-only stream encoders
