---
id: json-decoder
title: "JsonDecoder"
description: "Reference for the JsonDecoder type class: decodeJson, the fromJson extension methods, error handling, the combinators that build new decoders, and hand-written low-level decoders."
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
The full list of types that have one is in [Built-in Instances](built-in-instances.md).
Derived instances for your own types are covered in [Decoding](../how-to/decoding.md).

| Method                                                        | What it does                                             | Details                                                 |
|---------------------------------------------------------------|----------------------------------------------------------|---------------------------------------------------------|
| `decodeJson(...)`                                             | Decodes a `CharSequence`, `Chunk[Byte]` or `Array[Byte]` | [decodeJson](#decodejson)                               |
| `fromJson`                                                    | Extension: decodes a string or bytes                     | [Extension methods](#extension-methods)                 |
| `fromJsonAST`                                                 | Decodes from a `Json` tree                               | [fromJsonAST](#fromjsonast)                             |
| `map`, `mapOrFail`                                            | Transforms a decoded value, optionally failing           | [map and mapOrFail](#map-and-maporfail)                 |
| `widen`                                                       | Retypes a decoder for a supertype                        | [widen](#widen)                                         |
| `zip` (`<*>`), `zipLeft` (`<*`), `zipRight` (`*>`), `zipWith` | Decodes a JSON array of two values                       | [zip](#zip-zipleft-zipright-and-zipwith)                |
| `both`, `bothLeft`, `bothRight`, `bothWith`                   | Decodes the same value twice                             | [both](#both-bothleft-bothright-and-bothwith)           |
| `orElse` (`<>`), `orElseEither` (`<+>`)                       | Falls back to another decoder on failure                 | [orElse](#orelse-and-orelseeither)                      |
| `JsonDecoder.peekChar`                                        | Chooses a decoder by the next character                  | [peekChar](#peekchar)                                   |
| `unsafeDecodeMissing`                                         | Value used when a field is absent                        | [Missing Fields](#missing-fields)                       |
| `JsonDecoder.suspend`                                         | Defers construction for recursive types                  | [Recursive Decoders](#recursive-decoders)               |
| `JsonDecoder.keyValueChunk`                                   | Decodes an object into ordered key/value pairs           | [Map Keys](#map-keys)                                   |
| `unsafeDecode`, `JsonDecoder.AbstractJsonDecoder`             | The low-level method a hand-written decoder implements   | [Writing a Decoder by Hand](#writing-a-decoder-by-hand) |
| `unsafeFromJsonAST`                                           | The low-level method behind `fromJsonAST`                | [Decoding from the AST](#decoding-from-the-ast)         |
| `JsonDecoder.UnsafeJson`                                      | The exception a low-level decoder throws                 | [The Decoding Contract](#the-decoding-contract)         |
| `JsonDecoder.derived`                                         | Scala 3 `derives JsonDecoder`                            | [Scala 2 vs Scala 3](#scala-2-vs-scala-3)               |

## Decoding Values

The methods in this section are the entry points: they take input and return `Either[String, A]`.

### `decodeJson`

`decodeJson` has three overloads, one per input type:

```scala
trait JsonDecoder[A] {
  def decodeJson(str: CharSequence): Either[String, A]
  def decodeJson(bytes: Chunk[Byte]): Either[String, A]
  def decodeJson(bytes: Array[Byte]): Either[String, A]
}
```

All three overloads return `Left(message)` for input that is malformed or does not match `A`.
Exceptions thrown by your own functions, for example inside `map`, are not caught.
The two byte overloads read UTF-8 bytes directly, without first copying them into a `String`.
The `Array[Byte]` overload wraps the array in a `Chunk` and calls the `Chunk` one.

The examples on this page share one `Person` type with a derived decoder:

```scala mdoc:silent
import zio.Chunk
import zio.json._

final case class Person(name: String, age: Int)

object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
}
```

Each overload decodes the same value from a different input:

```scala mdoc
JsonDecoder[Person].decodeJson("""{"name":"Ada","age":36}""")

JsonDecoder[Int].decodeJson(Chunk.fromArray("42".getBytes("UTF-8")))

JsonDecoder[Int].decodeJson("42".getBytes("UTF-8"))
```

A function passed to `JsonDecoder#map` that throws makes `JsonDecoder#decodeJson` throw too, so catch such exceptions yourself, or return a `Left` from `JsonDecoder#mapOrFail` instead:

```scala mdoc
scala.util.Try(JsonDecoder[Int].map(n => 10 / n).decodeJson("0"))
```

### Trailing Input Is Ignored

`decodeJson` and `fromJson` read one complete JSON value from the start of the input and stop there.
They do not check that the rest of the input is empty, so whatever follows the first value is ignored.
For numbers this means a top-level number stops at the first character that cannot continue it, and the rest is dropped without an error:

```scala mdoc
"1e3".fromJson[Int]

"12x".fromJson[Int]

"""{"name":"Ada","age":36} and more""".fromJson[Person]
```

Inside an object or an array the same input fails, because the decoder then expects a `,` or a closing bracket after the value:

```scala mdoc
"[1e3]".fromJson[List[Int]]

"""{"a":12x}""".fromJson[Map[String, Int]]
```

[How Strict Is the Decoder](../explanation/decoder-strictness.md) explains this and the other leniencies, and how to get stricter checks.

### Extension methods

`import zio.json._` adds `fromJson[A]` to `CharSequence`, `Chunk[Byte]` and `Array[Byte]`.
They are defined in `DecoderOps`, `DecoderBytesOps` and `DecoderByteArrayOps` in the package object, and each delegates to the matching `decodeJson` overload:

```scala mdoc
"""{"name":"Ada","age":36}""".fromJson[Person]

"""{"name":"Ada","age":36}""".getBytes("UTF-8").fromJson[Person]
```

### Errors

The error type is a plain `String`.
It is rendered from a path to the failure, in a format close to `jq`.
Input that ends too early gives `Unexpected end of input`, and input nested too deeply gives `Unexpected structure`.
See [Errors](errors.md) for the full format.

A missing field and a value of the wrong type each produce an error that names the field:

```scala mdoc
"""{"name":"Ada"}""".fromJson[Person]

"""{"name":"Ada","age":"old"}""".fromJson[Person]
```

### `fromJsonAST`

`fromJsonAST` decodes from an already parsed [`Json`](json-ast.md) tree instead of text:

```scala
trait JsonDecoder[A] {
  def fromJsonAST(json: Json): Either[String, A]
}
```

Errors have the same shape as `decodeJson`.
`Json#as[A]` calls this method.

A `Json.Num` decodes as an `Int`, and a `Json.Str` does not:

```scala mdoc
import zio.json.ast.Json

JsonDecoder[Int].fromJsonAST(Json.Num(1))

JsonDecoder[Int].fromJsonAST(Json.Str("one"))
```

Some messages from the AST path are worded differently from the text path, for example `expected a Int` instead of `expected an Int`.
How the AST path works, and why it can be slower for some types, is covered in [Decoding from the AST](#decoding-from-the-ast).

## Transforming a Decoder

These methods build a new decoder from an existing one without touching the JSON layout.

### `map` and `mapOrFail`

`map` applies a function to the decoded value, and `mapOrFail` applies one that can reject it:

```scala
trait JsonDecoder[A] {
  def map[B](f: A => B): JsonDecoder[B]
  def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B]
}
```

Use `map` when the conversion cannot fail and `mapOrFail` when it can.
A `Left` from `mapOrFail` becomes the decode error at the position of the value.
The walkthrough is in [Manual Instances](../how-to/manual-instances.md).

To wrap an `Int` in a type and to reject non-positive numbers:

```scala mdoc:silent
final case class UserId(value: Int)

val userId: JsonDecoder[UserId] = JsonDecoder[Int].map(UserId(_))

val positive: JsonDecoder[Int] =
  JsonDecoder[Int].mapOrFail(n => if (n > 0) Right(n) else Left("must be positive"))
```

The first decoder wraps the value, and the second turns the `Left` into an error:

```scala mdoc
"7".fromJson[UserId](userId)

"-1".fromJson[Int](positive)
```

### `widen`

`widen` returns the same decoder typed for a supertype `B`:

```scala
trait JsonDecoder[A] {
  def widen[B >: A]: JsonDecoder[B]
}
```

It is an unchecked cast and creates no new decoder.

## Combining Decoders

These methods combine two decoders, either one after the other, both over the same value, or as alternatives.

### `zip`, `zipLeft`, `zipRight` and `zipWith`

The `zip` family decodes a JSON array whose elements are read by the two decoders in turn:

```scala
trait JsonDecoder[A] {
  def zip[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)]
  def zipLeft[B](that: => JsonDecoder[B]): JsonDecoder[A]
  def zipRight[B](that: => JsonDecoder[B]): JsonDecoder[B]
  def zipWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C]
}
```

The operators are aliases: `<*>` is `zip`, `<*` is `zipLeft`, and `*>` is `zipRight`.
Zipping reads the same positional layout that the tuple instances use: a JSON array with one element per decoder.
`zipLeft` and `zipRight` still decode both elements, then discard one.

To decode a `[name, count]` pair:

```scala mdoc:silent
val nameAndCount = JsonDecoder[String] <*> JsonDecoder[Int]
```

Each variant reads the same two-element array and keeps a different part of it:

```scala mdoc
nameAndCount.decodeJson("""["apples",3]""")

(JsonDecoder[String] *> JsonDecoder[Int]).decodeJson("""["apples",3]""")

(JsonDecoder[String] <* JsonDecoder[Int]).decodeJson("""["apples",3]""")

JsonDecoder[String].zipWith(JsonDecoder[Int])((s, n) => s * n).decodeJson("""["ab",3]""")
```

### `both`, `bothLeft`, `bothRight` and `bothWith`

The `both` family runs two decoders over the same JSON value:

```scala
trait JsonDecoder[A] {
  def both[B](that: => JsonDecoder[B]): JsonDecoder[(A, B)]
  def bothLeft[B](that: => JsonDecoder[B]): JsonDecoder[A]
  def bothRight[B](that: => JsonDecoder[B]): JsonDecoder[B]
  def bothWith[B, C](that: => JsonDecoder[B])(f: (A, B) => C): JsonDecoder[C]
}
```

It rewinds the input between the two decoders, instead of reading one value after another as `zip` does.
Use it to decode two views of one object.
The input is buffered while the first decoder runs, so this costs memory on large values.

Two decoders that each read one field of the same object:

```scala mdoc:silent
final case class Name(name: String)
final case class Age(age: Int)

val nameDecoder: JsonDecoder[Name] = DeriveJsonDecoder.gen[Name]
val ageDecoder: JsonDecoder[Age]   = DeriveJsonDecoder.gen[Age]
```

Combined with `both`, they read the object once each:

```scala mdoc
nameDecoder.both(ageDecoder).decodeJson("""{"name":"Ada","age":36}""")
```

### `orElse` and `orElseEither`

`orElse` and `orElseEither` try a second decoder when the first one fails:

```scala
trait JsonDecoder[A] {
  def orElse[A1 >: A](that: => JsonDecoder[A1]): JsonDecoder[A1]
  def orElseEither[B](that: => JsonDecoder[B]): JsonDecoder[Either[A, B]]
}
```

If this decoder fails, the input is rewound and `that` is tried.
`<>` is an alias for `orElse`, and `<+>` is an alias for `orElseEither`.
`orElseEither` wraps the result in `Left` or `Right`, so the two result types can differ.

To accept either a number or a string:

```scala mdoc:silent
val intOrString = JsonDecoder[Int] <+> JsonDecoder[String]
```

A number lands in `Left` and a string in `Right`:

```scala mdoc
intOrString.decodeJson("1")

intOrString.decodeJson("\"one\"")
```

:::warning[Fallback can use unbounded memory]
Because the decoder rewinds, it must buffer everything it has read.
The source documents this as unsafe against denial of service. Prefer `peekChar` when one character tells the cases apart.
:::

### `peekChar`

`JsonDecoder.peekChar` is on the companion object:

```scala
object JsonDecoder {
  def peekChar[A](partialFunction: PartialFunction[Char, JsonDecoder[A]]): JsonDecoder[A]
}
```

It reads the next non-whitespace character, puts it back, and picks the decoder for that character.
If no case matches, decoding fails with ``(missing case in `peekChar` for 'x'')``, where `x` is the character read.

To pick the `Boolean` decoder for `t` or `f` and the `Int` decoder otherwise:

```scala mdoc:silent
val boolOrInt: JsonDecoder[AnyVal] = JsonDecoder.peekChar[AnyVal] {
  case 't' | 'f' => JsonDecoder[Boolean].widen
  case _         => JsonDecoder[Int].widen
}
```

The first character of each input selects the decoder:

```scala mdoc
boolOrInt.decodeJson("true")

boolOrInt.decodeJson("5")
```

## Missing Fields

A derived decoder asks the field's decoder for a value when the field is absent from a JSON object:

```scala
trait JsonDecoder[A] {
  def unsafeDecodeMissing(trace: List[JsonError]): A
}
```

The default fails with `missing`.
The `Option` decoder overrides it to return `None`, which is why an `Option` field may be left out.
Collection decoders (`List`, `Map`, `Chunk`, ...) return an empty collection here, but derived decoders only use it when `explicitEmptyCollections` has `decoding = false`.
By default a missing collection field is an error.
`map`, `mapOrFail`, `orElse` and `suspend` forward this call to the decoder they wrap.
`zip`, the `both` family and `peekChar` do not, so they fail with `missing`.
See [Optional and Default Fields](../how-to/optional-and-default-fields.md) for the user-facing rules, and [Annotations](annotations.md) and [JsonCodecConfiguration](json-codec-configuration.md) for field defaults and null handling.

To make a field decode as `0` when it is absent, override `unsafeDecodeMissing` in a decoder that otherwise delegates to the `Int` decoder:

```scala mdoc:silent
import zio.json.internal.RetractReader

final case class Counter(name: String, count: Int)

object Counter {
  private val zeroIfMissing: JsonDecoder[Int] = new JsonDecoder.AbstractJsonDecoder[Int] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Int =
      JsonDecoder.int.unsafeDecode(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Int =
      JsonDecoder.int.unsafeFromJsonAST(trace, json)

    override def unsafeDecodeMissing(trace: List[JsonError]): Int = 0
  }

  implicit val decoder: JsonDecoder[Counter] = {
    implicit val count: JsonDecoder[Int] = zeroIfMissing
    DeriveJsonDecoder.gen[Counter]
  }
}
```

The derived decoder now fills the absent `count` field, while a present field is decoded as usual:

```scala mdoc
"""{"name":"visits"}""".fromJson[Counter]

"""{"name":"visits","count":3}""".fromJson[Counter]
```

## Recursive Decoders

`JsonDecoder.suspend` defers building a decoder until first use, so a decoder can refer to itself:

```scala
object JsonDecoder {
  def suspend[A](decoder0: => JsonDecoder[A]): JsonDecoder[A]
}
```

A tree whose children are trees of the same type:

```scala mdoc:silent
final case class Tree(value: Int, children: List[Tree])

object Tree {
  implicit val decoder: JsonDecoder[Tree] =
    JsonDecoder.suspend(DeriveJsonDecoder.gen[Tree])
}
```

## Map Keys

A JSON object decodes into a `Map[K, V]`, `HashMap`, `mutable.Map`, `SortedMap` or `ListMap` when `V` has a `JsonDecoder` and `K` has a `JsonFieldDecoder`.
Built-in key decoders exist only for `String`, `Int`, `Long` and `UUID`.
When the same key appears twice, the last value wins and no error is reported.
[JsonFieldEncoder and JsonFieldDecoder](json-field-codecs.md) covers key types in full, including how to make one for your own type.

`JsonDecoder.keyValueChunk` decodes an object into a `Chunk` of key/value pairs instead, keeping the order of the keys and every duplicate:

```scala
object JsonDecoder {
  def keyValueChunk[K, A](implicit K: JsonFieldDecoder[K], A: JsonDecoder[A]): JsonDecoder[Chunk[(K, A)]]
}
```

It is not implicit, because a `Chunk[(K, A)]` already has a decoder that reads a JSON array of pairs, so call it by name:

```scala mdoc
JsonDecoder.keyValueChunk[String, Int].decodeJson("""{"b":1,"a":2,"b":3}""")

"""{"b":1,"a":2,"b":3}""".fromJson[Map[String, Int]]
```

## Writing a Decoder by Hand

You rarely need this: `map`, `mapOrFail` and derivation cover almost every type.
It is an advanced API for decoders that must be as fast as the built-in ones, or that read a JSON shape no combinator can express.
A hand-written decoder implements `JsonDecoder#unsafeDecode`, which reads straight from the input with the helpers in `zio.json.internal.Lexer`.

:::warning[No compatibility guarantee]
`JsonDecoder#unsafeDecode` is part of the public `JsonDecoder` trait, but `Lexer`, `RetractReader`, `StringMatrix` and the rest of the `zio.json.internal` package are internal.
They can change in any release without notice, so pin the version and test hand-written decoders when you upgrade.
:::

### The Decoding Contract

Every decoder is built on one abstract method:

```scala
trait JsonDecoder[A] {
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A
}
```

The rules for implementing it are:

- **Start before the value.** `in` is positioned just before the value, which may be preceded by whitespace. Skip it with `in.nextNonWhitespace()` or a `Lexer` helper.
- **Leave the reader after the value.** Consume the value completely and nothing after it, because the caller reads the next `,`, `}` or `]`. `in.retract()` puts back the last character read, and may be called only once after each read.
- **Fail by throwing.** Report an error with `Lexer.error(message, trace)`, which throws `JsonDecoder.UnsafeJson`. Never return `null` or a placeholder.
- **Extend the trace for children.** Before decoding an object field or an array element, prepend `JsonError.ObjectAccess(name)` or `JsonError.ArrayAccess(index)` to `trace` and pass the result to the child decoder, so an error inside it renders as `.name(...)` or `[index](...)`.

`JsonDecoder.UnsafeJson` is a `case class` with one field, `trace: List[JsonError]`.
It extends `Exception` with `NoStackTrace`, so throwing it is cheap.
`Lexer.error(msg, trace)` throws it with `JsonError.Message(msg)` added to the front of `trace`.
`JsonDecoder#decodeJson`, `fromJson` and `JsonDecoder#fromJsonAST` catch it and render the trace with `JsonError.render`.
They also catch the library's end-of-input exception, which `RetractReader#readChar` throws when the input runs out, and turn it into `Unexpected end of input`.

The reader passed in is a `RetractReader`:

```scala
sealed trait RetractReader extends java.io.Reader {
  def readChar(): Char
  def nextNonWhitespace(): Char
  def retract(): Unit
}
```

`RetractReader#readChar` and `RetractReader#nextNonWhitespace` come from a library-private parent trait, `OneCharReader`.
Several `Lexer` methods name that parent as their parameter type; pass them the `RetractReader` you received.

To get a small, plain class, extend `JsonDecoder.AbstractJsonDecoder[A]` instead of the `JsonDecoder` trait.
It is an abstract class that adds nothing, and its only purpose is to reduce the size of the class files your anonymous decoder classes produce.

### Lexer Helpers

`Lexer` has a method for each step of reading JSON.
These are the ones a hand-written decoder uses most:

```scala
object Lexer {
  def error(msg: String, trace: List[JsonError]): Nothing
  def error(c: Char, got: Char, trace: List[JsonError]): Nothing

  def char(trace: List[JsonError], in: OneCharReader, c: Char): Unit
  def string(trace: List[JsonError], in: OneCharReader): CharSequence

  def firstField(in: RetractReader): Boolean
  def nextField(trace: List[JsonError], in: OneCharReader): Boolean
  def field(trace: List[JsonError], in: OneCharReader, matrix: StringMatrix): Int

  def firstArrayElement(in: RetractReader): Boolean
  def nextArrayElement(trace: List[JsonError], in: OneCharReader): Boolean

  def skipValue(trace: List[JsonError], in: RetractReader): Unit
}
```

| Method                        | What it does                                                                                                     |
|-------------------------------|------------------------------------------------------------------------------------------------------------------|
| `error(msg, trace)`           | Throws `UnsafeJson` with `msg` as the message.                                                                   |
| `error(c, got, trace)`        | Throws with the message `expected 'c' got 'got'`.                                                                |
| `char(trace, in, c)`          | Skips whitespace and reads `c`, or fails with `expected 'c' got '...'`.                                          |
| `string(trace, in)`           | Skips whitespace and reads a JSON string, unescaped, or fails with `expected string`.                            |
| `firstField(in)`              | Call after `{`. Returns `false` for an empty object (and consumes the `}`), `true` otherwise.                    |
| `field(trace, in, matrix)`    | Reads a key and its `:`, and returns the key's index in `matrix`, or `-1` for an unknown key.                    |
| `nextField(trace, in)`        | Call after a field's value. Returns `true` after a `,` and `false` after `}`, and fails on anything else.        |
| `firstArrayElement(in)`       | Call after `[`. Returns `false` for an empty array (and consumes the `]`), `true` otherwise.                     |
| `nextArrayElement(trace, in)` | Call after an element. Returns `true` after a `,` and `false` after `]`, and fails on anything else.             |
| `skipValue(trace, in)`        | Skips one value of any type, for example the value of an unknown field. It does not validate the value it skips. |

`Lexer` also has readers for single values, such as `Lexer.boolean`, `Lexer.int`, `Lexer.long`, `Lexer.double`, `Lexer.bigDecimal`, `Lexer.uuid` and readers for the `java.time` types. Delegating to the built-in decoders (`JsonDecoder.int.unsafeDecode(...)`) is simpler, and accepts the same inputs as they do, such as quoted numbers.

`StringMatrix` is the structure `Lexer.field` matches keys against.
`new StringMatrix(names: Array[String], aliases: Array[(String, Int)] = Array.empty)` takes the expected keys and, optionally, extra keys that map to the index of one of them.
The keys must be non-empty and there can be at most 64 of them, aliases included.
Matching happens character by character as the key is read, without building a `String` or hashing it.

### A Decoder for an Object

This decoder reads `{"x": ..., "y": ...}` into a `Point`, in any key order, skipping unknown keys:

```scala mdoc:silent
import zio.json.internal.{ Lexer, StringMatrix }

final case class Point(x: Int, y: Int)

object Point {
  implicit val decoder: JsonDecoder[Point] = new JsonDecoder.AbstractJsonDecoder[Point] {
    private val keys = new StringMatrix(Array("x", "y"))

    def unsafeDecode(trace: List[JsonError], in: RetractReader): Point = {
      Lexer.char(trace, in, '{')
      var x, y         = 0
      var seenX, seenY = false
      if (Lexer.firstField(in))
        while ({
          Lexer.field(trace, in, keys) match {
            case 0 =>
              x = JsonDecoder.int.unsafeDecode(JsonError.ObjectAccess("x") :: trace, in)
              seenX = true
            case 1 =>
              y = JsonDecoder.int.unsafeDecode(JsonError.ObjectAccess("y") :: trace, in)
              seenY = true
            case _ =>
              Lexer.skipValue(trace, in)
          }
          Lexer.nextField(trace, in)
        }) ()
      if (!seenX) Lexer.error("missing", JsonError.ObjectAccess("x") :: trace)
      if (!seenY) Lexer.error("missing", JsonError.ObjectAccess("y") :: trace)
      Point(x, y)
    }
  }
}
```

It decodes valid input, and its errors carry the same paths as a derived decoder's:

```scala mdoc
"""{"y":2,"note":[1,{"a":3}],"x":1}""".fromJson[Point]

"""{"x":"one","y":2}""".fromJson[Point]

"""{"x":1}""".fromJson[Point]

"""[1,2]""".fromJson[Point]
```

An array decoder follows the same pattern with `Lexer.firstArrayElement` and `Lexer.nextArrayElement`, and prepends `JsonError.ArrayAccess(i)` to the trace for element `i`.

### Testing a Decoder Directly

`JsonDecoder#decodeJson` is the normal way to test a decoder.
To call `JsonDecoder#unsafeDecode` itself, for example to check where the reader stops, wrap any `java.io.Reader` in `zio.json.internal.WithRetractReader`:

```scala mdoc:silent
import zio.json.internal.WithRetractReader

val reader = new WithRetractReader(new java.io.StringReader("""{"x":1,"y":2} rest"""))
```

Decoding from the reader returns the value, and the next character read is the one after it:

```scala mdoc
Point.decoder.unsafeDecode(Nil, reader)

reader.read().toChar
```

The decoder stopped right after the closing `}`, so the next character is the space before `rest`.
An error thrown by `unsafeDecode` is a `JsonDecoder.UnsafeJson`, whose `trace` holds the path with the message first:

```scala mdoc
try Point.decoder.unsafeDecode(Nil, new WithRetractReader(new java.io.StringReader("""{"x":true}""")))
catch { case e: JsonDecoder.UnsafeJson => e.trace }
```

Application code should not call `JsonDecoder#unsafeDecode`: it throws instead of returning an `Either`.

### Decoding from the AST

`fromJsonAST` and `Json#as` call a second low-level method:

```scala
trait JsonDecoder[A] {
  def unsafeFromJsonAST(trace: List[JsonError], json: Json): A
}
```

Its default implementation encodes the `Json` value back to text and runs `JsonDecoder#unsafeDecode` on that text.
That is correct for every decoder, including hand-written ones, but slower than reading the tree directly.
The decoders for primitives, `java.time` types, `UUID`, `Option`, `Chunk`, tuples and the `Json` types, derived decoders, and decoders built from them with `map`, `mapOrFail` or `orElse` read the tree directly.
Others, including the `List`, `Vector`, `Set`, `Map` and `Either` decoders, `JsonDecoder.peekChar`, and your hand-written decoders, fall back to the default.

The `Point` decoder above does not override it, so it still works on a `Json` value, through a round trip to text:

```scala mdoc
Json.Obj("x" -> Json.Num(1), "y" -> Json.Num(2)).as[Point]
```

To read the tree directly, override `unsafeFromJsonAST` and follow the same rules: throw with `Lexer.error`, and extend the trace for children.
This decoder for a `Point2` reads both the text and the tree without the round trip:

```scala mdoc:silent
final case class Point2(x: Int, y: Int)

object Point2 {
  private val viaPoint: JsonDecoder[Point2] = Point.decoder.map(p => Point2(p.x, p.y))

  implicit val decoder: JsonDecoder[Point2] = new JsonDecoder.AbstractJsonDecoder[Point2] {
    def unsafeDecode(trace: List[JsonError], in: RetractReader): Point2 =
      viaPoint.unsafeDecode(trace, in)

    override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Point2 =
      json match {
        case obj: Json.Obj =>
          def int(key: String): Int = {
            val trace_ = JsonError.ObjectAccess(key) :: trace
            obj.get(key) match {
              case Some(value) => JsonDecoder.int.unsafeFromJsonAST(trace_, value)
              case None        => Lexer.error("missing", trace_)
            }
          }
          Point2(int("x"), int("y"))
        case _ => Lexer.error("expected object", trace)
      }
  }
}
```

Both paths give the same result and the same error paths:

```scala mdoc
Json.Obj("x" -> Json.Num(1), "y" -> Json.Num(2)).as[Point2]

Json.Obj("x" -> Json.Num(1)).as[Point2]

"""{"x":1,"y":2}""".fromJson[Point2]
```

## Scala 2 vs Scala 3

`DeriveJsonDecoder.gen` works on both.
Scala 3 also has `JsonDecoder.derived`, which is what a `derives JsonDecoder` clause calls:

```scala
object JsonDecoder {
  inline def derived[A: deriving.Mirror.Of](using config: JsonCodecConfiguration): JsonDecoder[A]
}
```

It takes a `JsonCodecConfiguration` as a `using` parameter.
Without a given of your own, `JsonCodecConfiguration.default` from the companion object is used, so nothing has to be in scope.
On Scala 3, a case class derives its decoder like this:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/jsondecoder/DerivesDecoderExample.scala", "derives")
```

Decoding `{"name":"Ada","age":36}` with it gives `Right(Person(Ada,36))`.
To change the configuration for a `derives` clause, put a given `JsonCodecConfiguration` in scope where the type is defined:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/jsondecoder/DerivesDecoderExample.scala", "given-config")
```

With extra fields forbidden, `{"name":"Ada","age":36}` decodes as `Strict` to `Left((invalid extra field))`.

A few messages differ between the two versions.
For example, a sum type read from a `Json.Obj` with more than one key fails with `expected single field object` on Scala 2 and `expected object with a single field` on Scala 3.
[Errors](errors.md#common-messages) lists them.

## Platform Notes

The decoding methods and instances on this page are available on the JVM, Scala.js and Scala Native, with the exceptions below.

### Streaming

`decodeJsonStream`, `decodeJsonStreamInput` and `decodeJsonPipeline` exist only on the JVM, on the `JsonDecoderPlatformSpecific` trait.
On Scala.js and Scala Native that trait is empty.
See [Streaming](../how-to/streaming.md).

### Files and Other Platform Differences

The file helpers in the `zio.json` package, `readJsonAs` and `readJsonLinesAs`, also exist only on the JVM.
On Scala.js and Scala Native, decoding `ZoneId` and `ZonedDateTime` needs the time-zone database, which you add as a separate dependency (see the [installation notes](../index.md)).

## See Also

- [Write a Fast Encoder or Decoder by Hand](../how-to/low-level-codecs.md) for a worked hand-written codec
- [Decoding](../how-to/decoding.md) for derivation, ADTs and custom JSON shapes
- [Manual Instances](../how-to/manual-instances.md) for `map` and `mapOrFail` in practice
- [JsonEncoder](json-encoder.md) and [JsonCodec](json-codec.md)
- [JsonFieldEncoder and JsonFieldDecoder](json-field-codecs.md) for map keys
- [Built-in Instances](built-in-instances.md) for every type that has a decoder
- [Errors](errors.md) for the error message format
- [JSON AST](json-ast.md) for the `Json` type accepted by `fromJsonAST`
- [How Strict Is the Decoder](../explanation/decoder-strictness.md) for what the decoder accepts
