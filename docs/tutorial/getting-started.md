---
id: getting-started
title: "Getting Started with ZIO Json"
description: "Learn ZIO JSON step by step: install it, encode and decode case classes, handle optional fields and sealed traits, and read errors."
keywords:
  - "JSON Encoding"
  - "JSON Decoding"
  - "Case Class Codec"
  - "ZIO JSON Tutorial"
---

In this tutorial you will install ZIO JSON, turn Scala values into JSON, read JSON back into Scala values, and handle bad input. Every step builds on the one before it, so run the steps in order.

By the end you will have a small `Person` model that you can encode, decode, and extend with optional fields, lists, and a sealed trait.

## 1. Install and choose a Scala version

Add the library to your `build.sbt`:

```sbt
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

ZIO JSON works on Scala 2.13 and Scala 3. The code in this tutorial uses Scala 2.13 syntax. Where Scala 3 differs, a short Scala 3 note follows the example.

Every snippet below assumes this import:

```scala mdoc:silent
import zio.json._
```

The import brings in the typeclasses `JsonEncoder` and `JsonDecoder`, the derivation helpers, and the extension methods `toJson` and `fromJson`.

## 2. Encode your first value

A *codec* is a pair of one encoder (Scala to JSON) and one decoder (JSON to Scala). The easiest way to get one for a case class is to let the compiler derive it. Put the codec on the companion object so Scala finds it without any import.

```scala mdoc:silent
final case class Address(street: String, city: String)

object Address {
  implicit val codec: JsonCodec[Address] = DeriveJsonCodec.gen[Address]
}
```

Now call `toJson` on a value:

```scala mdoc
val home = Address("1 Main St", "Springfield")

println(home.toJson)
```

Use `toJsonPretty` when you want indented output for people to read:

```scala mdoc
println(home.toJsonPretty)
```

On Scala 3 you can skip the companion and write `derives JsonCodec` on the class instead.

```scala
// Scala 3 only
final case class Address(street: String, city: String) derives JsonCodec
```

## 3. Decode your first value

`fromJson` goes the other way. It does not throw. It returns an `Either[String, A]`: a `Right` with the value, or a `Left` with an error message.

```scala mdoc
"""{"street":"1 Main St","city":"Springfield"}""".fromJson[Address]
```

The same codec supplies the decoder, because a `JsonCodec` holds both halves. If you only need one direction, derive just that half with `DeriveJsonEncoder.gen` or `DeriveJsonDecoder.gen`.

## 4. Nest one case class in another

A case class field can be any type that has a codec. Here a `Person` holds an `Address`.

```scala mdoc:silent
final case class Person(name: String, age: Int, address: Address)

object Person {
  implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
}
```

The nested value becomes a nested JSON object:

```scala mdoc
val alice = Person("Alice", 30, home)

println(alice.toJsonPretty)
```

Decoding the same text gives the original value back:

```scala mdoc
alice.toJson.fromJson[Person]
```

## 5. Add optional and list fields

Standard types such as `Option`, `List`, `Map`, `String`, and the numeric types already have codecs. A case class made of them needs nothing extra.

```scala mdoc:silent
final case class Profile(name: String, nickname: Option[String], tags: List[String])

object Profile {
  implicit val codec: JsonCodec[Profile] = DeriveJsonCodec.gen[Profile]
}
```

A `None` is left out of the JSON, and a missing field decodes as `None`. A `List` is a JSON array.

```scala mdoc
println(Profile("Bob", None, List("admin", "dev")).toJson)

"""{"name":"Bob","tags":["admin","dev"]}""".fromJson[Profile]
```

## 6. Model alternatives with a sealed trait

A `sealed trait` lets one field hold one of several shapes. Derive the codec on the trait itself, and the subtypes need codecs too.

```scala mdoc:silent
sealed trait Contact

object Contact {
  final case class Email(address: String) extends Contact
  final case class Phone(number: String)  extends Contact

  implicit val emailCodec: JsonCodec[Email]  = DeriveJsonCodec.gen[Email]
  implicit val phoneCodec: JsonCodec[Phone]  = DeriveJsonCodec.gen[Phone]
  implicit val codec: JsonCodec[Contact]     = DeriveJsonCodec.gen[Contact]
}
```

By default the JSON names the subtype in a single wrapper key. This key is the *tag* that tells the decoder which subtype to build.

```scala mdoc
val contacts: List[Contact] = List(Contact.Email("a@example.com"), Contact.Phone("555-0100"))

println(contacts.toJson)

"""[{"Phone":{"number":"555-0100"}}]""".fromJson[List[Contact]]
```

You can swap the wrapper key for a field inside the object. That is a *discriminator*, and the sealed-traits how-to guide covers it.

On Scala 3 an `enum` or a sealed trait can use `derives JsonCodec` directly.

```scala
// Scala 3 only
enum Fruit derives JsonCodec {
  case Banana(curvature: Double)
  case Apple(poison: Boolean)
}
```

## 7. Handle a decode error

Bad input never throws. It comes back as a `Left` holding a message. The message is a path in `jq` style, followed by a note in parentheses that says what went wrong.

A value of the wrong type:

```scala mdoc
"""{"street":"1 Main St","city":42}""".fromJson[Address]
```

A missing field:

```scala mdoc
"""{"street":"1 Main St"}""".fromJson[Address]
```

Text that is not JSON at all:

```scala mdoc
"""{"street": womp}""".fromJson[Address]
```

A nested failure keeps the whole path, so you can see exactly where the problem sits:

```scala mdoc
"""{"name":"Alice","age":30,"address":{"street":"1 Main St","city":false}}""".fromJson[Person]
```

Because the result is an ordinary `Either`, you handle it with `fold`, `map`, or a `match`, as with any other.

```scala mdoc
"""{"street":"1 Main St"}""".fromJson[Address] match {
  case Right(address) => s"Got ${address.city}"
  case Left(error)    => s"Could not decode: $error"
}
```

## 8. Where next

You can now encode, decode, nest, extend, and debug. Pick the guide that matches your next task:

- [Encode values to JSON](../how-to/encoding.md) covers every way to produce JSON, including streams.
- [Decode JSON into values](../how-to/decoding.md) covers strings, bytes, and streams as input.
- [Rename fields](../how-to/field-names.md) when the JSON names differ from your Scala names.
- [Work with sealed traits](../how-to/sealed-traits.md) to change the tag or use a discriminator.
- [Annotations reference](../reference/annotations.md) lists every `@json...` annotation.
- [JsonCodec reference](../reference/json-codec.md) lists the codec combinators.
- [How ZIO JSON works](../explanation/how-it-works.md) explains why it is fast and secure.
