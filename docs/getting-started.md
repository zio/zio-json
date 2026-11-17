---
id: getting-started
title: "Getting Started"
description: "Learn ZIO JSON step by step: install it, encode and decode case classes, handle optional fields and sealed traits, and read errors."
keywords:
  - "JSON Encoding"
  - "JSON Decoding"
  - "Case Class Codec"
  - "ZIO JSON Tutorial"
---

On this page you will install ZIO JSON, turn Scala values into JSON, read JSON back into Scala values, and handle bad input. Every step builds on the one before it, so run the steps in order.

By the end you will have a small user-profile model: a `Profile` that holds a `Person` with an `Address`, an optional nickname, a list of tags, and a list of contacts of different kinds. You will encode it, decode it, and read the errors it reports for bad input.

## 1. Install

Add the library to your `build.sbt`:

```sbt
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

:::note[Scala.js and Scala Native]
ZIO JSON is published for the JVM, Scala.js and Scala Native, for Scala 2.12, 2.13 and 3. In a Scala.js or Scala Native project, use `%%%` instead of `%%`.

On those two platforms the `java.time` types come from the `scala-java-time` library. If you use `java.time.ZoneId` or `java.time.ZonedDateTime` there, also add the time-zone database in the same version as the `scala-java-time` in your dependency tree (`sbt dependencyTree` shows it), for example:

```scala
libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.10.0"
```

The add-on modules (YAML, golden testing, `@jsonDerive` macros and the integrations) are listed in the [Modules](index.md#modules) section of the Introduction page, with their platforms.
:::

The code on this page uses Scala 2.13 syntax. Where Scala 3 is shorter, a Scala 3 tip follows the example.

Every snippet below assumes this import:

```scala mdoc:silent:reset-object
import zio.json._
```

The import brings in the type classes `JsonEncoder` and `JsonDecoder`, the derivation helpers, and the extension methods `toJson` and `fromJson`.

## 2. Encode Your First Value

A *codec* is a pair of one encoder (Scala to JSON) and one decoder (JSON to Scala). The easiest way to get one for a case class is to let the compiler derive it. Put the codec on the companion object so Scala finds it without any import:

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

:::tip[Scala 3]
On Scala 3 you can skip the companion and write `derives JsonCodec` on the class instead:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/gettingstarted/DerivesExample.scala", "address")
```
:::

## 3. Decode Your First Value

`fromJson` goes the other way. It does not throw. It returns an `Either[String, A]`: a `Right` with the value, or a `Left` with an error message:

```scala mdoc
"""{"street":"1 Main St","city":"Springfield"}""".fromJson[Address]
```

The same codec supplies the decoder, because a `JsonCodec` holds both halves.

## 4. Nest One Case Class in Another

A `Person` holds the `Address` from step 2. A case class field can be any type that has a codec, and `Address` got its codec in step 2:

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

## 5. Model Alternatives with a Sealed Trait

A person can be reached by email or by phone. A `sealed trait` models "one of several shapes". Derive the codec on the trait itself, and its subtypes are derived along with it:

```scala mdoc:silent
sealed trait Contact

object Contact {
  final case class Email(address: String) extends Contact
  final case class Phone(number: String)  extends Contact

  implicit val codec: JsonCodec[Contact] = DeriveJsonCodec.gen[Contact]
}
```

By default the JSON names the subtype in a single wrapper key. This key is the *tag* that tells the decoder which subtype to build:

```scala mdoc
val contacts: List[Contact] = List(Contact.Email("alice@example.com"), Contact.Phone("555-0100"))

println(contacts.toJson)

"""[{"Phone":{"number":"555-0100"}}]""".fromJson[List[Contact]]
```

:::tip[Scala 3]
On Scala 3 the same alternatives can be an `enum` with `derives JsonCodec`:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/gettingstarted/DerivesExample.scala", "enum")
```

An enum case is written the same way, so `Contact.Phone("555-0100")` encodes as `{"Phone":{"number":"555-0100"}}`.
:::

## 6. Put It Together with Optional and List Fields

Now combine everything into a `Profile`. It holds the `Person` from step 4, the `Contact` list from step 5, a nickname that may be missing, and a list of tags. `Option` and `List` already have codecs, so the derived codec needs nothing extra:

```scala mdoc:silent
final case class Profile(
  person: Person,
  nickname: Option[String],
  tags: List[String],
  contacts: List[Contact]
)

object Profile {
  implicit val codec: JsonCodec[Profile] = DeriveJsonCodec.gen[Profile]
}
```

A `None` is left out of the JSON, and a `List` becomes a JSON array:

```scala mdoc
val profile = Profile(alice, None, List("admin", "dev"), contacts)

println(profile.toJsonPretty)
```

A missing `nickname` field decodes as `None`, so the encoded text decodes back to the same value:

```scala mdoc
profile.toJson.fromJson[Profile]
```

## 7. Handle a Decode Error

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

In the full `Profile`, a failure deep inside the model keeps the whole path, so you can see exactly where the problem sits:

```scala mdoc
"""{"person":{"name":"Alice","age":30,"address":{"street":"1 Main St","city":false}},"tags":[],"contacts":[]}""".fromJson[Profile]
```

Because the result is an ordinary `Either`, you handle it with `fold`, `map`, or a `match`, as with any other `Either`:

```scala mdoc
"""{"street":"1 Main St"}""".fromJson[Address] match {
  case Right(address) => s"Got ${address.city}"
  case Left(error)    => s"Could not decode: $error"
}
```

## 8. Where Next

You can now encode, decode, nest, model alternatives, and read errors. To practise on a larger example, continue with the next tutorial, [Model a Real API Payload](tutorial/model-an-api-payload.md). Or pick the guide that matches your next task:

- [Control the JSON a type encodes to](how-to/encoding.md) when the default JSON shape is not the one you need.
- [Decode JSON whose shape differs from your case class](how-to/decoding.md) when the JSON keys or nesting do not match your fields.
- [Rename fields](how-to/field-names.md) when the JSON names differ from your Scala names.
- [Change how sealed traits look in JSON](how-to/sealed-traits.md), for example with a discriminator field instead of the wrapper key.
- [Optional, default and null fields](how-to/optional-and-default-fields.md) for missing, `null` and empty fields.
- [Process large JSON as a stream](how-to/streaming.md) on the JVM.

For every option and method, see the [Reference](reference/index.md). For why ZIO JSON works the way it does, see [How ZIO JSON Works](explanation/how-it-works.md).
