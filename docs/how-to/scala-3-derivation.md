---
id: scala-3-derivation
title: "Derive Codecs on Scala 3"
description: "Derive ZIO JSON codecs on Scala 3 with derives, a given configuration, enums, union types, generic classes and opaque types."
keywords:
  - "Scala 3 Derivation"
  - "derives JsonCodec"
  - "Scala 3 Enum JSON"
  - "Union Types"
  - "Opaque Types"
  - "JsonCodecConfiguration"
---

Goal: get JSON codecs for your Scala 3 data types with as little code as possible, and know what to do when derivation needs help.

By the end of this guide you will have codecs for case classes, enums, sealed traits, string-literal union types, generic case classes and opaque types, all on Scala 3, and you will recognise the compile errors derivation reports. We start with a single `derives` clause and add one Scala 3 feature per section.

## The Problem

On Scala 2, every type you encode or decode needs a companion object with an `implicit val codec: JsonCodec[T] = DeriveJsonCodec.gen[T]` line, or the `@jsonDerive` annotation that writes it for you. Scala 3 replaces that line with a `derives` clause, but it also brings types that Scala 2 does not have: `enum`, union types and opaque types. Some of them work out of the box, some behave differently from their nearest Scala 2 equivalent, and one (opaque types) needs a hand-written codec. The configuration also moves from an `implicit` value to a `given`, and where you put it decides which types it affects.

This guide shows each case on its own, with the JSON it produces when run on Scala 3.9.0.

## What You Need

Only the core module, which is published for Scala 3:

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

Every example file on this page starts with `import zio.json.*`, which brings in the type classes, the derivation macros and the `toJson` and `fromJson` extension methods.

The guide assumes you know what `JsonEncoder`, `JsonDecoder` and `JsonCodec` are (see the [Getting Started](../tutorial/getting-started.md) tutorial). Default values of case class fields work on Scala 3 without the `-Yretain-trees` compiler option.

## 1. Derive a Codec with `derives`

Add `derives JsonCodec` to a case class to get an encoder and a decoder for it:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step1DerivesExample.scala", "derives")
```

The compiler turns the clause into a `given JsonCodec[Banana]` in the companion object of `Banana`, built by `JsonCodec.derived`. The `toJson` and `fromJson` extension methods then find it:

```text
Banana encoded: {"curvature":0.5}
Banana decoded: Right(Banana(0.5))
```

When a type is only written or only read, derive just that side. `derives JsonEncoder` and `derives JsonDecoder` produce one instance each:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step1DerivesExample.scala", "encoder-decoder")
```

Encoding a `Receipt` and decoding a `Refund` print:

```text
Receipt encoded: {"id":1,"total":9.99}
Refund decoded: Right(Refund(1,damaged))
```

`DeriveJsonCodec.gen`, `DeriveJsonEncoder.gen` and `DeriveJsonDecoder.gen` still exist on Scala 3, and a `derives` clause is a shorter way to call them. In code that must also compile on Scala 2, keep the explicit call in the companion object and declare the instance as an `implicit val`, which Scala 3 accepts too. On Scala 3 alone, the explicit form looks like this:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step1DerivesExample.scala", "explicit-given")
```

## 2. Change the Configuration with a `given`

A `derives` clause needs no configuration of its own. Without one, it uses `JsonCodecConfiguration.default` from the companion object of `JsonCodecConfiguration`:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step2ConfigurationExample.scala", "default")
```

`Visitor` keeps the field names as they are in Scala:

```text
Visitor (default): {"firstName":"Ada","lastVisit":"2026-11-17"}
```

To change the configuration, put a `given JsonCodecConfiguration` where the derived instance can see it. The derived instance lives in the companion object of the type, so a given in that companion object applies to that type only:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step2ConfigurationExample.scala", "companion")
```

Only `Customer` is written in snake case:

```text
Customer (companion given): {"first_name":"Ada","last_name":"Lovelace"}
```

A given in an enclosing object applies to every `derives` clause inside that object:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step2ConfigurationExample.scala", "enclosing")
```

Both case classes in `billing` use kebab case:

```text
Invoice (enclosing given): {"invoice-number":42,"due-date":"2026-12-01"}
Payment (enclosing given): {"invoice-number":42,"amount-paid":10}
```

To share one configuration across files, define it once in an object and import it with `import <object>.given`. A plain wildcard import (`import JsonConfig.*`) does not bring givens into scope:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step2ConfigurationExample.scala", "import")
```

The imported configuration forbids extra fields, so an input with an unknown `note` field is rejected:

```text
LineItem (imported given): Left((invalid extra field))
```

:::warning[A top-level given covers the whole package]
A `given JsonCodecConfiguration` written at the top level of a file is a member of its package. It applies to every `derives` clause in that package, in every file, not only in the file where you wrote it. Put the given in an object and import it where you want it.
:::

A given is read when the codec is derived, at compile time. Annotations on the type, such as `@jsonMemberNames` or `@jsonDiscriminator`, take priority over it. [JsonCodecConfiguration](../reference/json-codec-configuration.md) lists every option.

## 3. Encode Scala 3 Enums

A Scala 3 `enum` derives like a sealed trait. Because `enumValuesAsStrings` is `true` by default on Scala 3, an enum whose cases all have no parameters is written as plain strings:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step3EnumExample.scala", "simple-enum")
```

The decoder accepts only the case names:

```text
Status encoded: "Suspended"
Status decoded: Right(Closed)
Status unknown: Left((invalid enumeration value))
```

As soon as one case has parameters, the enum is no longer written as strings. Every case, including a parameterless one such as `Cash`, becomes an object with one key, the case name:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step3EnumExample.scala", "enum-with-parameters")
```

Running it shows the wrapper object for both kinds of case:

```text
Card encoded: {"Card":{"last4":"4242"}}
Cash encoded: {"Cash":{}}
```

To put the case name inside the object instead of around it, add `@jsonDiscriminator` to the enum. The encoder writes the discriminator first, and the decoder accepts it anywhere in the object:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step3EnumExample.scala", "discriminator")
```

The case name now sits in a `type` field:

```text
Created encoded: {"type":"Created","id":7}
Purged encoded: {"type":"Purged"}
Deleted decoded: Right(Deleted(7))
```

Annotations work on enum cases as they do on case classes. `@jsonHint` changes the string written for a case:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step3EnumExample.scala", "hints")
```

`(Priority.Urgent: Priority).toJson` gives `"p1"`.

To write a parameterless enum as objects, as Scala 2 does by default for case objects, set `enumValuesAsStrings = false` in a given configuration:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step3EnumExample.scala", "as-objects")
```

`(legacy.Level.High: legacy.Level).toJson` gives `{"High":{}}`. [Sealed traits](sealed-traits.md#5-write-case-objects-as-strings) explains the string form in more detail, including how it differs from Scala 2.

## 4. Derive Sealed Traits and Nested Families

A `derives` clause on a sealed trait covers the whole family. The case classes need no clause of their own:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step4SealedTraitExample.scala", "sealed-trait")
```

Each case is wrapped in an object named after it:

```text
Circle encoded: {"Circle":{"radius":1.5}}
Rectangle decoded: Right(Rectangle(2.0,3.0))
```

When a sealed trait extends another one, the codec of the outer trait sees only the leaves, the case classes and case objects at the bottom. The intermediate trait `Motorized` gets no hint of its own:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step4SealedTraitExample.scala", "nested")
```

A `Car` is written with its own name, and a hint that names `Motorized` is rejected:

```text
Car encoded: {"Car":{"seats":4}}
Bicycle decoded: Right(Bicycle(21))
Motorized hint: Left((invalid disambiguator))
```

Because all leaves share one set of names, two leaves in different branches must not have the same hint. [Sealed traits](sealed-traits.md) covers hints, discriminators and this flattening, which works the same way on Scala 2.

## 5. Use String-Literal Union Types

A union of string literal types, such as `"EUR" | "USD" | "GBP"`, has a built-in encoder and decoder on Scala 3. It is written as a plain JSON string, so it is a lightweight alternative to an enum when the values are fixed strings:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step5UnionTypesExample.scala", "union")
```

The decoder accepts only the listed strings and names them in the error:

```text
Price encoded: {"amount":9.99,"currency":"EUR"}
Price decoded: Right(Price(5,USD))
Price bad value: Left(.currency(expected one of: EUR, USD, GBP))
```

The same union also works as a `Map` key, with no key instances to write:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step5UnionTypesExample.scala", "union-keys")
```

Keys outside the union are rejected with the same message, at the path of the key:

```text
Rates encoded: {"base":"EUR","rates":{"USD":1.08}}
Rates bad key: Left(.rates.JPY(expected one of: EUR, USD, GBP))
```

A union value can also be encoded and decoded on its own:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step5UnionTypesExample.scala", "union-alone")
```

Both directions use the plain string:

```text
Currency encoded: "GBP"
Currency decoded: Right(USD)
```

Only unions of string literals are supported. A union has a `JsonEncoder` and a `JsonDecoder`, but no `JsonCodec`, so ask for the encoder or the decoder when you need one directly, for example `summon[JsonDecoder[Currency]]`. A derived codec of a case class with a union field works, because derivation looks up the encoder and decoder of each field separately. [Use Your Own Type as a Map Key](custom-map-keys.md#scala-3-string-subtypes-need-no-key-decoder) explains why union keys need no key instances.

## 6. Derive Codecs for Generic Case Classes

A `derives` clause works on a type-parameterised case class:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step6GenericExample.scala", "derives-generic")
```

The element type can be any type with a codec, including a collection:

```text
Box[Int] encoded: {"value":1}
Box[List[String]] decoded: Right(Box(List(a, b)))
```

The given that `derives JsonCodec` generates asks for a `JsonCodec[A]` for every type parameter. A type that has a separate `JsonEncoder` and `JsonDecoder` but no `JsonCodec` does not satisfy it. For such types, write the given yourself and ask for the encoder and the decoder:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step6GenericExample.scala", "given-generic")
```

`Sku` has only separate instances:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step6GenericExample.scala", "separate-instances")
```

`Envelope[Sku]` works:

```text
Envelope[Sku] encoded: {"id":"e1","payload":{"code":"A-1"}}
Envelope[Sku] decoded: Right(Envelope(e1,Sku(A-1)))
```

`Box(Sku("A-1")).toJson`, on the other hand, does not compile. The message mentions an unrelated union-type instance that the compiler tried last:

```text
No given instance of type zio.json.JsonEncoder[Box[Sku]] was found for parameter encoder of method toJson in class EncoderOps.
I found:

    zio.json.JsonEncoder.unionOfStringEnumeration[Box[Sku]](???)

But macro expansion was stopped.
```

[Derive Codecs for Generic Types](generic-types.md) covers generic sealed traits, recursive generic types and the Scala 2 form.

## 7. Encode Opaque Types

An opaque type has no codec of its own, and derivation cannot look through it: outside the scope that defines it, the compiler does not know that `UserId` is a `String`. A case class with an opaque field fails to derive until you provide a codec. Write one in the companion of the opaque type, where the underlying type is visible, by transforming the codec of the underlying type with `JsonCodec#transform`:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step7OpaqueTypeExample.scala", "opaque")
```

The opaque value is written as the bare string:

```text
User encoded: {"id":"u-1","name":"Ada"}
User decoded: Right(User(u-2,Grace))
```

When not every value of the underlying type is valid, use `JsonCodec#transformOrFail`, whose first function returns an `Either`. The `Left` message becomes the decoding error:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step7OpaqueTypeExample.scala", "validated")
```

A zero quantity is rejected while decoding:

```text
OrderLine decoded: Right(OrderLine(A1,3))
OrderLine invalid: Left(.quantity(quantity must be positive, got 0))
```

An opaque type declared with an upper bound, such as `opaque type Sku <: String = String`, can also be a `Map` key once it has a decoder. [Use Your Own Type as a Map Key](custom-map-keys.md#scala-3-string-subtypes-need-no-key-decoder) shows it. [Validate data while decoding](validate-on-decode.md) has more ways to reject bad values.

## 8. Read the Compile Errors

Derivation runs at compile time, so a type it cannot handle is a compile error, not a runtime failure. On Scala 3, a field type that has no instance is derived automatically when the compiler can see its structure (a case class, a sealed trait or an enum). When neither an instance nor a derivation is possible, derivation stops with `Cannot find or derive`. `Upload` has a field of type `java.io.File`, which has no codec and is not a case class:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step8CompileErrorsExample.scala", "missing-instance")
```

`Account` has a field whose type is opaque and has no codec:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/scala3derivation/Step8CompileErrorsExample.scala", "missing-opaque")
```

Deriving a codec for `Upload` or an encoder for `Account` fails with these messages:

```text
Cannot find or derive JsonDecoder for java.io.File
Cannot find or derive JsonEncoder for zio.json.docs.scala3derivation.Step8CompileErrorsExample.ids.AccountId
```

The message names the field type, not the type you derive. For a `JsonCodec`, it can name either side: here the decoder is reported first. The compiler adds an "Inline stack trace" section below the message, which points at the `derives` clause. To fix the error, give the named type its own codec, for example with `JsonCodec#transform` as in the opaque-type section, or change the field to a type that has one. [Built-in Instances](../reference/built-in-instances.md) lists the types that already have one.

## See Also

- [Scala 2 and Scala 3 differences](../explanation/scala-2-vs-scala-3.md) lists every behaviour that changes between the two versions, with a migration checklist.
- [JsonCodecConfiguration](../reference/json-codec-configuration.md) describes every configuration option.
- [Sealed traits](sealed-traits.md) covers type hints, discriminators and enums as strings.
- [Derive Codecs for Generic Types](generic-types.md) covers type parameters on Scala 2 and Scala 3.
- [zio-json-macros](../reference/zio-json-macros.md) describes `@jsonDerive`, the Scala 2 annotation that `derives` replaces.
