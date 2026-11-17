---
id: index
title: "Derivation"
description: "How ZIO JSON derives codecs for case classes and sealed traits, how to start derivation on Scala 2 and Scala 3, and the three ways to change what it produces: annotations, JsonCodecConfiguration and @jsonDerive."
keywords:
  - "Codec Derivation"
  - "DeriveJsonCodec"
  - "derives JsonCodec"
  - "JsonCodecConfiguration"
  - "Annotations"
---

*Derivation* is how ZIO JSON writes a `JsonEncoder`, `JsonDecoder` or `JsonCodec` for your own case classes and sealed traits at compile time, so you don't write them by hand. This section is the reference for everything that controls it. The pages in it are:

- [JsonCodecConfiguration](../json-codec-configuration.md): the options that apply to every type derived where the configuration is in scope.
- [Annotations](../annotations.md): the annotations that change one type or one field.
- [@jsonDerive Macro Annotation](../zio-json-macros.md): a Scala 2 macro annotation that writes the derived codec into the companion object for you.

## Starting Derivation

On Scala 2 and Scala 3, call `gen` on one of the three derivation objects, usually in the companion object so the compiler finds the instance without an import:

| Call                     | Produces         |
|--------------------------|------------------|
| `DeriveJsonEncoder.gen`  | `JsonEncoder[A]` |
| `DeriveJsonDecoder.gen`  | `JsonDecoder[A]` |
| `DeriveJsonCodec.gen`    | `JsonCodec[A]`   |

```scala mdoc:reset-object
import zio.json._

final case class Person(name: String, age: Int)

object Person {
  implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
}

println(Person("Ada", 36).toJson)
```

On Scala 3 you can write a `derives` clause instead. It calls `JsonCodec.derived` (or `JsonEncoder.derived`, `JsonDecoder.derived`):

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/jsoncodec/Scala3CodecExample.scala", "derives")
```

Derivation needs an instance for the parts of the type. For a sealed trait, the subtypes that have no instance of their own are derived together with the trait on both Scala versions. For a field whose type has no instance, Scala 3 derives one, while Scala 2 stops with a compile error, so give that type its own codec first. [Scala 2 and Scala 3 Differences](../../explanation/scala-2-vs-scala-3.md) lists every behaviour that differs between the two.

## Changing What Derivation Produces

There are three tools, and each works at a different scope:

| Tool                                                  | Scope                                                   | Example                                               |
|-------------------------------------------------------|---------------------------------------------------------|-------------------------------------------------------|
| [Annotations](../annotations.md)                      | One type or one field                                   | `@jsonField("user_id")`, `@jsonDiscriminator("type")` |
| [JsonCodecConfiguration](../json-codec-configuration.md) | Every type derived where the implicit or `given` is in scope | `JsonCodecConfiguration(fieldNameMapping = SnakeCase)` |
| [`@jsonDerive`](../zio-json-macros.md)                | Writes the derivation call for you (Scala 2 only)      | `@jsonDerive case class Person(...)`                  |

A configuration in scope sets the default for every derived type, and an annotation on a type or field takes priority over it. Here the configuration turns every field name into `snake_case`, and `@jsonField` still decides the name of one field:

```scala mdoc:reset-object
import zio.json._

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(fieldNameMapping = SnakeCase)

final case class Account(accountId: Int, @jsonField("name") displayName: String)

object Account {
  implicit val codec: JsonCodec[Account] = DeriveJsonCodec.gen[Account]
}

println(Account(1, "Ada").toJson)
```

On Scala 3 the configuration is a `given`:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/jsoncodec/Scala3CodecExample.scala", "config")
```

When no configuration is in scope, derivation uses `JsonCodecConfiguration.default`, so `derives` and `gen` never need one.

## See Also

- [Rename Fields](../../how-to/field-names.md), [Sealed Traits](../../how-to/sealed-traits.md) and [Optional, Default and Null Fields](../../how-to/optional-and-default-fields.md) for the common changes, step by step
- [Derive Codecs on Scala 3](../../how-to/scala-3-derivation.md) and [Derive Codecs for Generic Types](../../how-to/generic-types.md) for the cases derivation needs help with
- [JSON Shapes and Round Trips](../../explanation/wire-format.md) for how each kind of type is written under each setting
