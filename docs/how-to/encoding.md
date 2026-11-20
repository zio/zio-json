---
id: encoding
title: "Control the JSON a Type Encodes To"
description: "Get the exact JSON shape you need from a type: adjust a derived encoder, encode through another type with contramap, and tag a sealed trait."
keywords:
  - "JSON Encoding"
  - "JSON Shape"
  - "JsonEncoder"
  - "contramap"
  - "Discriminator Field"
---

Goal: make one of your types encode to the exact JSON shape you need, such as the shape another service expects.

A derived encoder writes a case class as an object with one key per field, and a sealed trait as an object with one key that names the subtype (see the [Getting Started](../getting-started.md) tutorial). When the JSON you need differs from that, pick the step that matches the difference:

| The JSON you need                                         | Step                                                                       |
|-----------------------------------------------------------|----------------------------------------------------------------------------|
| Other field names, a field left out, `null` for `None`    | [1](#1-rename-drop-or-null-out-fields-with-annotations)                    |
| A wrapper type written as the value it wraps              | [2](#2-write-a-wrapper-type-as-its-bare-value)                             |
| Extra computed fields, or an array instead of an object   | [3](#3-write-a-different-shape-through-an-intermediate-type)               |
| A sealed trait tagged by a field such as `"type"`         | [4](#4-tag-a-sealed-trait-with-a-type-field)                               |
| One naming rule for every type in the program             | [5](#5-apply-one-format-to-every-type)                                     |

Each step below is a separate option; use the ones you need. The steps derive only a `JsonEncoder`, because this page is about writing JSON. Derive a `JsonCodec` instead when you also read the same JSON back.

## What You Need

Only the core module:

```scala
// ZIO JSON: JSON encoders, decoders and codecs (JVM, Scala.js, Scala Native)
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

Every example on this page uses one import:

```scala mdoc:silent:reset-object
import zio.json._
```

## 1. Rename, Drop or Null Out Fields with Annotations

When the shape is right but the details are not, keep the derived encoder and annotate the case class fields. `@jsonField` renames a field, `@jsonExclude` leaves it out, and `@jsonExplicitNull` writes `null` for a `None` instead of omitting the field:

```scala mdoc:silent
final case class Account(
  @jsonField("user_name") userName: String,
  @jsonExclude password: String,
  @jsonExplicitNull nickname: Option[String]
)

object Account {
  implicit val encoder: JsonEncoder[Account] = DeriveJsonEncoder.gen[Account]
}
```

The password is gone, the name uses the new key, and the empty nickname is written as `null`:

```scala mdoc
println(Account("ada", "secret", None).toJson)
```

[Field Names](./field-names.md) covers renaming every field with one rule, and [Optional, Default and Null Fields](./optional-and-default-fields.md) covers `null`, empty collections and excluded fields, including what they mean for decoding. The [Annotations reference](../reference/annotations.md) lists every annotation.

## 2. Write a Wrapper Type as Its Bare Value

A case class that wraps one value, such as an `OrderId` around a `String`, derives to an object like `{"value":"A-17"}`. To write just the string, build the encoder from the `String` encoder with `JsonEncoder#contramap`, which takes a function from your type to a type that already has an encoder:

```scala mdoc:silent
final case class OrderId(value: String)

object OrderId {
  implicit val encoder: JsonEncoder[OrderId] = JsonEncoder[String].contramap(_.value)
}
```

The id is now a plain JSON string:

```scala mdoc
println(OrderId("A-17").toJson)
```

[Build an Encoder or Decoder from an Existing One](./manual-instances.md) adds the matching decoder and bundles both into a codec.

## 3. Write a Different Shape Through an Intermediate Type

When the JSON has a different structure from your type, for example a computed `total` that your `Order` does not store, define a case class with exactly the JSON's structure, derive its encoder, and build your type's encoder from it with `JsonEncoder#contramap`. An order line is an ordinary derived object:

```scala mdoc:silent
final case class OrderLine(sku: String, price: BigDecimal)

object OrderLine {
  implicit val encoder: JsonEncoder[OrderLine] = DeriveJsonEncoder.gen[OrderLine]
}
```

`OrderJson` describes the JSON, and the `Order` encoder converts each order into it:

```scala mdoc:silent
final case class OrderJson(id: OrderId, lines: List[OrderLine], total: BigDecimal)

object OrderJson {
  implicit val encoder: JsonEncoder[OrderJson] = DeriveJsonEncoder.gen[OrderJson]
}

final case class Order(id: OrderId, lines: List[OrderLine])

object Order {
  implicit val encoder: JsonEncoder[Order] =
    JsonEncoder[OrderJson].contramap(o => OrderJson(o.id, o.lines, o.lines.map(_.price).sum))
}
```

The `Order` is written with the computed total, and its `id` uses the bare-string encoder from step 2:

```scala mdoc
val order = Order(OrderId("A-17"), List(OrderLine("pen", BigDecimal("1.50")), OrderLine("ink", BigDecimal("4.25"))))

println(order.toJson)
```

The intermediate type can also be a tuple. Tuples are written as JSON arrays, so going through a tuple writes a case class as an array:

```scala mdoc:silent
final case class Point(x: Double, y: Double)

object Point {
  implicit val encoder: JsonEncoder[Point] = JsonEncoder[(Double, Double)].contramap(p => (p.x, p.y))
}
```

The point is written as a two-element array:

```scala mdoc
println(Point(1.0, 2.5).toJson)
```

## 4. Tag a Sealed Trait with a Type Field

Many APIs put the subtype name in a field of the object, such as `{"type":"card",...}`, instead of the default wrapper object. Put `@jsonDiscriminator` on the sealed trait to name that field, and `@jsonHint` on a subtype to change the name written for it:

```scala mdoc:silent
@jsonDiscriminator("type")
sealed trait Payment

object Payment {
  @jsonHint("card") final case class Card(last4: String)            extends Payment
  @jsonHint("bank_transfer") final case class BankTransfer(iban: String) extends Payment

  implicit val encoder: JsonEncoder[Payment] = DeriveJsonEncoder.gen[Payment]
}
```

Each payment carries its tag in the `type` field:

```scala mdoc
val payments: List[Payment] = List(Payment.Card("4242"), Payment.BankTransfer("DE89"))

println(payments.toJson)
```

The encoder is for `Payment`, so encode values typed as `Payment`. On both Scala 2 and Scala 3, `Payment.Card("4242").toJson` does not compile, because the compiler then looks for a `JsonEncoder[Payment.Card]`. Write `(Payment.Card("4242"): Payment).toJson` instead, or derive an encoder for the subtype as well if you want it written without the tag.

[Sealed Traits](./sealed-traits.md) covers the other choices for a sealed family: renaming all hints with one rule, writing case objects as plain strings, and the cost of a discriminator field.

## 5. Apply One Format to Every Type

To use the same rule for every derived encoder instead of annotating each class, put an implicit `JsonCodecConfiguration` in scope where the encoders are derived. Here `fieldNameMapping` writes every field name in snake_case:

```scala mdoc:silent:reset-object
import zio.json._

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(fieldNameMapping = SnakeCase)

final case class Shipment(trackingCode: String, weightGrams: Int)

object Shipment {
  implicit val encoder: JsonEncoder[Shipment] = DeriveJsonEncoder.gen[Shipment]
}
```

The field names follow the configuration without any annotation on `Shipment`:

```scala mdoc
println(Shipment("X1", 250).toJson)
```

Annotations such as `@jsonField` and `@jsonMemberNames` take priority over the configuration. [JsonCodecConfiguration](../reference/json-codec-configuration.md) lists every option, including `sumTypeHandling` for a program-wide discriminator field.

## See Also

- [Build an Encoder or Decoder from an Existing One](./manual-instances.md) builds both directions from existing instances.
- [Encode a Type You Do Not Own](./third-party-types.md) applies `JsonEncoder#contramap` to a class from another library.
- [Write a Fast Encoder or Decoder by Hand](./low-level-codecs.md) for a shape that no existing encoder can produce.
- [Golden Testing](./golden-testing.md) catches unintended changes to the JSON your types encode to.
- [JsonEncoder reference](../reference/json-encoder.md) covers every encoder method, including `toJsonPretty` and `toJsonAST`.
