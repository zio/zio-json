---
id: encoding
title: "Encoding"
---

## Automatic Derivation

Assume we want to encode this case class

```scala mdoc:reset-object
case class Banana(curvature: Double)
```

To produce JSON from our data we define a `JsonEncoder` like this:

```scala mdoc
import zio.json._

object Banana {
  implicit val encoder: JsonEncoder[Banana] =
    DeriveJsonEncoder.gen[Banana]
}
```

```scala mdoc
Banana(0.5).toJson
```

### ADTs

Say we extend our data model to include more data types

```scala mdoc:reset-object
sealed trait Fruit

case class Banana(curvature: Double) extends Fruit
case class Apple (poison: Boolean)   extends Fruit
```

we can generate the encoder for the entire `sealed` family

```scala mdoc
import zio.json._

object Fruit {
  implicit val encoder: JsonEncoder[Fruit] =
    DeriveJsonEncoder.gen[Fruit]
}
```

```scala mdoc
val apple: Fruit = Apple(poison = false)
apple.toJson
```

Almost all of the standard library data types are supported as fields on the case class (see [Built-in Instances](../reference/built-in-instances.md) for the full list), and it is easy to add support if one is missing.

For encoding sealed families and enums of parameterless cases as plain strings, see [Sealed Traits](./sealed-traits.md).

## Manual instances

Sometimes it is easier to reuse an existing `JsonEncoder` rather than generate a new one. This can be accomplished using convenience methods on the `JsonEncoder` typeclass to *derive* new encoders:

```scala
trait JsonEncoder[A] {
  def contramap[B](f: B => A): JsonEncoder[B]
  ...
}
```

### `JsonEncoder#contramap`

We can use `JsonEncoder#contramap` on an already existing encoder:

```scala mdoc
import zio.json._

case class FruitCount(value: Int)

object FruitCount {
  implicit val encoder: JsonEncoder[FruitCount] =
    JsonEncoder[Int].contramap(_.value)
}

FruitCount(3).toJson
```

[Manual Instances](./manual-instances.md) shows more of these patterns, and [Encode a type you do not own](./third-party-types.md) applies them to a class from another library.

## See Also

- [JsonEncoder reference](../reference/json-encoder.md) covers every encoder method, including `toJsonPretty` and hand-written encoders.
- [Field Names](./field-names.md) and [Optional, default and null fields](./optional-and-default-fields.md) change how derived encoders write fields.
- [JsonCodecConfiguration](../reference/json-codec-configuration.md) sets derivation options for a whole program.
