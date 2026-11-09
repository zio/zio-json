---
id: sealed-traits
title: "Sealed traits"
description: "Control how ZIO JSON encodes sealed traits: type hints, hint name formats, a discriminator field, and enums as strings."
keywords:
  - "Sealed Trait Encoding"
  - "Type Hints"
  - "Discriminator Field"
  - "Enum Values As Strings"
---

Goal: change how a sealed trait and its subtypes look in JSON.

By default, a value of a sealed family is written as an object with one key, the short class name, whose value is the encoded subtype:

```json
{"Banana":{"curvature":0.5}}
```

This page shows how to change the key (the type hint), how to use a discriminator field instead, and how case objects can become plain strings.

## What you need

```scala mdoc:silent
import zio.json._
```

## 1. Change a type hint with `@jsonHint`

Put `@jsonHint` on a subtype to choose its key:

```scala mdoc:silent
sealed trait Fruit

@jsonHint("banaani") case class Banana(curvature: Double) extends Fruit
@jsonHint("omena") case class Apple(poison: Boolean)      extends Fruit

object Fruit {
  implicit val codec: JsonCodec[Fruit] = DeriveJsonCodec.gen[Fruit]
}
```

The keys are now `banaani` and `omena`:

```scala mdoc
println((Banana(0.5): Fruit).toJson)
println((Apple(false): Fruit).toJson)
```

## 2. Transform all hints with `@jsonHintNames`

`@jsonHintNames` goes on the sealed trait and applies a `JsonMemberFormat` to every hint in the family. It accepts the same formats as `@jsonMemberNames`, which are listed in [Field names](field-names.md). A `@jsonHint` on a subtype wins over the format.

```scala mdoc:silent
@jsonHintNames(SnakeCase)
sealed trait FruitKind

case class GoodFruit(good: Boolean) extends FruitKind
case class BadFruit(bad: Boolean)   extends FruitKind

object FruitKind {
  implicit val codec: JsonCodec[FruitKind] = DeriveJsonCodec.gen[FruitKind]
}
```

```scala mdoc
println((GoodFruit(true): FruitKind).toJson)
println((BadFruit(true): FruitKind).toJson)
```

The codec of `FruitKind` expects the hint. You cannot decode a bare `{"good":true}` with it, and you cannot encode a `GoodFruit` on its own with it either.

### Decode a single subtype

To read one subtype without a hint, give that subtype its own codec in its companion. A case class does not need a discriminator to be decoded on its own:

```scala mdoc:silent:reset
import zio.json._

@jsonHintNames(SnakeCase)
sealed trait Nut

case class Almond(salted: Boolean) extends Nut
case class Cashew(roasted: Boolean) extends Nut

object Almond {
  implicit val codec: JsonCodec[Almond] = DeriveJsonCodec.gen[Almond]
}

object Nut {
  implicit val codec: JsonCodec[Nut] = DeriveJsonCodec.gen[Nut]
}
```

```scala mdoc
"""{"salted":true}""".fromJson[Almond]
```

To accept a subtype only when it arrives inside the sealed family, transform the codec of the family instead of deriving a new one. Keep this decoder out of implicit scope, or the codec of `Nut` would pick it up and call itself:

```scala mdoc:silent
val cashewDecoder: JsonDecoder[Cashew] =
  Nut.codec.decoder.mapOrFail {
    case cashew: Cashew => Right(cashew)
    case other          => Left(s"Expected Cashew, got $other")
  }
```

```scala mdoc
"""{"cashew":{"roasted":true}}""".fromJson[Cashew](cashewDecoder)
"""{"almond":{"salted":true}}""".fromJson[Cashew](cashewDecoder)
```

## 3. Use a discriminator field with `@jsonDiscriminator`

Some schemas put the type name inside the object:

```json
{"type":"banaani","curvature":0.5}
```

Use `@jsonDiscriminator` on the sealed trait to get this shape. The value of the field is still the hint, so `@jsonHint` and `@jsonHintNames` work as before.

```scala mdoc:silent
@jsonDiscriminator("type")
sealed trait Tropical

@jsonHint("banaani") case class TropicalBanana(curvature: Double) extends Tropical
@jsonHint("omena") case class TropicalApple(poison: Boolean)      extends Tropical

object Tropical {
  implicit val codec: JsonCodec[Tropical] = DeriveJsonCodec.gen[Tropical]
}
```

```scala mdoc
println((TropicalBanana(0.5): Tropical).toJson)
"""{"type":"omena","poison":false}""".fromJson[Tropical]
```

:::warning[Use it only when you must]
A discriminator is slower, uses more memory, and is more exposed to denial-of-service input than the default wrapper object. Use it only to match a schema that you do not control. Do not combine it with `@jsonNoExtraFields`, because the discriminator counts as an extra field of the subtype.
:::

## 4. Set it for the whole program

The configuration form of the same options is `JsonCodecConfiguration`. Use `SumTypeHandling.DiscriminatorField` for `sumTypeHandling`, and `sumTypeMapping` for the hint format:

```scala mdoc:silent:reset
import zio.json._
import zio.json.JsonCodecConfiguration.SumTypeHandling

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(
    sumTypeHandling = SumTypeHandling.DiscriminatorField("type"),
    sumTypeMapping = SnakeCase
  )

sealed trait Berry
case class BlueBerry(sweet: Boolean) extends Berry

object Berry {
  implicit val codec: JsonCodec[Berry] = DeriveJsonCodec.gen[Berry]
}
```

```scala mdoc
println((BlueBerry(true): Berry).toJson)
```

An annotation on the sealed trait takes priority over the configuration. The default is `SumTypeHandling.WrapperWithClassNameField`, the single-key object.

## 5. Write case objects as strings

A sealed family made only of case objects can be written as a plain JSON string:

```json
"Red"
```

Whether this happens depends on the `enumValuesAsStrings` setting of `JsonCodecConfiguration`:

| Scala version | Default of `enumValuesAsStrings` |
|---------------|----------------------------------|
| Scala 2       | `false`                          |
| Scala 3       | `true`                           |

The setting has no effect when a discriminator is used. Hints still apply, so `@jsonHint("crimson")` on a case object changes its string.

On Scala 3, a sealed family of case objects and an `enum` with only parameterless cases are both written as strings without any setup.

On Scala 2 the default is the single-key object, `{"Red":{}}`. Turn the setting on with an implicit configuration:

```scala mdoc:silent:reset
import zio.json._

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(enumValuesAsStrings = true)

sealed trait Color
case object Red   extends Color
case object Green extends Color

object Color {
  implicit val codec: JsonCodec[Color] = DeriveJsonCodec.gen[Color]
}
```

```scala mdoc
println((Red: Color).toJson)
""""Green"""".fromJson[Color]
```

:::note[Scala 2 caveat]
On Scala 2 the string form is off unless you set `enumValuesAsStrings = true`. The enum sections of [Encoding](encoding.md) and [Decoding](decoding.md) describe the Scala 3 behavior. Turn the setting on yourself on Scala 2, or the JSON will differ between the two Scala versions.
:::

## See also

- [Annotations](../reference/annotations.md) lists every derivation annotation in one table.
- [Field names](field-names.md) covers `@jsonMemberNames`, whose formats `@jsonHintNames` reuses.
- [Optional, default and null fields](optional-and-default-fields.md) covers `@jsonNoExtraFields`.
