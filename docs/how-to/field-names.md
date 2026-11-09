---
id: field-names
title: "Field names"
description: "Rename JSON fields with jsonField, rename all fields with jsonMemberNames, and read old names with jsonAliases in ZIO JSON."
keywords:
  - "JSON Field Names"
  - "Member Name Transformation"
  - "Field Aliases"
  - "jsonMemberNames"
---

Goal: make the JSON field names differ from the Scala field names of a case class.

By default, the JSON field names are the case class field names. This page shows three ways to change that:

- `@jsonField` renames one field.
- `@jsonMemberNames` renames every field with one rule.
- `@jsonAliases` accepts extra names when decoding.

## What you need

Only `zio-json` and one import:

```scala mdoc:silent
import zio.json._
```

## 1. Rename one field with `@jsonField`

Put `@jsonField` on a field and give it the JSON name:

```scala mdoc:silent
case class Banana(@jsonField("bendiness") curvature: Double)

object Banana {
  implicit val codec: JsonCodec[Banana] = DeriveJsonCodec.gen[Banana]
}
```

The field is written and read as `bendiness`:

```scala mdoc
println(Banana(0.5).toJson)
"""{"bendiness":0.5}""".fromJson[Banana]
```

## 2. Rename all fields with `@jsonMemberNames`

Put `@jsonMemberNames` on a case class. It takes a `JsonMemberFormat`, and the format is applied to every field name, for both encoding and decoding.

These formats are available:

| Format           | What it does                             |
|------------------|------------------------------------------|
| `SnakeCase`      | `passionFruit` becomes `passion_fruit`   |
| `KebabCase`      | `passionFruit` becomes `passion-fruit`   |
| `PascalCase`     | `passionFruit` becomes `PassionFruit`    |
| `CamelCase`      | `passion_fruit` becomes `passionFruit`   |
| `IdentityFormat` | Leaves the name as it is                 |
| `CustomCase(f)`  | Applies your own `String => String`      |

A `JsonMemberFormat` is itself a `String => String`, so you can call it to see what it does:

```scala mdoc
List[(String, JsonMemberFormat)](
  "SnakeCase"      -> SnakeCase,
  "KebabCase"      -> KebabCase,
  "PascalCase"     -> PascalCase,
  "CamelCase"      -> CamelCase,
  "IdentityFormat" -> IdentityFormat,
  "CustomCase"     -> CustomCase(_.toUpperCase)
).map { case (name, format) => name -> format("passionFruit") }
```

Here is a basket where most fields are `snake_case` in JSON:

```scala mdoc:silent
@jsonMemberNames(SnakeCase)
case class FruitBasket(
  passionFruit: Boolean,
  grannySmith: Boolean,
  dragonFruit: Boolean,
  @jsonField("blood-orange") bloodOrange: Boolean
)

object FruitBasket {
  implicit val codec: JsonCodec[FruitBasket] = DeriveJsonCodec.gen[FruitBasket]
}
```

Three fields follow the format. `bloodOrange` has its own `@jsonField`, which takes priority over `@jsonMemberNames`:

```scala mdoc
println(FruitBasket(true, true, true, false).toJson)
```

The same names are expected when decoding:

```scala mdoc
"""{"passion_fruit":true,"granny_smith":true,"dragon_fruit":true,"blood-orange":false}""".fromJson[FruitBasket]
```

## 3. Use a custom rule

`CustomCase` wraps any `String => String`. Define the function as a normal value, then pass it to the format:

```scala mdoc:silent
object Rules {
  val shout: String => String = _.toUpperCase
}

@jsonMemberNames(CustomCase(Rules.shout))
case class Lemon(sourness: Int)

object Lemon {
  implicit val codec: JsonCodec[Lemon] = DeriveJsonCodec.gen[Lemon]
}
```

```scala mdoc
println(Lemon(7).toJson)
```

## 4. Set the format for the whole program

Instead of annotating each class, set `fieldNameMapping` in an implicit `JsonCodecConfiguration`. It applies to every codec derived while the configuration is in scope. The `@jsonMemberNames` annotation on a class takes priority over it.

```scala mdoc:compile-only
implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(fieldNameMapping = SnakeCase)

case class Cherry(sweetnessLevel: Int)

implicit val cherryCodec: JsonCodec[Cherry] = DeriveJsonCodec.gen[Cherry]
```

## 5. Read old names with `@jsonAliases`

After a field has been renamed, you may still receive JSON that uses the old name. `@jsonAliases` lists extra names that the decoder accepts for that field. It was added in zio-json 0.4.3.

```scala mdoc:silent
case class Strawberry(@jsonAliases("seeds", "pips") seedCount: Int)

object Strawberry {
  implicit val decoder: JsonDecoder[Strawberry] = DeriveJsonDecoder.gen[Strawberry]
}
```

The old names and the new name decode to equal values:

```scala mdoc
"""{"seeds":32}""".fromJson[Strawberry]
"""{"pips":32}""".fromJson[Strawberry]
"""{"seedCount":32}""".fromJson[Strawberry]
```

Keep these points in mind:

- The annotation has no effect on encoding. The encoder always writes the main name.
- An alias is matched exactly as you wrote it. A `@jsonMemberNames` format is not applied to aliases.
- A field name and its aliases must not collide with another field or alias of the same class. On Scala 2, deriving the decoder fails with an `AssertionError` if they do.

## See also

- [Annotations](../reference/annotations.md) lists every derivation annotation in one table.
- [Sealed traits](sealed-traits.md) shows how to rename the type hints of a sealed family.
- [Encoding](encoding.md) and [Decoding](decoding.md) cover the basics of derived codecs.
