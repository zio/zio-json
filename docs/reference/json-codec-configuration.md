---
id: json-codec-configuration
title: "JsonCodecConfiguration"
description: "Reference for JsonCodecConfiguration: every codec derivation option, its type and default, and the Scala 2 and Scala 3 differences."
keywords:
  - "Codec Derivation"
  - "Codec Configuration"
  - "Sum Type Handling"
  - "JsonCodecConfiguration"
---

`JsonCodecConfiguration` is a case class that sets the defaults for derived codecs.
`DeriveJsonCodec.gen`, `DeriveJsonEncoder.gen` and `DeriveJsonDecoder.gen` read it when they derive an instance.
Annotations on a single type, such as `@jsonDiscriminator` or `@jsonMemberNames`, override the configuration for that type.

## Options

All options have defaults, so you only name the ones you want to change.

| Option                     | Type                       | Scala 2 default              | Scala 3 default              | What it controls                                   |
|----------------------------|----------------------------|------------------------------|------------------------------|----------------------------------------------------|
| `sumTypeHandling`          | `SumTypeHandling`          | `WrapperWithClassNameField`  | `WrapperWithClassNameField`  | How a sealed trait case is tagged                  |
| `fieldNameMapping`         | `JsonMemberFormat`         | `IdentityFormat`             | `IdentityFormat`             | How case class field names become JSON keys        |
| `allowExtraFields`         | `Boolean`                  | `true`                       | `true`                       | Whether unknown keys are accepted on decode        |
| `sumTypeMapping`           | `JsonMemberFormat`         | `IdentityFormat`             | `IdentityFormat`             | How case names become JSON tags                    |
| `explicitNulls`            | `Boolean`                  | `false`                      | `false`                      | Whether `None` is written as `null`                |
| `explicitEmptyCollections` | `ExplicitEmptyCollections` | `ExplicitEmptyCollections()` | `ExplicitEmptyCollections()` | Whether empty collections are written and required |
| `enumValuesAsStrings`      | `Boolean`                  | `false`                      | `true`                       | Whether case objects are plain JSON strings        |

The only difference between the two Scala versions is the default of `enumValuesAsStrings`.

## Passing a custom configuration

The derivation macros take the configuration as an implicit parameter.
The library provides `JsonCodecConfiguration.default` in the companion object.
To use your own, put an implicit value in scope before the `gen` call.
A value in scope wins over the companion default.

```scala mdoc:silent:reset
import zio.json._

case class User(firstName: String, lastName: String)

object User {
  implicit val config: JsonCodecConfiguration =
    JsonCodecConfiguration(fieldNameMapping = SnakeCase)

  implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen[User]
}
```

```scala mdoc
User("Ada", "Lovelace").toJson

"""{"first_name":"Ada","last_name":"Lovelace"}""".fromJson[User]
```

You can also start from the default and change one option with `copy`.

```scala mdoc:compile-only
import zio.json._

val strict: JsonCodecConfiguration =
  JsonCodecConfiguration.default.copy(allowExtraFields = false)
```

In Scala 3, supply it as a `given`. The macros take it as a `using` parameter. This also applies to `derives JsonCodec`.

```scala
// Scala 3 only
import zio.json.*

given JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)

case class User(firstName: String, lastName: String) derives JsonCodec
```

The configuration is read at derivation time.
Changing it afterwards has no effect on codecs that already exist.
Nested types are derived with the configuration in scope at the outer `gen` call.

## sumTypeHandling

Type: `SumTypeHandling`. Two values exist.

- `WrapperWithClassNameField` (the default) wraps the case in an object with one key, the case name.
- `DiscriminatorField(name)` writes the case name into a field called `name` inside the case object itself.

`@jsonDiscriminator("name")` on a sealed trait does the same for that trait only.
The scaladoc warns that a discriminator is slower, uses more memory, and is open to denial of service attacks that the wrapper form is not.
Use it only to match an external schema.

```scala mdoc:silent:reset
import zio.json._
import zio.json.JsonCodecConfiguration.SumTypeHandling.DiscriminatorField

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Square(side: Double)   extends Shape

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("type"))

implicit val codec: JsonCodec[Shape] = DeriveJsonCodec.gen[Shape]
```

```scala mdoc
(Circle(1.0): Shape).toJson

"""{"type":"Square","side":2.0}""".fromJson[Shape]
```

With the default, the same value encodes as `{"Circle":{"radius":1.0}}`.
See [sealed traits](../how-to/sealed-traits.md) for the how-to side.

## fieldNameMapping

Type: `JsonMemberFormat`. It rewrites every case class field name.
`@jsonMemberNames` overrides it per type, and `@jsonField` overrides it per field.
The available formats are `IdentityFormat`, `SnakeCase`, `KebabCase`, `CamelCase`, `PascalCase`, and `CustomCase(f: String => String)`.
The `ziojson_03` object holds the `SnakeCase` and `KebabCase` variants from zio-json 0.3.0, which put a separator before digits.
The example under [Passing a custom configuration](#passing-a-custom-configuration) uses `SnakeCase`.
See [field names](../how-to/field-names.md) for the how-to side.

## allowExtraFields

Type: `Boolean`. When `false`, decoding fails if the JSON has a key that the case class does not declare.
`@jsonNoExtraFields` sets it per type.

```scala mdoc:silent:reset
import zio.json._

case class Point(x: Int, y: Int)

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(allowExtraFields = false)

implicit val codec: JsonCodec[Point] = DeriveJsonCodec.gen[Point]
```

```scala mdoc
"""{"x":1,"y":2,"z":3}""".fromJson[Point]
```

## sumTypeMapping

Type: `JsonMemberFormat`. It rewrites the case names of a sealed trait, for both the wrapper key and the discriminator value.
`@jsonHintNames` overrides it per trait, and `@jsonHint` overrides it per case.

```scala mdoc:silent:reset
import zio.json._

sealed trait Shape
case class RoundCircle(radius: Double) extends Shape

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(sumTypeMapping = KebabCase)

implicit val codec: JsonCodec[Shape] = DeriveJsonCodec.gen[Shape]
```

```scala mdoc
(RoundCircle(1.0): Shape).toJson
```

## explicitNulls

Type: `Boolean`. When `true`, an `Option` field that is `None` is written as `null`.
By default the key is left out.
`@jsonExplicitNull` sets it per type or per field.
This option only affects encoding.

```scala mdoc:silent:reset
import zio.json._

case class Profile(name: String, nickname: Option[String])

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(explicitNulls = true)

implicit val codec: JsonCodec[Profile] = DeriveJsonCodec.gen[Profile]
```

```scala mdoc
Profile("Ada", None).toJson
```

## explicitEmptyCollections

Type: `ExplicitEmptyCollections`, a case class `ExplicitEmptyCollections(encoding: Boolean = true, decoding: Boolean = true)`.

- `encoding = false` leaves out fields whose collection is empty.
- `decoding = false` lets a missing collection field decode as an empty collection.

With the defaults, empty collections are written, and a missing collection field is an error.
`@jsonExplicitEmptyCollections(encoding, decoding)` sets it per type or per field.

```scala mdoc:silent:reset
import zio.json._

case class Basket(items: List[String])

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(encoding = false, decoding = false))

implicit val codec: JsonCodec[Basket] = DeriveJsonCodec.gen[Basket]
```

```scala mdoc
Basket(Nil).toJson

"{}".fromJson[Basket]
```

## enumValuesAsStrings

Type: `Boolean`. It applies to a sealed trait whose cases are all case objects.
When `true`, each case is a plain JSON string.
When `false`, each case is a wrapper object with an empty body, as for any other sum type.
On Scala 3 it also applies to enum cases without parameters.
It has no effect when a discriminator is set, since the discriminator form always uses objects.

This is the one option with a different default per Scala version, so set it explicitly if your JSON must be the same on both.

```scala mdoc:silent:reset
import zio.json._

sealed trait Color
case object Red   extends Color
case object Green extends Color

implicit val config: JsonCodecConfiguration =
  JsonCodecConfiguration(enumValuesAsStrings = true)

implicit val codec: JsonCodec[Color] = DeriveJsonCodec.gen[Color]
```

```scala mdoc
(Red: Color).toJson

""""Green"""".fromJson[Color]
```

With `enumValuesAsStrings = false`, `Red` encodes as `{"Red":{}}`.
