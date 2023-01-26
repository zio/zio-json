---
id: configuration
title: "Configuration"
---

## Field naming

By default, the field names of a case class are used as the JSON fields, but it is easy to override this with an annotation `@jsonField`.

Moreover, you can also mark a whole case class with a member name transformation that will be applied to all members using `@jsonMemberNames` annotation. It takes an argument of type `JsonMemberFormat` which encodes the transformation that will be applied to member names.

Four most popular transformations are already provided: KebabCase, SnakeCase, PascalCase and CamelCase. If you require something more specific you can also use CustomCase which takes a function of shape `String => String` as an argument and can be used to perform any arbitrary transformation. `@jsonField` annotation takes priority over the transformation defined by `@jsonMemberNames`.

Here's an example json with most fields snake_cased and one kebab-cased:
```json
{
  "passion_fruit": true,
  "granny_smith": true,
  "dragon_fruit": true,
  "blood-orange": false
}
```

And here's the target case class:

```scala mdoc:compile-only
import zio.json._

@jsonMemberNames(SnakeCase)
case class FruitBasket(
  passionFruit: Boolean, 
  grannySmith: Boolean, 
  dragonFruit: Boolean, 
  @jsonField("blood-orange") bloodOrange: Boolean
)
```

Notice that all fields are camelCased in Scala and will be both encoded and decoded correctly to snake_case in JSON except `bloodOrange` field that is annotated with a `@jsonField` override that will force it to become `"blood-orange"` after serialization.

It is also possible to change the type hint that is used to discriminate case classes with `@jsonHint`.

For example, these annotations change the expected JSON of our `Fruit` family

```scala mdoc
import zio.json._

sealed trait Fruit

@jsonHint("banaani") case class Banana(
  @jsonField("bendiness") curvature: Double
) extends Fruit

@jsonHint("omena") case class Apple(
  @jsonField("bad") poison: Boolean
) extends Fruit

object Fruit {
  implicit val encoder: JsonEncoder[Fruit] =
    DeriveJsonEncoder.gen[Fruit]
}

val banana: Fruit = Banana(0.5)
val apple: Fruit = Apple(false)
```

from

```json
{"Banana":{"curvature":0.5}}
{"Apple":{"poison":false}}
```

to

```scala mdoc:to-string
banana.toJson
```

```scala mdoc:to-string
apple.toJson
```

## jsonDiscriminator


A popular alternative way to encode sealed traits:

```json
{"type":"banaani", "bendiness":0.5}

{"type":"omena", "bad":false}
```

is discouraged for performance reasons. However, if we have no choice in the matter, it may be accomodated with the `@jsonDiscriminator` annotation

```scala mdoc:compile-only
@jsonDiscriminator("type") sealed trait Fruit
```

## Extra fields

We can raise an error if we encounter unexpected fields by using the `@jsonNoExtraFields` annotation on a case class.

```scala mdoc
@jsonNoExtraFields case class Watermelon(pips: Int)

object Watermelon {
  implicit val decoder: JsonDecoder[Watermelon] =
    DeriveJsonDecoder.gen[Watermelon]
}
```

```scala mdoc
"""{ "pips": 32 }""".fromJson[Watermelon]
```

```scala mdoc
"""{ "pips": 32, "color": "yellow" }""".fromJson[Watermelon]
```

## Aliases

    Since zio-json 0.4.3.

After a case class field has changed name, you may still want to read JSON documents that use the old name. This is supported by the `@jsonAliases` annotation.

```scala mdoc
case class Strawberry(
  @jsonAliases("seeds") seedCount: Int
)

object Strawberry {
  implicit val decoder: JsonDecoder[Strawberry] =
    DeriveJsonDecoder.gen[Strawberry]
}
```

The following two expressions result in an equal value:
```scala mdoc
"""{ "seeds": 32 }""".fromJson[Strawberry]
"""{ "seedCount": 32 }""".fromJson[Strawberry]
```

The `@jsonAliases` annotation supports multiple aliases. The annotation has no effect on encoding.

## @jsonDerive

**Requires zio-json-macros**

`@jsonDerive` allows to reduce that needs to be written using an annotation macro to generate JsonDecoder/JsonEncoder at build-time.

For generating both Encoder and Decoder, simply use jsonDerive

For example: 

```scala mdoc:compile-only
import zio.json._

@jsonDerive case class Watermelon(pips: Int)
```
It is equivalent to:

```scala mdoc:compile-only
import zio.json._

case class Watermelon(pips: Int)

object Watermelon {
  implicit val codec: JsonCodec[Watermelon] =
    DeriveJsonCodec.gen[Watermelon]
}
```

To generate only an encoder, we can set it as config parameter:

For example:

```scala mdoc:compile-only
import zio.json._

@jsonDerive(JsonDeriveConfig.Encoder) case class Watermelon(pips: Int)
```
It is equivalent to:

```scala mdoc:compile-only
import zio.json._

case class Watermelon(pips: Int)

object Watermelon {
  implicit val encorder: JsonEncoder[Watermelon] =
    DeriveJsonEncoder.gen[Watermelon]
}
```

To generate only a decoder, we can set it as config parameter:

For example:

```scala modc:compile-only
import zio.json._

@jsonDerive(JsonDeriveConfig.Decoder) case class Watermelon(pips: Int)
```
It is equivalent to:

```scala mdoc:compile-only
import zio.json._

case class Watermelon(pips: Int)

object Watermelon {
  implicit val decoder: JsonDecoder[Watermelon] =
    DeriveJsonDecoder.gen[Watermelon]
}
```
