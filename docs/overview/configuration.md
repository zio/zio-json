---
id: overview_configuration
title: "Configuration"
---

## Field naming

By default the field names of a case class are used as the JSON fields, but it is easy to override this with an annotation `@jsonField`.

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

## @jsonDerive

**Requires zio-json-macros**

`@jsonDerive` allows to reduce that needs to be written using an annotation macro to generate JsonDecoder/JsonEncoder at build-time.

For generating both Encoder and Decoder, simply use jsonDerive

For example: 

```scala
import zio.json._

@jsonDerive case class Watermelon(pips: Int)
```
It is equivalent to:

```scala
import zio.json._

case class Watermelon(pips: Int)

object Watermelon {
  implicit val codec: JsonCodec[Watermelon] =
    DeriveJsonCodec.gen[Watermelon]
}
```

To generate only an encoder, we can set it as config parameter:

For example:

```scala
import zio.json._

@jsonDerive(JsonDeriveConfig.Encoder) case class Watermelon(pips: Int)
```
It is equivalent to:

```scala
import zio.json._

case class Watermelon(pips: Int)

object Watermelon {
  implicit val encorder: JsonEncoder[Watermelon] =
    DeriveJsonEncoder.gen[Watermelon]
}
```

To generate only a decoder, we can set it as config parameter:

For example:

```scala
import zio.json._

@jsonDerive(JsonDeriveConfig.Decoder) case class Watermelon(pips: Int)
```
It is equivalent to:

```scala
import zio.json._

case class Watermelon(pips: Int)

object Watermelon {
  implicit val decoder: JsonDecoder[Watermelon] =
    DeriveJsonDecoder.gen[Watermelon]
}
```
