---
id: manual-instances
title: "Manual Instances"
---

Sometimes it is easier to reuse an existing `JsonDecoder` rather than generate a new one. This can be accomplished using convenience methods on the `JsonDecoder` typeclass to *derive* new decoders:

```scala
trait JsonDecoder[A] {
  def map[B](f: A => B): JsonDecoder[B]
  def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B]
  ...
}
```

Similarly, we can reuse an existing `JsonEncoder`:

```scala
trait JsonEncoder[A] {
  def contramap[B](f: B => A): JsonEncoder[B]
  ...
}
```

## `JsonDecoder#map`

We can use `JsonDecoder#map` on another `JsonDecoder` in cases where the conversion will always succeed. This is very useful if we have a `case class` that simply wraps another thing and shares the same expected JSON.

For example, say we want to model the count of fruit with a `case class` to provide us with additional type safety in our business logic (this pattern is known as a *newtype*):

```scala mdoc:silent
import zio.json._

case class FruitCount(value: Int)
```

but a derived decoder would cause us to expect JSON of the form:

```json
{"value":1}
```

whereas we really expect the raw number. We can derive a decoder from `JsonDecoder[Int]` and `map` the result into a `FruitCount`. Every time we use `JsonDecoder#map` to create a `JsonDecoder`, we can usually create a `JsonEncoder` with `JsonEncoder#contramap`, so the companion holds both:

```scala mdoc:silent
object FruitCount {
  implicit val decoder: JsonDecoder[FruitCount] = JsonDecoder[Int].map(FruitCount(_))
  implicit val encoder: JsonEncoder[FruitCount] = JsonEncoder[Int].contramap(_.value)
}
```

Now the `JsonDecoder` for `FruitCount` just expects a raw `Int`, and the encoder writes one:

```scala mdoc
"3".fromJson[FruitCount]
FruitCount(3).toJson
```

Another use case is if we want to encode a `case class` as an array of values, rather than an object with named fields. Such an encoding is very efficient because the messages are smaller and require less processing, but are very strict schemas that cannot be upgraded. Both halves go through the tuple instances:

```scala mdoc:silent
case class Things(s: String, i: Int, b: Boolean)

object Things {
  implicit val decoder: JsonDecoder[Things] =
    JsonDecoder[(String, Int, Boolean)].map { case (p1, p2, p3) => Things(p1, p2, p3) }

  implicit val encoder: JsonEncoder[Things] =
    JsonEncoder[(String, Int, Boolean)].contramap(t => (t.s, t.i, t.b))
}
```

which writes and parses the following JSON:

```scala mdoc
Things("hello", 1, true).toJson
"""["hello",1,true]""".fromJson[Things]
```

## `JsonDecoder#mapOrFail`

We can use `JsonDecoder#mapOrFail` to take the result of another `JsonDecoder` and try to convert it into our custom data type, failing with a message if there is an error.

Say we are using the [`refined`](https://github.com/fthomas/refined) library to ensure that a `Person` data type only holds a non-empty string in its `name` field. This does not compile, because there is no `JsonDecoder[String Refined NonEmpty]` in scope:

```scala mdoc:fail
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

case class Person(name: String Refined NonEmpty)

object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen
}
```

However, we can derive one by requesting the `JsonDecoder[String]` and calling `mapOrFail`, supplying the constructor for our special `String Refined NonEmpty` type. With that instance in scope, the code compiles:

```scala mdoc:silent
import eu.timepit.refined
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

implicit val decodeName: JsonDecoder[String Refined NonEmpty] =
  JsonDecoder[String].mapOrFail(refined.refineV[NonEmpty](_))

case class Person(name: String Refined NonEmpty)

object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen
}
```

An empty name is now rejected while decoding:

```scala mdoc
"""{"name":""}""".fromJson[Person]
"""{"name":"Ann"}""".fromJson[Person]
```

In fact, we do not need to provide `decodeName` for each `Refined` data type; `zio-json` comes with support out of the box, see [Refined Interop](../interop/refined.md).

## Map Keys

A type used as a `Map` key needs a `JsonFieldEncoder` and a `JsonFieldDecoder` rather than a `JsonEncoder` and a `JsonDecoder`, because JSON object keys are always strings. They have the same `contramap`, `map` and `mapOrFail` methods. See [JsonFieldEncoder and JsonFieldDecoder](../reference/json-field-codecs.md).

## Writing Instances by Hand

When no existing instance fits, you can implement the encoder or decoder directly against the low-level writer and reader. See [Writing an Encoder by Hand](../reference/json-encoder.md#writing-an-encoder-by-hand) and [Writing a Decoder by Hand](../reference/json-decoder.md#writing-a-decoder-by-hand).

## See Also

- [JsonEncoder#contramap](../reference/json-encoder.md#contramap) and [JsonDecoder#map and mapOrFail](../reference/json-decoder.md#map-and-maporfail) in the reference.
- [Transforming a codec](../reference/json-codec.md#transforming-a-codec) does both directions at once with `JsonCodec#transform` and `JsonCodec#transformOrFail`.
- [Encode a type you do not own](./third-party-types.md) applies these methods to a class from another library.
