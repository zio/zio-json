---
id: decoding
title: "Decoding"
---

## Automatic Derivation

Say we want to be able to read some JSON like

```json
{ "curvature": 0.5 }
```

into a Scala `case class`

```scala mdoc
case class Banana(curvature: Double)
```

To do this, we create an *instance* of the `JsonDecoder` typeclass for `Banana` using the `zio-json` code generator. It is best practice to put it on the companion of `Banana`, like so

```scala mdoc
import zio.json._

object Banana {
  implicit val decoder: JsonDecoder[Banana] =
    DeriveJsonDecoder.gen[Banana]
}
```

Now we can parse JSON into our object

```scala mdoc
"""{ "curvature": 0.5 }""".fromJson[Banana]
```

### Automatic Derivation and case class default field values

If a case class field is defined with a default value and the field is not present or `null`, the default value will be used.

Say we have a Scala `case class`

```scala mdoc
case class Entity(id: Long, description: String = "", related: Seq[Entity] = Seq())

implicit val decoder: JsonDecoder[Entity] =
  DeriveJsonDecoder.gen[Entity]
```

```scala mdoc
"""{ "id": 42, "related": null }""".fromJson[Entity]
```

_Note: If you’re using Scala 3 and your case class is defining default parameters, `-Yretain-trees` needs to be added to `scalacOptions`._

## ADTs

Say we extend our data model to include more data types

```scala mdoc:reset
sealed trait Fruit

case class Banana(curvature: Double) extends Fruit
case class Apple (poison: Boolean)   extends Fruit
```

we can generate the decoder for the entire `sealed` family:

```scala mdoc
import zio.json._

object Fruit {
  implicit val decoder: JsonDecoder[Fruit] =
    DeriveJsonDecoder.gen[Fruit]
}
```

```scala mdoc
"""{ "Banana":{ "curvature":0.5 }}""".fromJson[Fruit]
```

```scala mdoc
"""{ "Apple": { "poison": false }}""".fromJson[Fruit]
```

Almost all of the standard library data types are supported as fields on the case class, and it is easy to add support if one is missing.

## Manual instances

Sometimes it is easier to reuse an existing `JsonDecoder` rather than generate a new one. This can be accomplished using convenience methods on the `JsonDecoder` typeclass to *derive* new decoders

```scala
trait JsonDecoder[A] {
  def map[B](f: A => B): JsonDecoder[B]
  def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B]
  ...
}
```

### `.map`

We can `.map` from another `JsonDecoder` in cases where the conversion will always succeed. This is very useful if we have a `case class` that simply wraps another thing and shares the same expected JSON.

For example, say we want to model the count of fruit with a `case class` to provide us with additional type safety in our business logic (this pattern is known as a *newtype*).

```scala mdoc
case class FruitCount(value: Int)
```

but this would cause us to expect JSON of the form

```json
{"value": 1}
```

wheres we really expect the raw number. We can derive a decoder from `JsonDecoder[Int]` and `.map` the result into a `FruitCount`

```scala mdoc
object FruitCount {
  implicit val decoder: JsonDecoder[FruitCount] =
    JsonDecoder[Int].map(FruitCount(_))
}
```

and now the `JsonDecoder` for `FruitCount` just expects a raw `Int`.

```scala mdoc
"""3""".fromJson[FruitCount]
```

Another use case is if we want to encode a `case class` as an array of values, rather than an object with named fields. Such an encoding is very efficient because the messages are smaller and require less processing, but are very strict schemas that cannot be upgraded.

```scala:mdoc:reset
import zio.json._

case class Things(s: String, i: Int, b: Boolean)

object Things {
  implicit val decoder: JsonDecoder[Things] =
    JsonDecoder[(String, Int, Boolean)].map { case (p1, p2, p3) => Things(p1, p2, p3) }
}

"""[ "hello", 1, true ]""".fromJson[Things]
```

### `.mapOrFail`

We can use `.mapOrFail` to take the result of another `JsonDecoder` and try to convert it into our custom data type, failing with a message if there is an error.

Say we are using the [`refined`](https://github.com/fthomas/refined) library to ensure that a `Person` data type only holds a non-empty string in its `name` field

```scala mdoc
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

case class Person(name: String Refined NonEmpty)
```

we will get a compile time error because there is no `JsonDecoder[String Refined NonEmpty]`.

```scala mdoc:fail
object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen
}
```

However, we can derive one by requesting the `JsonDecoder[String]` and calling `.mapOrFail`, supplying the constructor for our special `String Refined NonEmpty` type

```scala mdoc
import eu.timepit.refined

implicit val decodeName: JsonDecoder[String Refined NonEmpty] =
  JsonDecoder[String].mapOrFail(refined.refineV[NonEmpty](_))
```

Now the code compiles.
