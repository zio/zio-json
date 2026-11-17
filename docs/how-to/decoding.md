---
id: decoding
title: "Decoding"
description: "Read JSON text or bytes into Scala values with ZIO JSON: derived decoders, sealed traits, defaults, and decoding JSON whose keys do not match your fields."
---

## Automatic Derivation

Say we want to be able to read some JSON like this:

```json
{ "curvature": 0.5 }
```

into a Scala `case class`:

```scala mdoc
case class Banana(curvature: Double)
```

To do this, we create an *instance* of the `JsonDecoder` type class for `Banana` with `DeriveJsonDecoder.gen`. It is best practice to put it on the companion of `Banana`:

```scala mdoc
import zio.json._

object Banana {
  implicit val decoder: JsonDecoder[Banana] =
    DeriveJsonDecoder.gen[Banana]
}
```

Now we can parse JSON into our object:

```scala mdoc
"""{ "curvature": 0.5 }""".fromJson[Banana]
```

### Decoding from Bytes

JSON usually arrives as bytes rather than as text: an HTTP response body, a file, a Kafka record. A `Chunk[Byte]` can be decoded directly, without being copied into a `String` first (see [`decodeJson`](../reference/json-decoder.md#decodejson) for the full list of inputs):

```scala mdoc:silent
import zio.Chunk

val bytes: Chunk[Byte] = Chunk.fromArray("""{ "curvature": 0.5 }""".getBytes("UTF-8"))
```

The chunk has the same `fromJson` method as a string:

```scala mdoc
bytes.fromJson[Banana]
```

A bare `Array[Byte]` works the same way, and is not copied either:

```scala mdoc
"""{ "curvature": 0.5 }""".getBytes("UTF-8").fromJson[Banana]
```

The bytes are turned into characters as the parser consumes them, so peak memory is the chunk plus a constant. Going through `new String(chunk.toArray, UTF_8)` instead holds the chunk, an array copy of it and the `String` all at once, which matters once payloads get large.

A chunk that is already backed by a single array — what `Chunk.fromArray` produces, and what an HTTP body handed over from Netty looks like — is read in place, with no copy at all. Other shapes, such as a body assembled from several network reads, are pulled through a small fixed window.

The input is expected to be UTF-8, as [RFC 8259](https://www.rfc-editor.org/rfc/rfc8259#section-8.1) requires for JSON interchange. Malformed UTF-8 decodes to the replacement character U+FFFD on every platform, matching what `new String(bytes, UTF_8)` produces on the JVM. Errors are reported exactly as for the `CharSequence` variant.

For payloads that do not fit in memory at all, use `JsonDecoder[A].decodeJsonStreamInput(stream)` on the JVM to decode straight from a `ZStream[R, Throwable, Byte]`. The streaming methods are JVM only; see [Streaming](./streaming.md).

### Default Field Values

If a case class field is defined with a default value and the field is not present or `null`, the default value is used. A default is evaluated again on each decode, so a default such as `java.util.UUID.randomUUID()` gives a fresh value every time.

Say we have a Scala `case class` with a derived decoder:

```scala mdoc:silent
case class Entity(id: Long, description: String = "", related: Seq[Entity] = Seq())

implicit val decoder: JsonDecoder[Entity] =
  DeriveJsonDecoder.gen[Entity]
```

The missing `description` and the `null` `related` both take their defaults:

```scala mdoc
"""{ "id": 42, "related": null }""".fromJson[Entity]
```

Defaults work the same way on Scala 2 and Scala 3, with no extra compiler options. [Optional, default and null fields](./optional-and-default-fields.md) covers missing and `null` fields in detail.

## ADTs

Say we extend our data model to include more data types:

```scala mdoc:reset
sealed trait Fruit

case class Banana(curvature: Double) extends Fruit
case class Apple (poison: Boolean)   extends Fruit
```

We can generate the decoder for the entire `sealed` family:

```scala mdoc
import zio.json._

object Fruit {
  implicit val decoder: JsonDecoder[Fruit] =
    DeriveJsonDecoder.gen[Fruit]
}
```

The key of the wrapping object names the subtype to decode:

```scala mdoc
"""{ "Banana":{ "curvature":0.5 }}""".fromJson[Fruit]
```

The same decoder reads the other subtype:

```scala mdoc
"""{ "Apple": { "poison": false }}""".fromJson[Fruit]
```

### String-Based Union Types (Scala 3 Only)

On Scala 3, a union of string literal types, such as `"Apple" | "Banana"`, has a codec out of the box. It is useful when a full `enum` is more than you need. The value is written as a plain JSON string, and decoding accepts only the listed strings. Only unions of `String` literals are supported.

The input of `fromJson` must be JSON text, so the string being decoded carries its own JSON quotes:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/decoding/UnionTypesExample.scala", "union")
```

Running the example file on Scala 3 prints:

```text
encoded: "Apple"
valid: Right(Apple)
invalid: Left((expected one of: Apple, Banana))
```

Decoding `Pear` fails because it is not one of the listed values. The message format is described in [Errors](../reference/errors.md).

Almost all of the standard library data types are supported as fields on the case class (see [Built-in Instances](../reference/built-in-instances.md)), and it is easy to add support if one is missing.

For decoding sealed families and enums of parameterless cases as plain strings, see [Sealed Traits](./sealed-traits.md).

## Manual Instances

Sometimes it is easier to reuse an existing `JsonDecoder` than to generate a new one: `JsonDecoder#map` and `JsonDecoder#mapOrFail` derive a decoder from another one. See [Manual Instances](./manual-instances.md) for newtypes, array-encoded case classes and validated types such as `refined`.

## Parsing Custom JSON

In this section we show several approaches for decoding JSON that looks like:

```json
{
  "01. symbol": "IBM",
  "02. open": "182.4300",
  "03. high": "182.8000"
}
```

Which we want to decode into the following case class:

```scala mdoc:reset
import zio.json._

final case class Quote(
  symbol: String,
  open: String,
  high: String
)

object Quote {
  implicit val decoder: JsonDecoder[Quote] = DeriveJsonDecoder.gen[Quote]
}
```

With a derived decoder and no customization, decoding fails because the JSON keys do not match the field names:

```scala mdoc
"""{"01. symbol":"IBM","02. open": "182.4300","03. high": "182.8000"}""".fromJson[Quote]
```

Each approach below makes this input decode to `Right(Quote(IBM,182.4300,182.8000))`.

### Approach 1: Use Annotations

In this approach we enrich the case class with `@jsonField` annotations to tell the derived decoder which field names to use. This approach only works if we can change the case class:

```scala mdoc:reset
import zio.json._

final case class Quote(
  @jsonField("01. symbol") symbol: String,
  @jsonField("02. open") open: String,
  @jsonField("03. high") high: String
)

object Quote {
  implicit val decoder: JsonDecoder[Quote] = DeriveJsonDecoder.gen[Quote]
}
```

### Approach 2: Use an Intermediate Case Class

Instead of annotations, we can also put the actual field names in an intermediate case class. In our example the field names are not valid Scala identifiers. We fix this by putting the names in backticks:

```scala mdoc:reset
import zio.json._

final case class Quote(symbol: String, open: String, high: String)

object Quote {
  private final case class JsonQuote(
    `01. symbol`: String,
    `02. open`: String,
    `03. high`: String
  )

  implicit val decoder: JsonDecoder[Quote] =
    DeriveJsonDecoder
      .gen[JsonQuote]
      .map { case JsonQuote(s, o, h) => Quote(s, o, h) }
}
```

### Approach 3: Decode to `Json`

In this approach we first decode to the generic `Json` data structure. This approach is very flexible because it can extract data from any valid JSON.

`Json#asString` returns the text of a `Json.Str` and `None` for any other JSON type, so a field that holds a number or an object is rejected. Do not use `toString` here: it renders the node as JSON, so a string would keep its quotes:

```scala mdoc:reset
import zio.json._
import zio.json.ast.Json

final case class Quote(symbol: String, open: String, high: String)

object Quote {
  implicit val decoder: JsonDecoder[Quote] = JsonDecoder[Json]
    .mapOrFail {
      case Json.Obj(fields) =>
        def findField(name: String): Either[String, String] =
          fields
            .find(_._1 == name)
            .flatMap(_._2.asString)
            .toRight(left = s"Field '$name' is missing or not a string")
  
        for {
          symbol <- findField("01. symbol")
          open <- findField("02. open")
          high <- findField("03. high")
        } yield Quote(symbol, open, high)
      case _ =>
        Left("Not a JSON record")
    }
}
```

### Approach 4: Decode to `Json` and Use Cursors

Here we also first decode to `Json`, but now we use cursors to find the data we need. `JsonCursor.isString` rejects a field that is not a string, as `Json#asString` does in approach 3:

```scala mdoc:reset
import zio.json._
import zio.json.ast.{Json, JsonCursor}

final case class Quote(symbol: String, open: String, high: String)

object Quote {
  private val symbolC = JsonCursor.field("01. symbol") >>> JsonCursor.isString
  private val openC = JsonCursor.field("02. open") >>> JsonCursor.isString
  private val highC = JsonCursor.field("03. high") >>> JsonCursor.isString

  implicit val decoder: JsonDecoder[Quote] = JsonDecoder[Json]
    .mapOrFail { c =>
      for {
        symbol <- c.get(symbolC)
        open <- c.get(openC)
        high <- c.get(highC)
      } yield Quote(symbol.value, open.value, high.value)
  }
}
```

## More Custom Decoder Examples

Let's consider an `Animal` case class with a `categories` field that should be a list of strings. However, some producers accidentally represent the categories as a comma-separated string instead of a proper list. We want to parse both cases.

Here is a custom decoder for our `Animal` case class:

```scala mdoc:reset
import zio.Chunk
import zio.json._
import zio.json.ast._

case class Animal(name: String, categories: List[String])

object Animal {
  private val nameC = JsonCursor.field("name") >>> JsonCursor.isString
  private val categoryArrayC = JsonCursor.field("categories") >>> JsonCursor.isArray
  private val categoryStringC = JsonCursor.field("categories") >>> JsonCursor.isString

  implicit val decoder: JsonDecoder[Animal] = JsonDecoder[Json]
    .mapOrFail { c =>
      for {
        name <- c.get(nameC).map(_.value)
        categories <- arrayCategory(c).map(_.toList)
          .orElse(c.get(categoryStringC).map(_.value.split(',').map(_.trim).toList))
      } yield Animal(name, categories)
    }

  private def arrayCategory(c: Json): Either[String, Chunk[String]] =
    c.get(categoryArrayC)
      .flatMap { arr =>
        // Get the string elements, and sequence the obtained eithers to a single either
        sequence(arr.elements.map(_.get(JsonCursor.isString).map(_.value)))
      }

  private def sequence[A, B](chunk: Chunk[Either[A, B]]): Either[A, Chunk[B]] =
    chunk.partition(_.isLeft) match {
      case (Nil, rights) => Right(rights.collect { case Right(r) => r })
      case (lefts, _) => Left(lefts.collect { case Left(l) => l }.head)
    }
}
```

And now, the JSON decoder for `Animal` can handle both formats:

```scala mdoc
"""{"name": "Dog", "categories": "Warm-blooded, Mammal"}""".fromJson[Animal]
"""{"name": "Snake", "categories": [ "Cold-blooded", "Reptile"]}""".fromJson[Animal]
```

## JSON AST and Cursors

In most cases it is not necessary to work with the JSON AST directly; it is more convenient to decode directly to domain objects. However, sometimes it is handy to work with a lower level representation of JSON. This may for example be the case when you need to work with deeply nested JSON structures that would result in deeply nested case classes, or when you expect a lot of variation in the JSON structure, which would result in complicated decoders.

To get the AST representation of a JSON string, decode it as `Json` with `fromJson[Json]`:

```scala mdoc
import zio.json._
import zio.json.ast._

val jsonString: String            = """{"name": "John Doe"}"""
val jsonAst: Either[String, Json] = jsonString.fromJson[Json]
```

The `Json` type is a recursive data structure that can be navigated with pattern matching on its six cases:

```scala mdoc:reset

import zio.Chunk
import zio.json._
import zio.json.ast.Json
import zio.json.ast.Json._

val jsonString: String = """{"name": "John Doe"}"""
val jsonAst: Json      = jsonString.fromJson[Json].toOption.get
jsonAst match {
  case Obj(fields: Chunk[(String, Json)]) => ()
  case Arr(elements: Chunk[Json])         => ()
  case Bool(value: Boolean)               => ()
  case Str(value: String)                 => ()
  case Num(value: java.math.BigDecimal)   => ()
  case Json.Null                          => ()
}
```

To get the `name` field, you could do the following:

```scala mdoc
import zio.json._
import zio.json.ast.Json

val json: Option[Json] = """{"name": "John Doe"}""".fromJson[Json].toOption
val name: Option[String] = json.flatMap { json =>
  json match {
    case Json.Obj(fields) => fields.collectFirst { case ("name", Json.Str(name)) => name }
    case _                => None
  }
}
```

For navigating the AST with cursors, see [Cursors](../reference/json-ast.md#cursors).
