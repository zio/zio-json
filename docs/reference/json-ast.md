---
id: json-ast
title: "JSON AST"
---

The JSON AST is a tree of values that represents any JSON document, using the `zio.json.ast.Json` type.
Most code decodes JSON straight into domain types, but the AST is useful when the shape of the document is not known in advance, when you need to edit a document, or when you want to combine several documents.
For navigating the AST with cursors, see [Cursors](#cursors).

## The Json Type

`Json` is a sealed type with six cases: `Json.Obj`, `Json.Arr`, `Json.Str`, `Json.Num`, `Json.Bool`, and `Json.Null`.
Objects keep their fields in insertion order as a `Chunk` of key-value pairs.

The cases can be constructed directly:

```scala mdoc
import zio.json._
import zio.json.ast.{ Json, JsonCursor }

val user: Json = Json.Obj(
  "name"   -> Json.Str("Alice"),
  "age"    -> Json.Num(30),
  "admin"  -> Json.Bool(false),
  "tags"   -> Json.Arr(Json.Str("ops"), Json.Str("dev")),
  "avatar" -> Json.Null
)
```

`Json.Num` wraps a `java.math.BigDecimal`, and its companion has `apply` overloads for `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `BigInt`, `BigDecimal` and `java.math.BigInteger`, so numbers keep their precision.

## Converting Between Types and the AST

`JsonEncoder#toJsonAST` takes a value and returns `Either[String, Json]`, and the `value.toJsonAST` extension calls it with the implicit encoder. `JsonDecoder#fromJsonAST` goes from a `Json` value to a domain type. The `Json#as` method is the shortcut for decoding:

```scala mdoc:silent
case class Person(name: String, age: Int)

object Person {
  implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
}
```

To encode a value into the AST, call `toJsonAST` on the value:

```scala mdoc
Person("Bob", 41).toJsonAST
```

To decode an AST into a domain type, call `as` on the `Json` value:

```scala mdoc
user.as[Person]
```

Decoding with `as` fails when the fields do not match, which lets you check the shape of a document:

```scala mdoc
Json.Obj("name" -> Json.Str("Carol")).as[Person]
```

## Reading Values

The `Json#get` method takes a `JsonCursor` and returns the node at that location, with its type refined by the cursor. The `Obj#get` method reads one field by name, and `Obj#contains` checks whether a field exists:

```scala mdoc
user.asObject.map(_.get("name"))
```

Checking for a field that is not present returns `false` inside an `Option`:

```scala mdoc
user.asObject.map(_.contains("nickname"))
```

Each case has an `as*` method that returns an `Option` of the matching type, for example `asString`, `asNumber`, `asArray`, `asObject`, `asBoolean` and `asNull`:

```scala mdoc
user.get(JsonCursor.field("age")).map(_.asNumber).toOption.flatten
```

## Transforming Values

The `mapXxx` methods change one kind of node without touching the rest. For example, `mapObjectKeys` renames every key in an object:

```scala mdoc
user.mapObjectKeys(_.toUpperCase)
```

`mapObjectValues` and `mapArrayValues` apply a function to each direct child:

```scala mdoc
user.mapObjectValues {
  case Json.Num(value) => Json.Num(value.add(java.math.BigDecimal.ONE))
  case other           => other
}
```

The `transformDown` method walks the whole tree from the top and applies a function to every node. The `transformUp` method works from the bottom, so the function sees each node after its children were transformed.
Use the `transformDownSome` and `transformUpSome` variants when the function only applies to some nodes. The following replaces every string with its upper-case form and leaves other values unchanged:

```scala mdoc
user.transformDownSome { case Json.Str(value) => Json.Str(value.toUpperCase) }
```

To change a single node located by a cursor, use `transformAt`. It returns an `Either` because the cursor may not match the document:

```scala mdoc
user.transformAt(JsonCursor.field("age").isNumber)(age => Json.Num(age.value.add(java.math.BigDecimal.TEN)))
```

## Deleting and Relocating Values

The `delete` method removes the node at a cursor. The `relocate` method moves a node from one cursor to another, removing it from its old location:

```scala mdoc
user.delete(JsonCursor.field("avatar"))
```

Relocating `tags` to `avatar` replaces the `null` value there, and removes `tags` from its original location:

```scala mdoc
user.relocate(JsonCursor.field("tags"), JsonCursor.field("avatar"))
```

## Combining Documents

The `merge` method combines two values. When both values are objects, the result contains the fields of both, and the right-hand side wins when a key appears on both sides. When both values are arrays, their elements are merged. For any other combination, the right-hand side replaces the left:

```scala mdoc
val defaults: Json = Json.Obj("admin" -> Json.Bool(false), "locale" -> Json.Str("en"))
val overrides: Json = Json.Obj("locale" -> Json.Str("de"))

defaults.merge(overrides)
```

The `intersect` method keeps only the fields or elements that are present in both values. It returns an error for values that are not both objects or both arrays:

```scala mdoc
val left: Json  = Json.Obj("a" -> Json.Num(1), "b" -> Json.Num(2))
val right: Json = Json.Obj("b" -> Json.Num(2), "c" -> Json.Num(3))

left.intersect(right)
```

## Folding Over a Document

The `foldDown` and `foldUp` methods reduce the whole tree into one value. They visit every node, with `foldDownSome` and `foldUpSome` as the variants for partial functions. For example, to count the string values in a document:

```scala mdoc
user.foldDown(0) {
  case (count, Json.Str(_)) => count + 1
  case (count, _)           => count
}
```

## Rendering and Parsing

The `toJson` extension on `Json` renders the AST as a compact string, and `toJsonPretty` renders it with indentation. The `fromJson[Json]` method parses a string into an AST:

```scala mdoc
user.toJsonPretty
```

Parsing a string into the AST uses the same `fromJson` method as domain types, with `Json` as the type:

```scala mdoc
"""{"id": 7, "ok": true}""".fromJson[Json]
```

For encoding and decoding in streams, see [Streaming](../how-to/streaming.md). For YAML, the AST is the intermediate form, as described in [YAML](./yaml.md).

## Cursors

Cursors navigate the JSON AST. They are usually more convenient than pattern matching on `Json` directly.

```scala mdoc
import zio.json.ast.Json.Str

val nameJson: Either[String, Json] = """{"name": "John Doe"}""".fromJson[Json]
val nameCursor: JsonCursor[Json, Str] = JsonCursor.field("name").isString
val nameValue: Either[String, String] = nameJson.flatMap(_.get(nameCursor).map(_.value))
```

Cursors can be composed to navigate more complex JSON structures.

```scala mdoc
val json1: Either[String, Json] = """{"posts": [{"id": 0, "title": "foo"}]}""".fromJson[Json]
val json2: Either[String, Json] = """{"userPosts": [{"id": 1, "title": "bar"}]}""".fromJson[Json]

val commonCursor = 
  JsonCursor.isArray >>> 
    JsonCursor.element(0) >>> 
    JsonCursor.isObject >>> 
    JsonCursor.field("title") >>> 
    JsonCursor.isString

val cursor1 = JsonCursor.field("posts")
val cursor2 = JsonCursor.field("userPosts")

def getTitle(json: Either[String, Json]) =
  for {
    ast   <- json
    posts <- ast.get(cursor1).orElse(ast.get(cursor2))
    title <- posts.get(commonCursor).map(_.value)
  } yield title

val title1 = getTitle(json1)
val title2 = getTitle(json2)
```
