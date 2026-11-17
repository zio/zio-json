---
id: edit-json-with-cursors
title: "Read and Edit JSON of Unknown Shape"
description: "Parse JSON you have no case class for into the Json AST, read nested values with cursors, and edit, move, merge and print it."
keywords:
  - "JSON AST"
  - "JsonCursor"
  - "Dynamic JSON"
  - "Edit JSON"
  - "Webhook Payload"
---

In this guide, we take a JSON document that we have no case class for, a webhook payload from a payment provider, and read and rewrite it with the `Json` AST and `JsonCursor`. By the end, you will have a function that reads the values your code needs, masks personal data, drops a debug flag, moves a nested object to the top level, fills in default values, and prints the cleaned payload.

We work on one payload from start to finish:

- Parse the text into a `Json` value.
- Read nested values with cursors, and read the errors you get when a field is missing or has the wrong type.
- Decode one part of the document into a case class.
- Update, delete and move values.
- Merge in default values and print the result.

## The Problem

A webhook payload, a configuration file or a response from a third-party API often has a shape you do not control. It has many fields you do not care about, and the provider adds new fields over time. You need a few values from deep inside the document, and sometimes you need to forward a changed copy of the whole document to another service.

Decoding such a document into case classes forces you to model every level of nesting just to reach the values you need. Forwarding is worse: derived decoders ignore fields that the case class does not declare, so encoding the case class again silently drops every field you did not model. Each time the provider adds a field, your forwarded copy loses it.

The `Json` AST solves this. It holds the whole document as a tree, so nothing is lost, and cursors let you read and change single nodes deep inside it without modelling the rest.

## Prerequisites

The AST and cursors are part of the core `zio-json` module:

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

All examples use these imports:

```scala mdoc:silent:reset-object
import zio.json._
import zio.json.ast.{ Json, JsonCursor }
```

This guide shows the operations in the order you would use them on a real document. The [JSON AST](../reference/json-ast.md) reference page lists every method, case and error message in detail.

## Parse the Payload

Our payment provider sends an `order.paid` event. The order is nested two levels down, and the `metadata` object holds information we do not want to forward:

```scala mdoc:silent
val payload: String =
  """{
    |  "id": "evt_1001",
    |  "type": "order.paid",
    |  "data": {
    |    "order": {
    |      "id": 42,
    |      "customer": { "name": "Ada", "email": "ada@example.com" },
    |      "items": [
    |        { "sku": "BOOK-1", "qty": 2, "price": 12.50 },
    |        { "sku": "PEN-7", "qty": 1, "price": 3.00 }
    |      ],
    |      "total": 28.00
    |    }
    |  },
    |  "metadata": { "source": "web", "debug": true }
    |}""".stripMargin
```

Parsing with `Json` as the target type accepts any valid JSON. It returns `Either[String, Json]`, with a `Left` only when the text is not valid JSON:

```scala mdoc:silent
val parsed: Either[String, Json] = payload.fromJson[Json]
```

To keep the next sections short, we take the value out of the `Either`. In real code, keep working inside the `Either`, as the last section of this guide does:

```scala mdoc:silent
val event: Json = parsed.fold(error => sys.error(error), identity)
```

## Read Nested Values with a Cursor

A cursor is a path into a document. It is built from steps: `JsonCursor#field` goes into a field of an object, `JsonCursor#element` goes into an element of an array, and the type steps (`JsonCursor#isObject`, `JsonCursor#isArray`, `JsonCursor#isString`, `JsonCursor#isNumber`, `JsonCursor#isBool` and `JsonCursor#isNull`) check the type of the current node. The `JsonCursor` companion has the same methods to start a path. `Json#get` follows the path and returns the node it ends on.

A field step ends on a plain `Json`, because the field could hold any kind of value. Before going one level deeper, add `JsonCursor#isObject` (or `JsonCursor#isArray` before an element step) so the next step knows it starts from an object. We define the paths once and reuse them in the rest of the guide:

```scala mdoc:silent
val order    = JsonCursor.field("data").isObject.field("order").isObject
val customer = order.field("customer").isObject
val email    = customer.field("email").isString
val items    = order.field("items").isArray
val total    = order.field("total").isNumber
```

Without the `JsonCursor#isObject` step, the code does not compile, because `JsonCursor#field` needs a cursor that ends on a `Json.Obj`:

```scala mdoc:fail
JsonCursor.field("data").field("order")
```

A cursor that ends with a type step such as `JsonCursor#isString` returns that case of `Json`, so `Json#get` gives a `Json.Str` and `Json.Str#value` gives the `String`. For a number, `Json.Num#value` is a `java.math.BigDecimal`:

```scala mdoc
event.get(JsonCursor.field("type").isString).map(_.value)

event.get(email).map(_.value)

event.get(total).map(_.value)
```

To read an element of an array, add `JsonCursor#element` with a zero-based index after a `JsonCursor#isArray` step:

```scala mdoc
event.get(items.element(0).isObject.field("sku").isString).map(_.value)
```

## Handle Missing Fields and Wrong Types

`Json#get` returns a `Left` when the document does not match the cursor. A field that does not exist gives `No such field`:

```scala mdoc
event.get(order.field("coupon"))
```

A type step that does not match gives `expected <type>`. Here, the order `id` is a number, not a string:

```scala mdoc
event.get(order.field("id").isString)
```

An index past the end of an array has its own message:

```scala mdoc
event.get(items.element(5))
```

The messages name only the last step that failed, not the whole path. When you read several values, add the path to the error yourself, for example with `left.map(error => s"data.order.id: $error")`. The full list of messages is in [Cursor Errors](../reference/json-ast.md#cursor-errors).

## Decode a Part of the Document

When a part of the document has a stable shape, decode just that part into a case class. Get the sub-tree with a cursor, then call `Json#as`, which uses the `JsonDecoder` of the target type:

```scala mdoc:silent
final case class Customer(name: String, email: String)

object Customer {
  implicit val decoder: JsonDecoder[Customer] = DeriveJsonDecoder.gen[Customer]
}

final case class LineItem(sku: String, qty: Int, price: BigDecimal)

object LineItem {
  implicit val decoder: JsonDecoder[LineItem] = DeriveJsonDecoder.gen[LineItem]
}
```

The customer object and the items array decode into domain types, while the rest of the payload stays untyped:

```scala mdoc
event.get(customer).flatMap(_.as[Customer])

event.get(items).flatMap(_.as[List[LineItem]])
```

If the sub-tree does not match, `Json#as` returns the usual decoder error. Its path starts at the sub-tree, not at the root of the document. Decoding the order object as a `Customer` fails on the first missing field:

```scala mdoc
event.get(order).flatMap(_.as[Customer])
```

## Update a Value

`Json#transformAt` replaces the node at a cursor with the result of a function. The function receives the node with the type the cursor ends on, so with `email` it receives a `Json.Str`. To mask the customer's email before forwarding the event:

```scala mdoc:silent
val masked: Either[String, Json] = event.transformAt(email)(_ => Json.Str("***"))
```

The email in the new document is masked:

```scala mdoc
masked.flatMap(_.get(email)).map(_.value)
```

`Json` values are immutable: `Json#transformAt` and the other editing methods return a new document and leave `event` unchanged. They return an `Either` because the cursor may not match. If the field is missing, or the type step does not match, the result is a `Left` with the same messages as `Json#get`.

## Delete a Field

`Json#delete` removes the node at a cursor. To drop the `debug` flag from `metadata`:

```scala mdoc
println(
  event
    .delete(JsonCursor.field("metadata").isObject.field("debug"))
    .flatMap(_.get(JsonCursor.field("metadata")))
)
```

:::warning
`Json#delete` does not check a type step that comes after the last field or element step. A cursor ending in `field("debug").isString` still deletes `debug`, although it is a boolean. Type steps earlier in the path are checked. If the type matters, check it with `Json#get` before deleting.
:::

## Move a Field with `Json#relocate`

`Json#relocate(from, to)` copies the node at `from` over the node at `to`, then deletes the node at `from`. Our downstream service expects `customer` at the top level of the event, not inside `data.order`.

The target must already exist. Relocating to a key that is not in the document fails with `No such field`:

```scala mdoc
event.relocate(customer, JsonCursor.field("customer"))
```

To move a node to a new key, add a placeholder first. `JsonCursor.isObject` points at the root object, so `Json#transformAt` with it can add a field through `Json.Obj#add`. Then `Json#relocate` replaces the placeholder:

```scala mdoc:silent
val moved: Either[String, Json] =
  event
    .transformAt(JsonCursor.isObject)(_.add("customer", Json.Null))
    .flatMap(_.relocate(customer, JsonCursor.field("customer")))
```

The customer object is now at the top level, and `data.order` no longer contains it:

```scala mdoc
println(moved.flatMap(_.get(JsonCursor.field("customer"))))

moved.flatMap(_.get(order)).map(_.keys)
```

## Fill In Defaults with `Json#merge`

`Json#merge` combines two documents. When both sides are objects, the result has the fields of both. A key on both sides is merged again one level down, so nested objects combine. Two arrays are merged element by element, and the extra elements of the longer array are kept. For any other pair of values, the right-hand side wins. Merging the event into a document of defaults keeps every value the event has and adds only the missing ones:

```scala mdoc:silent
val defaults: Json = Json.Obj(
  "metadata" -> Json.Obj("source" -> Json.Str("unknown"), "retries" -> Json.Num(0)),
  "currency" -> Json.Str("EUR")
)
```

The event's `source` wins over the default, while `retries` and `currency` come from the defaults:

```scala mdoc
println(defaults.merge(event).get(JsonCursor.field("metadata")))

defaults.merge(event).get(JsonCursor.field("currency").isString).map(_.value)
```

The keys of the left-hand document come first in the result. Put the defaults on the right (`event.merge(defaults)`) only when the defaults should overwrite the event.

## Put the Edits Together

Every editing step returns an `Either`, so a for-comprehension chains them and stops at the first error. The `clean` function applies all the edits of this guide to a raw payload:

```scala mdoc:silent
def clean(raw: String): Either[String, Json] =
  for {
    json      <- raw.fromJson[Json]
    masked    <- json.transformAt(email)(_ => Json.Str("***"))
    noDebug   <- masked.delete(JsonCursor.field("metadata").isObject.field("debug"))
    withSlot  <- noDebug.transformAt(JsonCursor.isObject)(_.add("customer", Json.Null))
    relocated <- withSlot.relocate(customer, JsonCursor.field("customer"))
  } yield defaults.merge(relocated)
```

`toJsonPretty` renders a `Json` value with indentation, and `toJson` renders it on one line. To print the cleaned payload:

```scala mdoc
clean(payload).foreach(json => println(json.toJsonPretty))
```

A payload that does not match fails with the error of the first step that did not match. This one has no `customer`:

```scala mdoc
clean("""{"data": {"order": {"id": 7}}, "metadata": {}}""")
```

## Going Further

- The [JSON AST](../reference/json-ast.md) reference covers the other ways to read and change a document: the `as*` methods, the `Json.Obj` field methods, `transformDown` and `transformUp` for changing every node of a kind, `foldDown` and `foldUp`, `intersect`, and composing cursors with `>>>`.
- To keep a free-form part of a typed model, give a case class a field of type `Json` or `Json.Obj`, as shown in [Embedding Raw JSON in Case Classes](../reference/json-ast.md#embedding-raw-json-in-case-classes).
- To read a configuration file written in YAML into the same `Json` type, see [Load a YAML Configuration File](load-yaml-config.md).
