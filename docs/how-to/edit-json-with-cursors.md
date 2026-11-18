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

Goal: read a few values from a JSON document you have no case class for, such as a webhook payload, and forward a changed copy of it without losing the fields you do not model.

Use this guide when decoding into case classes would force you to model every level of nesting, or would drop the fields your case classes do not declare when you encode the document again. The `Json` AST keeps the whole document, and a `JsonCursor` points at one node inside it.

Step 1 parses the document, and step 2 defines the cursors the other steps use. Steps 3 to 8 are separate options that each start from the parsed document, so use the ones you need. Step 9 chains several edits into one function.

## What You Need

The AST and cursors are part of the core `zio-json` module:

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

All code on this page uses these imports:

```scala mdoc:silent:reset-object
import zio.json._
import zio.json.ast.{ Json, JsonCursor }
```

The [JSON AST](../reference/json-ast.md) reference page lists every method, case and error message in detail.

## 1. Parse the Payload into `Json`

The example payload is an `order.paid` event from a payment provider. The order is nested two levels down, and `metadata` holds information we do not want to forward:

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

Decode it with `Json` as the target type. This accepts any valid JSON and returns `Either[String, Json]`, with a `Left` only when the text is not valid JSON:

```scala mdoc:silent
val parsed: Either[String, Json] = payload.fromJson[Json]
```

To keep the next steps short, they work on the value taken out of the `Either`. In real code, stay inside the `Either`, as step 9 does:

```scala mdoc:silent
val event: Json = parsed.fold(error => sys.error(error), identity)
```

## 2. Build Cursors for the Values You Need

A cursor is a path into a document. `JsonCursor#field` goes into a field of an object, `JsonCursor#element` goes into an element of an array, and the type steps (`JsonCursor#isObject`, `JsonCursor#isArray`, `JsonCursor#isString`, `JsonCursor#isNumber`, `JsonCursor#isBool` and `JsonCursor#isNull`) check the type of the current node. The `JsonCursor` companion has the same methods to start a path. Put `JsonCursor#isObject` before each deeper field step, and `JsonCursor#isArray` before an element step. Define the paths once and reuse them:

```scala mdoc:silent
val order    = JsonCursor.field("data").isObject.field("order").isObject
val customer = order.field("customer").isObject
val email    = customer.field("email").isString
val items    = order.field("items").isArray
val total    = order.field("total").isNumber
```

Without the `JsonCursor#isObject` step, the path does not compile, because `JsonCursor#field` needs a cursor that ends on a `Json.Obj`:

```scala mdoc:fail
JsonCursor.field("data").field("order")
```

`Json#get` follows a cursor and returns the node it ends on. A cursor that ends with a type step returns that case of `Json`, so with `JsonCursor#isString` you get a `Json.Str`, and `Json.Str#value` gives the `String`. For a number, `Json.Num#value` is a `java.math.BigDecimal`. To read the event type, the customer's email and the order total from `event` (the payload parsed in step 1):

```scala mdoc
event.get(JsonCursor.field("type").isString).map(_.value)

event.get(email).map(_.value)

event.get(total).map(_.value)
```

To read an element of an array, add `JsonCursor#element` with a zero-based index after a `JsonCursor#isArray` step:

```scala mdoc
event.get(items.element(0).isObject.field("sku").isString).map(_.value)
```

The [Cursors](../reference/json-ast.md#cursors) reference section covers the cursor type and its other ways to build a path.

## 3. Handle Missing Fields and Wrong Types

`Json#get` returns a `Left` when the document does not match the cursor. Here `event` is the parsed payload from step 1, and `order` and `items` are the cursors to `data.order` and `data.order.items` from step 2. A field that does not exist gives `No such field`:

```scala mdoc
event.get(order.field("coupon"))
```

A type step that does not match gives `expected <type>`. The order `id` is a number, not a string:

```scala mdoc
event.get(order.field("id").isString)
```

An index past the end of an array has its own message:

```scala mdoc
event.get(items.element(5))
```

The messages name only the last step that failed, not the whole path. When you read several values, add the path to the error yourself, for example with `left.map(error => s"data.order.id: $error")`. The full list of messages is in [Cursor Errors](../reference/json-ast.md#cursor-errors).

## 4. Decode Part of the Document into a Case Class

When one part of the document has a stable shape, decode just that part. Define case classes with a `JsonDecoder` for the parts you need:

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

Get the sub-tree with a cursor, then call `Json#as`, which uses the `JsonDecoder` of the target type. With `customer` and `items` (the cursors from step 2) and `event` (the payload from step 1), the customer object and the items array decode into domain types, while the rest of the payload stays untyped:

```scala mdoc
event.get(customer).flatMap(_.as[Customer])

event.get(items).flatMap(_.as[List[LineItem]])
```

If the sub-tree does not match, `Json#as` returns the usual decoder error, with a path that starts at the sub-tree, not at the root of the document. Decoding the order object as a `Customer` fails on the first missing field:

```scala mdoc
event.get(order).flatMap(_.as[Customer])
```

## 5. Replace a Value

`Json#transformAt` replaces the node at a cursor with the result of a function. The function receives the node with the type the cursor ends on, so with `email` (the cursor to `data.order.customer.email` from step 2) it receives a `Json.Str`. To mask the customer's email in `event`, the payload from step 1:

```scala mdoc:silent
val masked: Either[String, Json] = event.transformAt(email)(_ => Json.Str("***"))
```

The email in the new document is masked:

```scala mdoc
masked.flatMap(_.get(email)).map(_.value)
```

:::note
`Json` values are immutable: `Json#transformAt` and the other editing methods return a new document and leave `event` unchanged. They return an `Either` because the cursor may not match, with the same messages as `Json#get`.
:::

## 6. Delete a Field

`Json#delete` removes the node at a cursor. To drop the `debug` flag from `metadata` in `event`, the payload from step 1:

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

## 7. Move a Field to a New Key

`Json#relocate(from, to)` copies the node at `from` over the node at `to`, then deletes the node at `from`. The target must already exist. In this example, the downstream service expects `customer` at the top level of `event` (the payload from step 1), not inside `data.order`, and relocating `customer` (the cursor from step 2) to a key that is not in the document fails with `No such field`:

```scala mdoc
event.relocate(customer, JsonCursor.field("customer"))
```

To move a node to a new key, add a placeholder first, then relocate over it. `JsonCursor.isObject` points at the root object, so `Json#transformAt` with it can add a field through `Json.Obj#add`:

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

## 8. Fill In Missing Defaults with `Json#merge`

`Json#merge` combines two documents: objects are merged key by key, recursively, and for two scalar values the right-hand side wins. To add only the values the event lacks, write the defaults as a `Json` document:

```scala mdoc:silent
val defaults: Json = Json.Obj(
  "metadata" -> Json.Obj("source" -> Json.Str("unknown"), "retries" -> Json.Num(0)),
  "currency" -> Json.Str("EUR")
)
```

Then merge `event` (the payload from step 1) into the defaults, with the event on the right. The event's `source` wins over the default, while `retries` and `currency` come from the defaults:

```scala mdoc
println(defaults.merge(event).get(JsonCursor.field("metadata")))

defaults.merge(event).get(JsonCursor.field("currency").isString).map(_.value)
```

The keys of the left-hand document come first in the result. Put the defaults on the right (`event.merge(defaults)`) only when the defaults should overwrite the event. [Combining Documents](../reference/json-ast.md#combining-documents) describes how arrays and other pairs of values merge.

## 9. Chain the Edits in One Function

Every editing method returns an `Either`, so a for-comprehension chains them and stops at the first error. This `clean` function parses a raw payload and applies the edits of steps 5 to 8, using the cursors `email` and `customer` from step 2 and `defaults` from step 8:

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

## See Also

- [JSON AST](../reference/json-ast.md) for the other ways to read and change a document: the `as*` methods, the `Json.Obj` field methods, `transformDown` and `transformUp`, `foldDown` and `foldUp`, `intersect`, and composing cursors with `>>>`.
- [Embedding Raw JSON in Case Classes](../reference/json-ast.md#embedding-raw-json-in-case-classes) to keep a free-form part of a typed model in a field of type `Json` or `Json.Obj`.
- [Load a YAML Configuration File](load-yaml-config.md) to read YAML into the same `Json` type.
