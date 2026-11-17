---
id: generate-case-classes
title: "Generate Case Classes from Sample JSON"
description: "Print a first draft of case classes and codecs from a sample JSON document with Generator.printCaseClasses, and know its limits."
keywords:
  - "Code Generation"
  - "Case Classes"
  - "Sample JSON"
  - "Generator"
  - "printCaseClasses"
---

In this guide, we turn a sample JSON document, such as a response copied from an API's documentation, into Scala case classes with derived codecs. We use `zio.json.codegen.Generator.printCaseClasses`, a developer utility that you run once in a REPL or a small program. It prints Scala source code, which you copy into your project and adjust. By the end, you will have a working model for the sample, and you will know which inputs the generator cannot handle.

We cover these tasks in order:

- Run the generator on a sample document.
- Read its output and the types it chooses.
- Turn the output into code for your project.
- Avoid the inputs that make it fail or print invalid code.

## The Problem

To consume a third-party JSON API, you need case classes that match its responses. A real response has dozens of fields and several levels of nesting. Writing the case classes by hand from a sample means checking every field: whether a number fits in an `Int` or needs a `Long`, which strings are dates, which fields are sometimes `null` or missing, and which nested objects need their own case class. A mistake shows up only when a response fails to decode.

The generator reads a sample and does this first pass for you. It is a starting point, not a build step: the result needs review, and the generator is not meant to run as part of your build.

## Prerequisites

`Generator` is part of the core `zio-json` module, so there is nothing else to add:

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

The model at the end of this guide uses the usual import for codecs:

```scala mdoc:silent:reset-object
import zio.json._
```

The generated code uses `DeriveJsonCodec`, described in [JsonCodec](../reference/json-codec.md).

## Run the Generator

`Generator.printCaseClasses(input: String): Unit` parses its argument and prints the generated code to standard output. It returns nothing, so call it where you can read the output: a REPL, a worksheet, or a small `main` method. With [Scala CLI](https://scala-cli.virtuslab.org/), start a REPL that has `zio-json` on the classpath:

```bash
scala-cli repl --dep dev.zio::zio-json:@VERSION@
```

Then paste the sample into a call to `Generator.printCaseClasses`. The same code works as the body of a `main` method in a scratch file. Our sample is an order from a shop's API:

```scala mdoc:compile-only
import zio.json.codegen.Generator

Generator.printCaseClasses(
  """{
    |  "id": 42,
    |  "orderNumber": 9876543210,
    |  "placedAt": "2024-05-01T10:15:30",
    |  "deliveryDate": "2024-05-03",
    |  "trackingId": "3f1c2a9e-8b7d-4c6e-9a1b-2d3e4f5a6b7c",
    |  "total": 28.5,
    |  "paid": true,
    |  "customer": { "name": "Ada", "email": "ada@example.com" },
    |  "line_items": [
    |    { "sku": "BOOK-1", "qty": 2, "price": 12.5, "note": null },
    |    { "sku": "PEN-7", "qty": 1, "price": 3, "note": "gift", "giftWrap": true }
    |  ],
    |  "tags": ["new", "priority"]
    |}""".stripMargin
)
```

If the argument is not valid JSON, `Generator.printCaseClasses` prints `Invalid JSON: ` followed by the input, instead of failing.

## Read the Output

The call prints the following code. It is the real output of the call above, with the colour codes removed (see the note after it):

```scala title="Printed output"
final case class RootObject(
  id: Int,
  orderNumber: Long,
  placedAt: java.time.LocalDateTime,
  deliveryDate: java.time.LocalDate,
  trackingId: java.util.UUID,
  total: Double,
  paid: Boolean,
  customer: Customer,
  line_items: List[LineItems],
  tags: List[String]
)

object RootObject {
  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
}

final case class Customer(
  name: String,
  email: String
)

object Customer {
  implicit val codec: JsonCodec[Customer] = DeriveJsonCodec.gen
}

final case class LineItems(
  sku: String,
  qty: Int,
  price: Double,
  note: Option[String],
  giftWrap: Option[Boolean]
)

object LineItems {
  implicit val codec: JsonCodec[LineItems] = DeriveJsonCodec.gen
}
```

:::note
`Generator.printCaseClasses` wraps the code in ANSI colour codes, so it shows in cyan in a terminal. If you redirect the output to a file, the file starts with the escape sequence `ESC[36m` and ends with `ESC[0m`. Remove them before compiling the file.
:::

The generator prints one case class for each JSON object, with a companion object that derives a `JsonCodec`. It does not print a package or an `import zio.json._` line. The root object is always called `RootObject`. A nested object is named after its key in PascalCase: `customer` becomes `Customer`, and the key is split at `_`, `-` and `.`, so the elements of `line_items` become `LineItems`. If the root of the sample is an array of objects, the generated class for its elements is called `RootObject`.

The type of each field comes from the value in the sample:

| JSON value in the sample                              | Scala type                |
|-------------------------------------------------------|---------------------------|
| `true` or `false`                                     | `Boolean`                 |
| An integer in the `Int` range                         | `Int`                     |
| An integer in the `Long` range                        | `Long`                    |
| Any other number up to `Double.MaxValue`              | `Double`                  |
| A number larger than `Double.MaxValue`                | `BigDecimal`              |
| A string in ISO date-time format                      | `java.time.LocalDateTime` |
| A string in ISO date format (`2024-05-03`)            | `java.time.LocalDate`     |
| A string in UUID format                               | `java.util.UUID`          |
| Any other string                                      | `String`                  |
| An object                                             | A new case class          |
| An array                                              | `List[A]`                 |

For an array, the generator combines the types of all elements into one element type. Numbers widen: `Int` and `Long` give `Long`, an integer and a `Double` give `Double`, and anything with a `BigDecimal` gives `BigDecimal`. That is why `price` is a `Double`: the sample has `12.5` and `3`. For an array of objects, the fields of all the objects are combined. A field that is `null` in one element and has a value in another becomes an `Option`, like `note`, and a field that is missing from some elements becomes an `Option` too, like `giftWrap`.

## Turn the Output into Project Code

The output compiles once `import zio.json._` is added, and it decodes the sample. Before you commit it, review the names and types. For our order, we give the classes domain names, use `BigDecimal` for money, which should not be a `Double`, and rename `line_items` to `lineItems` with `@jsonField`, so the Scala name follows Scala conventions while the JSON key stays the same:

```scala mdoc:silent
final case class Order(
  id: Int,
  orderNumber: Long,
  placedAt: java.time.LocalDateTime,
  deliveryDate: java.time.LocalDate,
  trackingId: java.util.UUID,
  total: BigDecimal,
  paid: Boolean,
  customer: Customer,
  @jsonField("line_items") lineItems: List[LineItem],
  tags: List[String]
)

object Order {
  implicit val codec: JsonCodec[Order] = DeriveJsonCodec.gen
}

final case class Customer(name: String, email: String)

object Customer {
  implicit val codec: JsonCodec[Customer] = DeriveJsonCodec.gen
}

final case class LineItem(
  sku: String,
  qty: Int,
  price: BigDecimal,
  note: Option[String],
  giftWrap: Option[Boolean]
)

object LineItem {
  implicit val codec: JsonCodec[LineItem] = DeriveJsonCodec.gen
}
```

The sample now decodes into the model:

```scala mdoc
"""{
  |  "id": 42,
  |  "orderNumber": 9876543210,
  |  "placedAt": "2024-05-01T10:15:30",
  |  "deliveryDate": "2024-05-03",
  |  "trackingId": "3f1c2a9e-8b7d-4c6e-9a1b-2d3e4f5a6b7c",
  |  "total": 28.5,
  |  "paid": true,
  |  "customer": { "name": "Ada", "email": "ada@example.com" },
  |  "line_items": [
  |    { "sku": "BOOK-1", "qty": 2, "price": 12.5, "note": null },
  |    { "sku": "PEN-7", "qty": 1, "price": 3, "note": "gift", "giftWrap": true }
  |  ],
  |  "tags": ["new", "priority"]
  |}""".stripMargin.fromJson[Order]
```

Check the types against the API's documentation as well. A single sample cannot show that a field the sample always fills can be `null`, or that an ID that fits in an `Int` today can grow beyond it. To learn more about renaming fields, see [Field names](field-names.md).

## Limitations

The generator is a small utility with no options, and several inputs make it fail or print code that does not compile. Fix the sample before you run it, or fix the output by hand:

- An empty array anywhere in the sample throws `java.lang.UnsupportedOperationException: empty.reduceLeft`, because there is no element to take a type from. Put one representative element in each array.
- An array whose elements have types that cannot be combined, such as `[1, "x"]`, throws an exception with the message `Cannot combine:` followed by the two types (`LEFT: JInt`, `RIGHT: JString`). The same happens when a field holds a number in one object of an array and a string in another.
- A field that is `null` in the sample and never has a value gets the type `null`, which is not a valid Scala type. Replace it with an `Option` of the right type.
- Field names are copied unchanged. A key such as `first-name` or `type` produces a field that does not compile. Rename the field and keep the JSON key with `@jsonField("first-name")`, or quote it with backticks.
- Two nested objects with the same key in different places produce two case classes with the same name, even when their fields differ. Rename one of them.
- An integer outside the `Long` range becomes a `Double`, which loses precision. Use `BigInt` or `BigDecimal` instead.
- A date-time with an offset, such as `2024-05-01T10:15:30Z`, becomes a `java.time.LocalDateTime`, but the `LocalDateTime` decoder rejects the offset. Change the type to `java.time.OffsetDateTime` or `java.time.Instant`.
- The generated code uses Scala 2 syntax (`implicit val`), which also compiles on Scala 3. The generator's tests run only on Scala 2.12 and 2.13.

For example, a sample whose keys are not valid Scala identifiers:

```scala mdoc:compile-only
import zio.json.codegen.Generator

Generator.printCaseClasses("""{"first-name": "Ada", "type": "user"}""")
```

The call prints this code, which does not compile (colour codes removed):

```scala title="Printed output"
final case class RootObject(
  first-name: String,
  type: String
)

object RootObject {
  implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
}
```

Rename `first-name` with `@jsonField`, and quote `type` with backticks. Both fields keep their JSON keys:

```scala mdoc
final case class Account(@jsonField("first-name") firstName: String, `type`: String)

object Account {
  implicit val codec: JsonCodec[Account] = DeriveJsonCodec.gen
}

"""{"first-name": "Ada", "type": "user"}""".fromJson[Account]
```

## Going Further

- [JsonCodec](../reference/json-codec.md) describes the derived codecs the generator writes into each companion object.
- [Field names](field-names.md) shows how to rename fields and apply a naming convention such as snake_case to a whole class.
- [Optional, default and null fields](optional-and-default-fields.md) explains how `Option` fields and default values decode, which helps when you adjust the generated types.
- When you only need a few values from a document and do not want a model at all, read them with cursors as shown in [Read and Edit JSON of Unknown Shape](edit-json-with-cursors.md).
