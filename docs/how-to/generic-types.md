---
id: generic-types
title: "Derive Codecs for Generic Types"
description: "Derive ZIO JSON codecs for type-parameterised case classes and sealed traits on Scala 2 and Scala 3, including recursive ones."
keywords:
  - "Generic Case Class"
  - "Type Parameters"
  - "Implicit Def Codec"
  - "Recursive Types"
  - "DeriveJsonCodec"
---

Goal: derive one codec that works for every element type of a generic case class or sealed trait, such as `Box[A]`, `Page[A]` or `Result[A]`.

By the end of this guide you will have codecs for a generic wrapper, a generic page of results, a generic sealed trait and a generic tree, on Scala 2 and on Scala 3. We start with one type parameter and then add separate encoders and decoders, sealed families and recursion.

## The Problem

A codec for a non-generic type is a single value: `implicit val codec: JsonCodec[User]`. A generic type has no single codec, because the JSON for `Box[Int]` depends on how an `Int` is written, and the JSON for `Box[User]` depends on how a `User` is written. Writing a separate `implicit val` for `Box[Int]`, `Box[String]` and every other element type you use does not scale, and it breaks as soon as a new element type appears.

The fix is a codec that is a function of the element codecs: an `implicit def` (Scala 2) or a `given` with type parameters (Scala 3) that asks for the instances of `A` and derives the instance of `Box[A]` from them.

## What You Need

Only the core module:

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "@VERSION@"
```

The Scala 2 examples on this page use this import:

```scala mdoc:silent:reset-object
import zio.json._
```

The guide assumes you know how to derive a codec for a plain case class (see the [Getting Started](../tutorial/getting-started.md) tutorial). Higher-kinded types, such as a case class with a parameter `F[_]`, are not covered here.

## 1. Derive a Codec for a Generic Case Class

Put an `implicit def` in the companion object. Its context bounds ask for a `JsonEncoder[A]` and a `JsonDecoder[A]`, and its body derives the codec of `Box[A]` with them:

```scala mdoc:silent
final case class Box[A](value: A)

object Box {
  implicit def codec[A: JsonEncoder: JsonDecoder]: JsonCodec[Box[A]] =
    DeriveJsonCodec.gen[Box[A]]
}
```

Any element type that has an encoder and a decoder now works, including another `Box`:

```scala mdoc
Box(42).toJson
Box(List("a", "b")).toJson
"""{"value":{"value":true}}""".fromJson[Box[Box[Boolean]]]
```

Ask for `JsonEncoder[A]` and `JsonDecoder[A]` rather than `JsonCodec[A]`. A `JsonCodec[A]` also provides both, but the separate bounds also accept element types that have two separate instances and no codec, such as `Sku` here:

```scala mdoc:silent
final case class Sku(code: String)

object Sku {
  implicit val encoder: JsonEncoder[Sku] = DeriveJsonEncoder.gen[Sku]
  implicit val decoder: JsonDecoder[Sku] = DeriveJsonDecoder.gen[Sku]
}
```

`Box[Sku]` finds both instances of `Sku` through the bounds:

```scala mdoc
Box(Sku("A-1")).toJson
```

With more than one type parameter, add bounds for each of them:

```scala mdoc:silent
final case class Pair[A, B](first: A, second: B)

object Pair {
  implicit def codec[A: JsonEncoder: JsonDecoder, B: JsonEncoder: JsonDecoder]: JsonCodec[Pair[A, B]] =
    DeriveJsonCodec.gen[Pair[A, B]]
}
```

Encoding a `Pair[String, Int]` uses both bounds:

```scala mdoc
Pair("id", 7).toJson
```

A variance annotation such as `Box[+A]` is fine here, because the type parameter of the `implicit def` is a new, invariant one. The `@jsonDerive` annotation from `zio-json-macros` cannot do this: it copies the variance annotation into the generated method, and `@jsonDerive final case class Box[+A](value: A)` does not compile. [zio-json-macros](../reference/zio-json-macros.md#generic-classes) shows the error.

## 2. Derive Only an Encoder or a Decoder

A codec needs an encoder and a decoder for `A`. When some element types can only be written, for example events you send but never read, derive the two sides separately. Each side asks only for what it needs:

```scala mdoc:silent
final case class Page[A](items: List[A], total: Int)

object Page {
  implicit def encoder[A: JsonEncoder]: JsonEncoder[Page[A]] = DeriveJsonEncoder.gen[Page[A]]
  implicit def decoder[A: JsonDecoder]: JsonDecoder[Page[A]] = DeriveJsonDecoder.gen[Page[A]]
}
```

`AuditEvent` has an encoder and no decoder:

```scala mdoc:silent
final case class AuditEvent(action: String)

object AuditEvent {
  implicit val encoder: JsonEncoder[AuditEvent] = DeriveJsonEncoder.gen[AuditEvent]
}
```

A `Page[AuditEvent]` can still be encoded, and a `Page[Int]` can be decoded:

```scala mdoc
Page(List(AuditEvent("login")), 1).toJson
"""{"items":[1,2],"total":2}""".fromJson[Page[Int]]
```

The `Box` codec from the previous section cannot do this, because it needs both instances of `AuditEvent` even to encode:

```scala mdoc:fail
Box(AuditEvent("login")).toJson
```

## 3. Derive a Generic Sealed Trait

A generic sealed trait works the same way: the `implicit def` goes in the companion of the trait, and the cases are derived along with it. Cases that do not use the type parameter can extend `Result[Nothing]`, which the variance annotation `+A` allows:

```scala mdoc:silent
sealed trait Result[+A]
final case class Ok[A](value: A)        extends Result[A]
final case class Failed(reason: String) extends Result[Nothing]
case object Pending                     extends Result[Nothing]

object Result {
  implicit def codec[A: JsonEncoder: JsonDecoder]: JsonCodec[Result[A]] =
    DeriveJsonCodec.gen[Result[A]]
}
```

Each case is written with its name as the key, as for any sealed trait:

```scala mdoc
(Ok(1): Result[Int]).toJson
(Failed("timeout"): Result[Int]).toJson
(Pending: Result[Int]).toJson
"""{"Ok":{"value":"done"}}""".fromJson[Result[String]]
```

`Pending` is written as `{"Pending":{}}` because `enumValuesAsStrings` is off by default on Scala 2. [Sealed traits](sealed-traits.md) shows how to change the keys and how to use a discriminator field.

:::warning[Scala 3: a case class that extends `Result[Nothing]` crashes the compiler]
On Scala 3.9.0, deriving `Result` fails with a compiler crash (`java.lang.AssertionError: assertion failed: TypeRef(..., class Failed)`) when the family has a case class that does not take the type parameter, such as `Failed` above. Case objects are fine. Give such a case class the type parameter of the trait, `final case class Failed[A](reason: String) extends Result[A]`, as the Scala 3 example below does.
:::

## 4. Derive a Recursive Generic Type

A tree whose children are trees of the same type refers to its own codec. On Scala 2, the plain `implicit def` from the first section compiles, but it calls itself without end when the codec is built:

```scala mdoc:compile-only
final case class BrokenTree[A](value: A, children: List[BrokenTree[A]])

object BrokenTree {
  implicit def codec[A: JsonEncoder: JsonDecoder]: JsonCodec[BrokenTree[A]] =
    DeriveJsonCodec.gen[BrokenTree[A]]
}
```

Encoding any `BrokenTree`, even one without children, throws a `java.lang.StackOverflowError`. The derived codec looks up the codec for `List[BrokenTree[A]]`, which calls `BrokenTree.codec[A]` again, which derives a new codec, and so on.

Inside the method, derive the codec once and store it in a local `implicit lazy val`. The derivation then finds that value for the children instead of calling the method again:

```scala mdoc:silent
final case class Tree[A](value: A, children: List[Tree[A]])

object Tree {
  implicit def codec[A: JsonEncoder: JsonDecoder]: JsonCodec[Tree[A]] = {
    implicit lazy val self: JsonCodec[Tree[A]] = DeriveJsonCodec.gen[Tree[A]]
    self
  }
}
```

The tree now encodes and decodes at any depth:

```scala mdoc
Tree(1, List(Tree(2, Nil))).toJson
"""{"value":1,"children":[{"value":2,"children":[]}]}""".fromJson[Tree[Int]]
```

A recursive type without type parameters has the same problem on Scala 2 in another form: `implicit val codec: JsonCodec[Node] = DeriveJsonCodec.gen[Node]` fails at runtime with a `NullPointerException` inside an `ExceptionInInitializerError`. Declare it as an `implicit lazy val`, or wrap the derived instance in `JsonEncoder.suspend` and `JsonDecoder.suspend` as shown in [JsonEncoder](../reference/json-encoder.md#recursive-encoders). Scala 3 needs none of this, as the next section shows.

## 5. Derive Generic Types on Scala 3

On Scala 3, a `derives` clause on a generic case class does all of the above in one line:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/generictypes/GenericTypesExample.scala", "derives")
```

The given that `derives JsonCodec` generates asks for a `JsonCodec[A]`. When an element type has only a separate encoder and decoder, write the given yourself, with the same bounds as the Scala 2 `implicit def`:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/generictypes/GenericTypesExample.scala", "given")
```

Separate encoder and decoder givens take one bound each:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/generictypes/GenericTypesExample.scala", "separate")
```

A generic sealed trait derives with `derives JsonCodec` on the trait. Every case class takes the type parameter, which avoids the compiler crash described in the warning above. A case object can still extend `Result[Nothing]`:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/generictypes/GenericTypesExample.scala", "sealed")
```

A recursive generic type needs no `lazy val` on Scala 3:

```scala mdoc:passthrough
docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/generictypes/GenericTypesExample.scala", "recursive")
```

Running the example file on Scala 3.9.0 prints:

```text
Box: {"value":"x"}
Page: {"items":[1,2],"total":2}
Page decoded: Right(Page(List(a),1))
Delta: {"before":1,"after":2}
Ok: {"Ok":{"value":1}}
Failed: {"Failed":{"reason":"timeout"}}
Pending: {"Pending":{}}
Result decoded: Right(Failed(timeout))
Tree: {"value":1,"children":[{"value":2,"children":[]}]}
Tree decoded: Right(Tree(1,List(Tree(2,List()))))
```

Deriving a generic sealed trait on Scala 3 also prints an unchecked warning (E092) for the generated code. For the example above it reads `the type test for ...GenericTypesExample.Ok[A] cannot be checked at runtime because its type arguments can't be determined from ...GenericTypesExample.Result[A]`, with the full package names in place of the dots. The warning is harmless, but it fails a build that turns warnings into errors (`-Werror`). [Derive codecs on Scala 3](scala-3-derivation.md#6-derive-codecs-for-generic-case-classes) shows the compile error you get when an element type has no `JsonCodec`.

## See Also

- [Derive codecs on Scala 3](scala-3-derivation.md) covers `derives`, enums, union types and opaque types.
- [Sealed traits](sealed-traits.md) covers type hints and discriminator fields for any sealed family.
- [zio-json-macros](../reference/zio-json-macros.md#generic-classes) shows what `@jsonDerive` generates for a generic class on Scala 2.
- [Scala 2 and Scala 3 differences](../explanation/scala-2-vs-scala-3.md) lists the recursion and generic sealed trait differences together with the others.
