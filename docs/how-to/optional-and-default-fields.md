---
id: optional-and-default-fields
title: "Optional, default and null fields"
description: "Decide how ZIO JSON treats missing, null and empty fields: Option, default values, explicit nulls, empty collections and extra fields."
keywords:
  - "Optional Fields"
  - "Default Field Values"
  - "Explicit Nulls"
  - "Empty Collections"
  - "Extra Fields"
---

Goal: control what happens when a JSON field is missing, `null`, empty or unexpected.

## What you need

```scala mdoc:silent
import zio.json._
```

## 1. Know the default behavior

When the decoder does not find a field, it picks the result by the type of the field:

| Field type                          | Missing field decodes to                        |
|-------------------------------------|-------------------------------------------------|
| `Option[A]`                         | `None`                                          |
| Field with a default value          | The default value                               |
| Collection such as `List` or `Map`  | An error, unless `decoding = false` (section 3) |
| Anything else                       | An error: `missing`                             |

A field with a default value also takes its default when the JSON value is `null`. On Scala 3, defaults need `-Yretain-trees` in `scalacOptions`.

```scala mdoc:silent
case class Profile(name: String, nickname: Option[String], age: Int = 18)

implicit val profileCodec: JsonCodec[Profile] = DeriveJsonCodec.gen[Profile]
```

The codec of a class with default values is a plain `implicit val` here, not a member of the companion. These examples are compiled inside a wrapper object, and there a companion codec for a class with defaults fails with a `StackOverflowError`. In a normal top-level class, a companion codec works too.

```scala mdoc
"""{"name":"Ann"}""".fromJson[Profile]
"""{"name":"Ann","age":null}""".fromJson[Profile]
```

A missing field that is not an `Option` and has no default is an error:

```scala mdoc
"""{"nickname":"A"}""".fromJson[Profile]
```

When encoding, `None` is left out of the output by default:

```scala mdoc
println(Profile("Ann", None).toJson)
```

## 2. Write nulls with `@jsonExplicitNull`

Put `@jsonExplicitNull` on a case class to write `None` as `null`. Put it on a single field to do that for that field only. Missing nulls are always accepted when decoding.

```scala mdoc:silent
@jsonExplicitNull
case class Mango(ripeness: Option[Int])

object Mango {
  implicit val codec: JsonCodec[Mango] = DeriveJsonCodec.gen[Mango]
}

case class Papaya(@jsonExplicitNull ripeness: Option[Int], color: Option[String])

object Papaya {
  implicit val codec: JsonCodec[Papaya] = DeriveJsonCodec.gen[Papaya]
}
```

```scala mdoc
println(Mango(None).toJson)
println(Papaya(None, None).toJson)
"""{}""".fromJson[Mango]
```

The program-wide form is `JsonCodecConfiguration(explicitNulls = true)`. It is `false` by default.

## 3. Omit empty collections with `@jsonExplicitEmptyCollections`

By default, an empty collection is written as `[]` or `{}`, and the decoder requires the key to be present. The annotation turns each direction off separately:

```scala mdoc:silent
@jsonExplicitEmptyCollections(encoding = false, decoding = false)
case class Pineapple(leaves: List[String])

object Pineapple {
  implicit val codec: JsonCodec[Pineapple] = DeriveJsonCodec.gen[Pineapple]
}
```

Now an empty list is left out, and a missing key decodes to an empty list:

```scala mdoc
println(Pineapple(Nil).toJson)
"""{}""".fromJson[Pineapple]
```

You can mix the two flags, for example `@jsonExplicitEmptyCollections(encoding = true, decoding = false)`. On a field, the annotation changes encoding only. Decoding reads the annotation on the class.

The program-wide form is `JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(encoding = false, decoding = false))`. Both flags are `true` by default.

## 4. Reject unexpected fields with `@jsonNoExtraFields`

By default, the decoder ignores fields it does not know. Put `@jsonNoExtraFields` on a case class to make them an error:

```scala mdoc:silent
@jsonNoExtraFields
case class Watermelon(pips: Int)

object Watermelon {
  implicit val decoder: JsonDecoder[Watermelon] = DeriveJsonDecoder.gen[Watermelon]
}
```

```scala mdoc
"""{"pips":32}""".fromJson[Watermelon]
"""{"pips":32,"color":"yellow"}""".fromJson[Watermelon]
```

The program-wide form is `JsonCodecConfiguration(allowExtraFields = false)`. The annotation does not combine with `@jsonDiscriminator`, see [Sealed traits](sealed-traits.md).

## 5. Keep a field out of the JSON with `@jsonExclude`

Put `@jsonExclude` on a field to leave it out of the encoded JSON. Use it for values that stay in memory, such as secrets or cached data.

```scala mdoc:silent
case class Session(user: String, @jsonExclude token: String)

object Session {
  implicit val codec: JsonCodec[Session] = DeriveJsonCodec.gen[Session]
}
```

The encoder skips the field:

```scala mdoc
println(Session("alice", "secret").toJson)
```

The decoder does not skip it. If the JSON has the field, the value is decoded. If not, decoding fails, as for any other missing field:

```scala mdoc
"""{"user":"alice","token":"abc"}""".fromJson[Session]
"""{"user":"alice"}""".fromJson[Session]
```

Give the field a default value to make it optional when decoding:

```scala mdoc:silent
case class SessionWithDefault(user: String, @jsonExclude token: String = "")

implicit val sessionWithDefaultCodec: JsonCodec[SessionWithDefault] =
  DeriveJsonCodec.gen[SessionWithDefault]
```

```scala mdoc
"""{"user":"alice"}""".fromJson[SessionWithDefault]
```

## See also

- [Annotations](../reference/annotations.md) lists every derivation annotation in one table.
- [Decoding](decoding.md) shows default values in a recursive case class.
- [Field names](field-names.md) covers `@jsonField` and `@jsonAliases`.
