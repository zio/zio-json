---
id: validate-on-decode
title: "Validate data while decoding"
description: "Reject bad values during decoding with mapOrFail and transformOrFail, read the error paths, and use refined types or strict objects."
keywords:
  - "JSON Validation"
  - "Decode Errors"
  - "mapOrFail"
  - "transformOrFail"
  - "Refined Types"
---

Goal: make decoding fail when a value is well-formed JSON but is not acceptable to your program, for example an age below zero or an email with no `@`.

The tool is `mapOrFail`. It runs a function on a decoded value. The function returns `Right` to accept or `Left` with a message to reject. The failure is part of decoding, so the bad value never reaches your code.

```scala mdoc:silent
import zio.json._
```

## 1. Validate one value type

Wrap the raw type in a small case class, then build its decoder from an existing decoder with `mapOrFail`.

```scala mdoc:silent
final case class Email(value: String)

object Email {
  implicit val decoder: JsonDecoder[Email] =
    JsonDecoder[String].mapOrFail { s =>
      if (s.contains("@")) Right(Email(s))
      else Left("email must contain @")
    }

  implicit val encoder: JsonEncoder[Email] =
    JsonEncoder[String].contramap(_.value)
}
```

`contramap` is the encoder side. It turns an `Email` back into a `String` so the JSON stays a plain string.

## 2. Validate both directions with transformOrFail

When you want encoder and decoder together, use a `JsonCodec` and `transformOrFail`. Its first function can fail. Its second function cannot.

```scala mdoc:silent
final case class Age(value: Int)

object Age {
  implicit val codec: JsonCodec[Age] =
    JsonCodec[Int].transformOrFail(
      n => if (n > 0) Right(Age(n)) else Left("age must be positive"),
      _.value
    )
}
```

## 3. Use the types in a case class

Fields of these types are checked as the case class is decoded.

```scala mdoc:silent
final case class Signup(name: String, age: Age, email: Email)

object Signup {
  implicit val decoder: JsonDecoder[Signup] = DeriveJsonDecoder.gen[Signup]
}
```

Valid input decodes as normal:

```scala mdoc
"""{"name":"Ada","age":36,"email":"ada@example.com"}""".fromJson[Signup]
```

## 4. Read the error

A rejected value produces a `Left`. The string starts with the path to the field, written as `.fieldName`, then your message in parentheses.

```scala mdoc
"""{"name":"Ada","age":-1,"email":"ada@example.com"}""".fromJson[Signup]

"""{"name":"Ada","age":36,"email":"ada.example.com"}""".fromJson[Signup]
```

When the value is not nested in a field, such as decoding an `Age` alone, the path is empty and only the parenthesised message remains.

```scala mdoc
"0".fromJson[Age]
```

## 5. Know that the first failure wins

Decoding stops at the first bad value. It does not collect a list of errors. Fields are read in the order they appear in the JSON text, so when two fields are bad, only the first one in the text is reported.

```scala mdoc
"""{"name":"Ada","age":-1,"email":"nope"}""".fromJson[Signup]

"""{"name":"Ada","email":"nope","age":-1}""".fromJson[Signup]
```

If you need every error at once, decode to the [`Json` AST](../reference/json-ast.md) or to plain types and run your own validation after decoding.

## 6. Check a rule across fields

`mapOrFail` also works on a whole object. Use it for a rule that involves more than one field.

```scala mdoc:silent
final case class Window(from: Int, to: Int)

object Window {
  implicit val decoder: JsonDecoder[Window] =
    DeriveJsonDecoder.gen[Window].mapOrFail { r =>
      if (r.from <= r.to) Right(r) else Left("from must not be after to")
    }
}
```

```scala mdoc
"""{"from":5,"to":1}""".fromJson[Window]
```

Because the check runs after the object is built, the error has no field path.

## 7. Use refined types

The `zio-json-interop-refined` module gives you encoders and decoders for [refined](https://github.com/fthomas/refined) types, so a predicate such as `NonEmpty` or `Positive` replaces the hand-written check. Add it to your build:

```sbt
libraryDependencies += "dev.zio" %% "zio-json-interop-refined" % "@VERSION@"
```

```scala mdoc:compile-only
import zio.json._
import zio.json.interop.refined._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

final case class Account(owner: String Refined NonEmpty)

object Account {
  implicit val decoder: JsonDecoder[Account] = DeriveJsonDecoder.gen[Account]
}
```

A failed predicate shows up as a `Left` such as `.owner(Predicate isEmpty() did not fail.)`. See [Refined interop](../interop/refined.md) for the details.

## 8. Reject unknown fields

Validation can also mean refusing JSON that has extra keys. Put `@jsonNoExtraFields` on the case class, and any field not in the class makes decoding fail.

```scala mdoc:silent
@jsonNoExtraFields
final case class Login(user: String)

object Login {
  implicit val decoder: JsonDecoder[Login] = DeriveJsonDecoder.gen[Login]
}
```

```scala mdoc
"""{"user":"ada","admin":true}""".fromJson[Login]
```

Without the annotation, extra fields are skipped. See [Optional and default fields](../how-to/optional-and-default-fields.md) for how missing fields are handled, and the [Annotations reference](../reference/annotations.md) for every annotation.
