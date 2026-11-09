---
id: json-codec
title: "JsonCodec"
description: "Reference for JsonCodec, which pairs a JsonEncoder and a JsonDecoder for one type, with transform, transformOrFail and zip combinators."
keywords:
  - "JSON Codec"
  - "Encoder Decoder Pair"
  - "Codec Transformation"
  - "JsonCodec"
---

`JsonCodec[A]` is a case class that holds a `JsonEncoder[A]` and a `JsonDecoder[A]` for the same type.
Use it when one value should describe both directions: you define the type's JSON shape once and get both halves.
Its instances should round-trip: encoding a value and decoding the result gives the value back.
Unlike `JsonEncoder` and `JsonDecoder`, `JsonCodec` is a `final case class`, not a trait.

| Member                                             | What it does                                             | Details                                         |
|----------------------------------------------------|----------------------------------------------------------|-------------------------------------------------|
| `JsonCodec(encoder, decoder)`                      | Builds a codec from two instances                        | [Construction](#construction)                   |
| `encoder`, `decoder`                               | The two halves                                           | [Construction](#construction)                   |
| `encodeJson(a, indent)`                            | Encodes with the encoder                                 | [Encoding and Decoding](#encoding-and-decoding) |
| `decodeJson(str)`                                  | Decodes with the decoder                                 | [Encoding and Decoding](#encoding-and-decoding) |
| `transform`, `transformOrFail`                     | Maps both directions, the decode side optionally failing | [Transforming a Codec](#transforming-a-codec)   |
| `orElse` (`<>`), `orElseEither` (`<+>`)            | Falls back to another codec when decoding                | [orElse](#orelse-and-orelseeither)              |
| `zip` (`<*>`), `zipLeft` (`<*`), `zipRight` (`*>`) | Pairs two codecs as a JSON array                         | [zip](#zip-zipleft-and-zipright)                |

## Construction

```scala
final case class JsonCodec[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A])
```

The companion object offers three ways to get a codec:

- `JsonCodec(encoder, decoder)` builds one from two existing instances.
- `JsonCodec[A]` summons the implicit instance. The companion has instances for the standard types.
- `DeriveJsonCodec.gen[A]` derives both halves together (Scala 2 and Scala 3). Scala 3 also has `JsonCodec.derived`, used by `derives JsonCodec`.

The companion also declares `implicit def fromEncoderDecoder[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A]`, which is the same as `JsonCodec(encoder, decoder)`.

```scala mdoc:silent
import zio.json._

final case class Person(name: String, age: Int)

object Person {
  implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
}
```

A `JsonCodec[A]` in implicit scope also supplies a `JsonEncoder[A]` and a `JsonDecoder[A]`.
Both companions define `fromCodec` for this, so `toJson` and `fromJson` work with only the codec defined.

```scala mdoc
Person("Ada", 36).toJson

"""{"name":"Ada","age":36}""".fromJson[Person]
```

## Encoding and Decoding

```scala
final def encodeJson(a: A, indent: Option[Int]): CharSequence
final def decodeJson(str: CharSequence): Either[String, A]
```

Both delegate to the encoder and decoder. See [JsonEncoder](json-encoder.md#encodejson) and [JsonDecoder](json-decoder.md#decodejson) for the behavior of `indent` and for the `Left(String)` errors.

:::note[No default for indent]
`JsonEncoder#encodeJson` defaults `indent` to `None`. `JsonCodec#encodeJson` has no default, so you must pass it.
The codec has only the `CharSequence` overload of `decodeJson`. To decode bytes, use `codec.decoder.decodeJson(bytes)` or `fromJson`.
:::

```scala mdoc
Person.codec.encodeJson(Person("Ada", 36), None)

Person.codec.decodeJson("""{"name":"Ada","age":36}""")
```

## Transforming a Codec

```scala
final def transform[B](f: A => B, g: B => A): JsonCodec[B]
final def transformOrFail[B](f: A => Either[String, B], g: B => A): JsonCodec[B]
```

`transform` maps the decoded value with `f` and contramaps the encoder with `g`.
`transformOrFail` is the same, but `f` may fail with an error message, as in `JsonDecoder#mapOrFail`.
`g` cannot fail: the type `B` must always be encodable.
Internally they are `JsonCodec(encoder.contramap(g), decoder.map(f))` and `JsonCodec(encoder.contramap(g), decoder.mapOrFail(f))`.

```scala mdoc:silent
final case class Email(value: String)

val emailCodec: JsonCodec[Email] =
  JsonCodec[String].transformOrFail[Email](
    s => if (s.contains("@")) Right(Email(s)) else Left("not an email address"),
    _.value
  )
```

```scala mdoc
emailCodec.encodeJson(Email("ada@example.com"), None)

emailCodec.decodeJson("\"ada\"")
```

## Combining Codecs

### `orElse` and `orElseEither`

```scala
final def orElse(that: => JsonCodec[A]): JsonCodec[A]
final def orElseEither[B](that: => JsonCodec[B]): JsonCodec[Either[A, B]]
```

`<>` is an alias for `orElse`, and `<+>` is an alias for `orElseEither`.
The decoder tries this codec first and falls back to `that` if it fails.
For `orElseEither`, the encoder writes a `Left` with this codec's encoder and a `Right` with the other, with no tag in the JSON.
`orElse` is `orElseEither` followed by `transform`. It decodes with either codec, but its transform encodes every value as a `Right`, so it always writes with `that`, not with this codec.
The fallback rewinds the input, so it has the same memory caveat as [`JsonDecoder#orElse`](json-decoder.md#orelse-and-orelseeither).

```scala mdoc:silent
val intOrString: JsonCodec[Either[Int, String]] = JsonCodec[Int] <+> JsonCodec[String]
```

```scala mdoc
intOrString.encodeJson(Left(1), None)

intOrString.decodeJson("\"one\"")
```

### `zip`, `zipLeft` and `zipRight`

```scala
final def zip[B](that: => JsonCodec[B]): JsonCodec[(A, B)]
final def zipLeft(that: => JsonCodec[Unit]): JsonCodec[A]
final def zipRight[B](that: => JsonCodec[B])(implicit ev: Unit <:< A): JsonCodec[B]
```

`<*>` is `zip`, `<*` is `zipLeft`, and `*>` is `zipRight`.
`zip` pairs two codecs as a two-element JSON array.
`zipLeft` and `zipRight` drop the `Unit` side, which still takes up a slot in the array.
`zipRight` is only available when `A` is `Unit`, because it encodes `()` for the dropped side.
There is no implicit `JsonCodec[Unit]`, so you build one from `JsonEncoder.unit` and `JsonDecoder.unit`.

```scala mdoc:silent
val pair: JsonCodec[(String, Int)] = JsonCodec[String] <*> JsonCodec[Int]

val unitCodec: JsonCodec[Unit] = JsonCodec(JsonEncoder.unit, JsonDecoder.unit)

val countOnly: JsonCodec[Int] = unitCodec *> JsonCodec[Int]
```

```scala mdoc
pair.encodeJson(("apples", 3), None)

countOnly.encodeJson(3, None)

countOnly.decodeJson("""[{},3]""")
```

## Codec or Separate Instances

Use a `JsonCodec` when the type has one JSON shape in both directions.
It is the least code, and one definition cannot drift out of step with the other.
Use a separate `JsonEncoder` and `JsonDecoder` when the two directions differ, for example when you accept several input shapes but always write one, or when a type only ever goes one way.
You can also build a codec from two different halves with `JsonCodec(encoder, decoder)`.

## Scala 2 vs Scala 3

In Scala 2, derive with `DeriveJsonCodec.gen`, as above.
In Scala 3, `derives JsonCodec` calls `JsonCodec.derived`, which needs a given `JsonCodecConfiguration`.

```scala
// Scala 3 only
import zio.json.*

final case class Person(name: String, age: Int) derives JsonCodec
```

## See Also

- [JsonEncoder](json-encoder.md) and [JsonDecoder](json-decoder.md), the two halves of a codec
- [Encoding](../how-to/encoding.md) and [Decoding](../how-to/decoding.md) for derivation
- [Manual Instances](../how-to/manual-instances.md) for `contramap`, `map` and `mapOrFail`
- [JsonCodecConfiguration](json-codec-configuration.md) and [Annotations](annotations.md) for the derivation options
