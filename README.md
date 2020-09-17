The goal of this project is to create the best all-round JSON library for Scala:

- **Performance** to handle more requests per second than the incumbents, i.e. reduced operational costs.
- **Security** to mitigate against adversarial JSON payloads that threaten the capacity of the server.
- **Fast Compilation** no shapeless, no type astronautics.
- **Future Proof** prepared for Scala 3 and next generation Java.
- **Simple** small codebase, short and concise documentation that covers everything.
- **Helpful errors** are readable by humans and machines.

# How

Extreme **performance** is achieved by decoding JSON directly from the input source into business objects (inspired by [plokhotnyuk](https://github.com/plokhotnyuk/jsoniter-scala)). Although not a requirement, the latest advances in [Java Loom](https://wiki.openjdk.java.net/display/loom/Main) can be used to support arbitrarily large payloads with near-zero overhead.

Best in class **security** is achieved with an aggressive *early exit* strategy that avoids costly stacktraces, even when parsing malformed numbers. Malicious (and badly formed) payloads are rejected before finishing reading.

**Fast compilation** and **future proofing** is possible thanks to [Magnolia](https://propensive.com/opensource/magnolia/) which allows us to generate boilerplate in a way that will survive the exodus to Scala 3. `zio-json` is internally implemented using the [`java.io.Reader`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/Reader.html) interface which is making a comeback to center stage in Loom.

**Simplicity** is achieved by using well-known software patterns and avoiding bloat. The only requirement to use this library is to know about Scala's encoding of typeclasses, described in [Functional Programming for Mortals](https://leanpub.com/fpmortals/read#leanpub-auto-functionality).

**Helpful errors** are produced in the form of a [`jq`](https://stedolan.github.io/jq/) query, with a note about what went wrong, pointing to the exact part of the payload that failed to parse.

# Documentation

## Installation

`zio-json` is in beta and available as a source dependency, e.g. use a git subproject in `build.sbt`

```
lazy val zio_json = ProjectRef(uri("https://github.com/zio/zio-json.git#develop"), "zio-json")

lazy val root = (project in file(".")).dependsOn(zio_json)

scalaVersion in ThisBuild := "2.13.3"
```

All of the following code snippets assume that the following imports have been declared

```scala
import zio.json
import zio.json._
```

## Simple Example

Say we want to be able to read some JSON like

```json
{"curvature":0.5}
```

into a Scala `case class`

```scala
case class Banana(curvature: Double)
```

To do this, we create an *instance* of the `json.JsonDecoder` typeclass for `Banana` using the `zio-json` code generator. It is best practice to put it on the companion of `Banana`, like so

```scala
object Banana {
  implicit val decoder: json.JsonDecoder[Banana] = json.DeriveJsonDecoder.gen[Banana]
}
```

Now we can parse JSON into our object

```
scala> json.parser.decode[Banana]("""{"curvature":0.5}""")
val res: Either[String, Banana] = Right(Banana(0.5))
```

Likewise, to produce JSON from our data we define a `json.JsonEncoder`

```scala
object Banana {
  ...
  implicit val encoder: json.JsonEncoder[Banana] = json.DeriveEncoder.gen[Banana]
}

scala> Banana(0.5).toJson
val res: String = {"curvature":0.5}

scala> Banana(0.5).toJsonPretty
val res: String =
{
  "curvature" : 0.5
}
```

And bad JSON will produce an error in `jq` syntax with an additional piece of contextual information (in parentheses)

```
scala> json.parser.decode[Banana]("""{"curvature": womp}""")
val res: Either[String, Banana] = Left(.curvature(expected a number, got w))
```

Say we extend our data model to include more data types

```scala
sealed trait Fruit
case class Banana(curvature: Double) extends Fruit
case class Apple (poison: Boolean)   extends Fruit
```

we can generate the encoder and decoder for the entire `sealed` family

```scala
object Fruit {
  implicit val decoder: json.JsonDecoder[Fruit] = json.DeriveJsonDecoder.gen[Fruit]
  implicit val encoder: json.JsonEncoder[Fruit] = json.DeriveEncoder.gen[Fruit]
}
```

allowing us to load the fruit based on a single field type tag in the JSON

```
scala> json.parser.decode[Fruit]("""{"Banana":{"curvature":0.5}}""")
val res: Either[String, Fruit] = Right(Banana(0.5))

scala> json.parser.decode[Fruit]("""{"Apple":{"poison":false}}""")
val res: Either[String, Fruit] = Right(Apple(false))
```

Almost all of the standard library data types are supported as fields on the case class, and it is easy to add support if one is missing.

## Configuration

By default the field names of a case class are used as the JSON fields, but it is easy to override this with an annotation `@json.field`.

It is also possible to change the type hint that is used to discriminate case classes with `@json.hint`.

For example, these annotations change the expected JSON of our `Fruit` family

```scala
sealed trait Fruit
@json.hint("banaani") case class Banana(
  @json.field("bendiness") curvature: Double
) extends Fruit
@json.hint("omena") case class Apple(
  @json.field("bad") poison: Boolean
) extends Fruit
```

from

```json
{"Banana":{"curvature":0.5}}

{"Apple":{"poison":false}}
```

to

```json
{"banaani":{"bendiness":0.5}}

{"omena":{"bad":false}}
```

We can raise an error if we encounter unexpected fields by using the `@json.no_extra_fields` annotation on a case class.

A popular alternative way to encode sealed traits:

```json
{"type":"banaani", "bendiness":0.5}

{"type":"omena", "bad":false}
```

is discouraged for performance reasons. However, if we have no choice in the matter, it may be accomodated with the `@json.discriminator` annotation

```scala
@json.discriminator("type") sealed trait Fruit
```

## Manual Instances

Sometimes it is easier to reuse an existing `json.JsonDecoder` rather than generate a new one. This can be accomplished using convenience methods on the `json.JsonDecoder` typeclass to *derive* new decoders:

```scala
trait JsonDecoder[+A] {
  def map[B](f: A => B): JsonDecoder[B]
  def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B]
  ...
}
```

Similarly, we can reuse an existing `json.JsonEncoder`

```scala
trait JsonEncoder[-A] {
  def contramap[B](f: B => A): JsonEncoder[B]
  ...
}
```

### `.map`

We can `.map` from another `json.JsonDecoder` in cases where the conversion will always succeed. This is very useful if we have a `case class` that simply wraps another thing and shares the same expected JSON.

For example, say we want to model the count of fruit with a `case class` to provide us with additional type safety in our business logic (this pattern is known as a *newtype*).

```scala
case class FruitCount(value: Int)
```

but this would cause us to expect JSON of the form

```json
{"value":1}
```

wheres we really expect the raw number. We can derive a decoder from `json.JsonDecoder[Int]` and `.map` the result into a `FruitCount`

```scala
object FruitCount {
  implicit val decoder: json.JsonDecoder[FruitCount] = json.JsonDecoder[Int].map(FruitCount(_))
}
```

and now the `json.JsonDecoder` for `FruitCount` just expects a raw `Int`.

Every time we use a `.map` to create a `json.JsonDecoder` we can usually create a `json.JsonEncoder` with `.contramap`

```scala
object FruitCount {
  ...
  implicit val encoder: json.JsonEncoder[FruitCount] = json.JsonEncoder[Int].contramap(_.value)
}
```

Another usecase is if we want to encode a `case class` as an array of values, rather than an object with named fields. Such an encoding is very efficient because the messages are smaller and require less processing, but are very strict schemas that cannot be upgraded.

```scala
case class Things(s: String, i: Int, b: Boolean)
object Things {
  implicit val decoder: json.JsonDecoder[Things] =
    json.JsonDecoder[(String, Int, Boolean)].map { case (p1, p2, p3) => Things(p1, p2, p3) }
}
```

which parses the following JSON

```json
["hello",1,true]
```

### `.mapOrFail`

We can use `.mapOrFail` to take the result of another `json.JsonDecoder` and try to convert it into our custom data type, failing with a message if there is an error.

Say we are using the [`refined`](https://github.com/fthomas/refined) library to ensure that a `Person` data type only holds a non-empty string in its `name` field

```scala
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

case class Person(name: String Refined NonEmpty)

object Person {
  implicit val decoder: json.JsonDecoder[Person] = json.DeriveJsonDecoder.gen
}
```

we will get a compiletime error because there is no `json.JsonDecoder[String Refined NonEmpty]`.

However, we can derive one by requesting the `json.JsonDecoder[String]` and calling `.mapOrFail`, supplying the constructor for our special `String Refined NonEmpty` type

```scala
implicit val decodeName: json.JsonDecoder[String Refined NonEmpty] =
  json.JsonDecoder[String].mapOrFail(refined.refineV[NonEmpty](_))
```

Now the code compiles.

In fact, we do not need to provide `decodeName` for each `Refined` data type; `zio-json` comes with support out of the box, see the Integrations section below.

## Integrations

Integrations are provided for the following external libraries which must be depended upon separately:

- [scalaz 7.3](https://github.com/scalaz/scalaz) with `import zio.json.compat.scalaz._`
- [refined 0.9.15](https://github.com/fthomas/refined) with `import zio.json.compat.refined._`

Alternative (binary incompatible) versions are not supported; if you require support for a different version of any of these libraries, just copy the source code into your project and change the package name (and address any API changes).

# Performance

The following benchmarks are freely available to run on your hardware with `sbt "jmh:run -prof gc"` and can be extended to include more niche libraries. We only compare `zio-json` against Circe and Play as they are the incumbent solutions used by most of the Scala ecosystem.

`zio-json`, when used in legacy mode (i.e. using a `StringReader`), is typically x2 faster than Circe and x5 faster than Play. When used with Loom, `zio-json` has finished its work before the others even begin. The following benchmarks are therefore only for legacy mode comparisons.

There are two main factors to consider when comparing the performance of JSON libraries: memory usage and operations per second. We perform measurements in one thread at a time but in a real server situation, there are multiple threads each consuming resources.

Here are JMH benchmarks (higher `ops/sec` is better, lower `MB/sec` is better) on a standard Google Maps API performance-testing dataset (stressing array and number parsing). Note that a better metric for memory usage might be `MB` per decode or encode, since it can be misleading to have the same `MB/sec` but be processing more JSON: the library that consumes the least amount of memory is likely to have highest throughput.

<!-- jmh:run -prof gc GoogleMaps.*Success1 -->
<!-- jmh:run -prof gc GoogleMaps.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec      MB/sec
zio    15761 ± 283   1633 ± 29     | 16125 ± 192   629 ±  7
circe   8832 ± 269   1816 ± 55     | 11980 ± 142  2030 ± 24
play    5756 ±  47   2260 ± 19     |  6669 ± 160  2677 ± 64
```

on a standard Twitter API performance-testing dataset (stressing nested case classes with lots of fields)

<!-- jmh:run -prof gc Twitter.*Success1 -->
<!-- jmh:run -prof gc Twitter.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec      MB/sec
zio    16989 ± 113    827 ±  6     | 16945 ± 247  1100 ± 16
circe  16010 ±  72   1349 ±  6     | 15664 ± 209  1627 ± 22
play    5256 ± 165   1231 ± 39     | 15580 ± 314  2260 ± 45
```

on a standard GeoJSON performance-testing dataset (stressing nested sealed traits that use a discriminator)

<!-- jmh:run -prof gc GeoJSON.*Success1 -->
<!-- jmh:run -prof gc GeoJSON.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec       MB/sec
zio    17104 ± 155   2768 ± 25     | 6031 ± 63      522 ±  6
circe   8388 ± 118   2879 ± 41     | 4762 ± 47      592 ±  6
play     704 ±   9   3946 ± 55     | 2587 ± 24     1091 ± 10
```

and on a standard synthetic performance-testing dataset (stressing nested recursive types)

<!-- jmh:run -prof gc Synthetic.*Success -->
<!-- jmh:run -prof gc Synthetic.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec       MB/sec
zio    59099 ± 1307  2108 ± 46     | 57547 ±  235   787 ±  3
circe  19609 ±  370  2873 ± 53     | 13830 ±  109  1730 ± 14
play    9001 ±  182  3348 ± 67     | 14529 ±  200  3533 ± 48
```

`zio-json` easily wins every benchmark except ops/sec for the Twitter test data where Circe matches ops/sec but loses heavily on memory usage. Play loses on every benchmark.

# Security

A [Denial of Service](https://en.wikipedia.org/wiki/Denial-of-service_attack) (DOS) attack is a cyber-attack in which the perpetrator seeks to make a machine or network resource unavailable to its intended users by temporarily or indefinitely disrupting services. The vast majority of public-facing servers written in Scala are vulnerable to DOS attack.

Attacks that are in the form of a valid payload are designed to be stealthy and will produce the same end-result as a legitimate payload, but will consume more resources along the way. In this section we investigate specific attacks and how `zio-json` mitigates against them.

### Resource Attack: Larger Payload

An obvious way to slow down a server is to give it more data to read. JSON is particularly susceptible to this kind of attack because it has an in-built mechanism to expand the size of the message without altering the contents: pad with whitespace.

Many web frameworks will fully consume the contents of a payload into a `String` before handing it off to the JSON library, so if we receive a JSON message consisting of 1GB of whitespace, we will consume 1GB of heap on that server.

The best way to mitigate against message size attacks is to cap the `Content-Length` to a reasonable size for the use case. A further mitigation is to use a streaming parser that does not require the entire message to be read into memory before parsing begins.

For all the remaining attacks, we will cap the malicious message size to 100KB (the original message is 25KB) and compare the attacks against this baseline. The benchmark results for the original (unedited) payload are given in parentheses, and we can immediately see a reduction in the ops/sec for all frameworks, accompanied by a reduction in memory usage.

<!-- jmh:run -prof gc GoogleMaps.*Attack0 -->

```
       ops/sec        MB/sec
zio    10104 (14823)  1047 (1537)
circe   7456 ( 8832)  1533 (1816)
play    3589 ( 5756)  1344 (2260)
```

### Redundant Data

Most JSON libraries (but not `zio-json`) first create a representation of the JSON message in an Abstract Syntax Tree (AST) that represents all the objects, arrays and values in a generic way. Their decoders typically read what they need from the AST.

An intermediate AST enables attack vectors that insert redundant data, for example in our Google Maps dataset we can add a new field called `redundant` at top-level containing a 60K `String`. If we do this, and run the benchmarks, we see that Circe is heavily impacted, with a 75% reduction in capacity and an increase in memory usage. Play is also impacted, although not as severely. `zio-json`'s ops/sec are reduced but the memory usage is in line which means that throughput is unlikely to be affected by this kind of attack.

<!-- jmh:run -prof gc GoogleMaps.*Attack1 -->

```
       ops/sec       MB/sec
zio    5999 (10104)   622 (1047)
circe  2224 ( 7456)  1655 (1533)
play   2350 ( 3589)  1854 (1344)
```

The reason why `zio-json` is not as badly affected is because it skips values that are unexpected. We can completely mitigate this kind of attack by using the `@json.no_extra_fields` annotation which results in the payload being rejected at a rate of 5.5 million ops/sec.

Other kinds of redundant values attacks are also possible, such as using an array of 60K full of high precision decimal numbers that require slow parsing (also known as ["near halfway numbers"](https://www.exploringbinary.com/17-digits-gets-you-there-once-youve-found-your-way/)), attacking the CPU. However, the memory attack afforded to us by a redundant `String` is already quite effective.

### `hashCode` Collisions

Following on from the redundant data attack, we can place redundant data in the location of object fields.

JSON libraries that use an intermediate AST often store JSON objects as a stringy `HashMap` (circe uses a [`java.util.LinkedHashMap`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/LinkedHashMap.html)). If we insert redundant fields that have hashcode collisions with legitimate fields, we can successfully attack both memory and CPU. We need to know the hashing algorithm that is being used, which often falls down to some version of the default Java `String.hashCode` [which is very easy to exploit](https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-benchmark/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/benchmark/HashCodeCollider.scala).

In this malicious payload, we add redundant fields that have hashcode collisions, up to 4 collisions per field; we could add more if we used a bruteforce search.

<!-- jmh:run -prof gc GoogleMaps.*Attack2 -->

Again, `zio-json` completely mitigates this attack if the `@json.no_extra_fields` annotation is used. Note that even if Circe and Play rejected payloads of this nature, it would be too late because the attack happens at the AST layer, not the decoders. However, for the sake of comparison, let's turn off the `zio-json` mitigation:

```
       ops/sec       MB/sec
zio    3742 (10104)   695 (1047)
circe  1992 ( 7456)  1162 (1533)
play   1312 ( 3589)  1636 (1344)
```

ops/sec is down for all decoders relative to the baseline, but since `zio-json` and Circe memory usage is also reduced the throughput on a server might not be impacted as badly as it sounds.

However, this attack hurts Play very badly; memory usage is up compared to the baseline with throughput reduced to 40% of the baseline (22% of the original).

There is a variant of this attack that can be devastating for libraries that rely on `HashMap`. In this attack, [developed by plokhotnyuk to DOS ujson](https://github.com/plokhotnyuk/jsoniter-scala/pull/325), an object is filled with many fields that have a `hashCode` of zero. This exploits two facts:

- Java `String` does not cache `hashCode` of zero, recomputing every time it is requested
- many `HashMap` implementations re-request the hashes of all objects as the number of entries increases during construction.

### Death by a Thousand Zeros

Another kind of attack is to provide data that will cause the decoder for a specific value to do more work than it needs to. Numbers are always a great example of this.

The most brutal attack of this nature is to trick a deserialisation library into constructing a gigantic number as a `BigDecimal` and then to downcast to a `BigInteger`. The JVM will happily attempt to reserve GBs of heap for the conversion. Try this in your REPL:

```scala
new java.math.BigDecimal("1e214748364").toBigInteger
```

Fortunately, this kind of attack is prevented by all the mainsteam libraries. But it is possible to perform a much weaker form of the attack on Circe, which (to its credit) goes to great effort to pre-validate numbers.

The Google Maps schema has fields of type `Int` and Circe supports the conversion of floating point numbers to integers if the fractional part is zero: so we can pad an integer with as many zeros as possible.

```
       ops/sec       MB/sec
circe  4529 ( 7456)  2037 (1533)
```

This attack is very effective in schemas with lots of numbers, causing ops/sec to be halved with a 33% increase in memory usage.

`zio-json` is resistant to a wide range of number based attacks because it uses a from-scratch number parser that will exit early when the number of bits of any number exceeds 128 bits, which can be customised by the system property `zio.json.number.bits`.

# Even Moar Performance

If `zio-json` isn't fast enough for you, then try out [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala); whereas `zio-json` has picked a blend of goals, jsoniter has gone for raw performance. Caveat emptor.

JSON is an inefficent transport format and everybody would benefit from a port of this library to msgpack or protobuf. For legacy services, a port supporting XML is also be possible.
