# zio-json

| Project Stage | CI | Release | Snapshot | Discord |
| --- | --- | --- | --- | --- |
| [![Project stage][Stage]][Stage-Page] | ![CI][Badge-GitHub] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots] | [![Badge-Discord]][Link-Discord] |

# Summary
The goal of this project is to create the best all-round JSON library for Scala:

- **Performance** to handle more requests per second than the incumbents, i.e. reduced operational costs.
- **Security** to mitigate against adversarial JSON payloads that threaten the capacity of the server.
- **Fast Compilation** no shapeless, no type astronautics.
- **Future-Proof**, prepared for Scala 3 and next generation Java.
- **Simple** small codebase, short and concise documentation that covers everything.
- **Helpful errors** are readable by humans and machines.
- **ZIO Integration** so nothing more is required.

# How

Extreme **performance** is achieved by decoding JSON directly from the input source into business objects (inspired by [plokhotnyuk](https://github.com/plokhotnyuk/jsoniter-scala)). Although not a requirement, the latest advances in [Java Loom](https://wiki.openjdk.java.net/display/loom/Main) can be used to support arbitrarily large payloads with near-zero overhead.

Best in class **security** is achieved with an aggressive *early exit* strategy that avoids costly stack traces, even when parsing malformed numbers. Malicious (and badly formed) payloads are rejected before finishing reading.

**Fast compilation** and **future-proofing** is possible thanks to [Magnolia](https://propensive.com/opensource/magnolia/) which allows us to generate boilerplate in a way that will survive the exodus to Scala 3. `zio-json` is internally implemented using a [`java.io.Reader`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/Reader.html) / [`java.io.Writer`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/Writer.html)-like interface, which is making a comeback to center stage in Loom.

**Simplicity** is achieved by using well-known software patterns and avoiding bloat. The only requirement to use this library is to know about Scala's encoding of typeclasses, described in [Functional Programming for Mortals](https://leanpub.com/fpmortals/read#leanpub-auto-functionality).

**Helpful errors** are produced in the form of a [`jq`](https://stedolan.github.io/jq/) query, with a note about what went wrong, pointing to the exact part of the payload that failed to parse.

# Documentation

## Installation

```scala
libraryDependencies += "dev.zio" %% "zio-json" % "0.1.0"

scalaVersion in ThisBuild := "2.13.4"
```

All of the following code snippets assume that the following imports have been declared

```scala
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

To do this, we create an *instance* of the `JsonDecoder` typeclass for `Banana` using the `zio-json` code generator. It is best practice to put it on the companion of `Banana`, like so

```scala
object Banana {
  implicit val decoder: JsonDecoder[Banana] = DeriveJsonDecoder.gen[Banana]
}
```

Now we can parse JSON into our object

```
scala> """{"curvature":0.5}""".fromJson[Banana]
val res: Either[String, Banana] = Right(Banana(0.5))
```

Likewise, to produce JSON from our data we define a `JsonEncoder`

```scala
object Banana {
  ...
  implicit val encoder: JsonEncoder[Banana] = DeriveJsonEncoder.gen[Banana]
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
scala> """{"curvature": womp}""".fromJson[Banana]
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
  implicit val decoder: JsonDecoder[Fruit] = DeriveJsonDecoder.gen[Fruit]
  implicit val encoder: JsonEncoder[Fruit] = DeriveJsonEncoder.gen[Fruit]
}
```

allowing us to load the fruit based on a single field type tag in the JSON

```
scala> """{"Banana":{"curvature":0.5}}""".fromJson[Fruit]
val res: Either[String, Fruit] = Right(Banana(0.5))

scala> """{"Apple":{"poison":false}}""".fromJson[Fruit]
val res: Either[String, Fruit] = Right(Apple(false))
```

Almost all of the standard library data types are supported as fields on the case class, and it is easy to add support if one is missing.

## Configuration

By default, the field names of a case class are used as the JSON fields, but it is easy to override this with an annotation `@jsonField`. 
Moreover, you can also mark a whole case class with a member name transformation that will be applied to all members using `@jsonMemberNames`
annotation. It takes an argument of type `JsonMemberFormat` which encodes the transformation that will be applied to member names. 
Four most popular transformations are already provided: KebabCase, SnakeCase, PascalCase and CamelCase. If you require something more
specific you can also use CustomCase which takes a function of shape `String => String` as an argument and can be used to perform
any arbitrary transformation. `@jsonField` annotation takes priority over the transformation defined by `@jsonMemberNames`.

Here's an example json with most fields snake_cased and one kebab-cased:
```json
{
  "passion_fruit": true,
  "granny_smith": true,
  "dragon_fruit": true,
  "blood-orange": false
}
```

And here's the target case class:

```scala
@jsonMemberNames(SnakeCase)
case class FruitBasket(
  passionFruit: Boolean, 
  grannySmith: Boolean, 
  dragonFruit: Boolean, 
  @jsonField("blood-orange") bloodOrange: Boolean
)
```

Notice that all fields are camelCased in Scala and will be both encoded and decoded correctly to snake_case in JSON 
except `bloodOrange` field that is annotated with a `@jsonField` override that will force it to become `"blood-orange"` 
after serialization.

It is also possible to change the type hint that is used to discriminate case classes with `@jsonHint`.

For example, these annotations change the expected JSON of our `Fruit` family

```scala
sealed trait Fruit
@jsonHint("banaani") case class Banana(
  @jsonField("bendiness") curvature: Double
) extends Fruit
@jsonHint("omena") case class Apple(
  @jsonField("bad") poison: Boolean
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

We can raise an error if we encounter unexpected fields by using the `@jsonNoExtraFields` annotation on a case class.

A popular alternative way to encode sealed traits:

```json
{"type":"banaani", "bendiness":0.5}

{"type":"omena", "bad":false}
```

is discouraged for performance reasons. However, if we have no choice in the matter, it may be accomodated with the `@jsonDiscriminator` annotation

```scala
@jsonDiscriminator("type") sealed trait Fruit
```

## Manual Instances

Sometimes it is easier to reuse an existing `JsonDecoder` rather than generate a new one. This can be accomplished using convenience methods on the `JsonDecoder` typeclass to *derive* new decoders:

```scala
trait JsonDecoder[A] {
  def map[B](f: A => B): JsonDecoder[B]
  def mapOrFail[B](f: A => Either[String, B]): JsonDecoder[B]
  ...
}
```

Similarly, we can reuse an existing `JsonEncoder`

```scala
trait JsonEncoder[A] {
  def contramap[B](f: B => A): JsonEncoder[B]
  ...
}
```

### `.map`

We can `.map` from another `JsonDecoder` in cases where the conversion will always succeed. This is very useful if we have a `case class` that simply wraps another thing and shares the same expected JSON.

For example, say we want to model the count of fruit with a `case class` to provide us with additional type safety in our business logic (this pattern is known as a *newtype*).

```scala
case class FruitCount(value: Int)
```

but this would cause us to expect JSON of the form

```json
{"value":1}
```

wheres we really expect the raw number. We can derive a decoder from `JsonDecoder[Int]` and `.map` the result into a `FruitCount`

```scala
object FruitCount {
  implicit val decoder: JsonDecoder[FruitCount] = JsonDecoder[Int].map(FruitCount(_))
}
```

and now the `JsonDecoder` for `FruitCount` just expects a raw `Int`.

Every time we use a `.map` to create a `JsonDecoder` we can usually create a `JsonEncoder` with `.contramap`

```scala
object FruitCount {
  ...
  implicit val encoder: JsonEncoder[FruitCount] = JsonEncoder[Int].contramap(_.value)
}
```

Another usecase is if we want to encode a `case class` as an array of values, rather than an object with named fields. Such an encoding is very efficient because the messages are smaller and require less processing, but are very strict schemas that cannot be upgraded.

```scala
case class Things(s: String, i: Int, b: Boolean)
object Things {
  implicit val decoder: JsonDecoder[Things] =
    JsonDecoder[(String, Int, Boolean)].map { case (p1, p2, p3) => Things(p1, p2, p3) }
}
```

which parses the following JSON

```json
["hello",1,true]
```

### `.mapOrFail`

We can use `.mapOrFail` to take the result of another `JsonDecoder` and try to convert it into our custom data type, failing with a message if there is an error.

Say we are using the [`refined`](https://github.com/fthomas/refined) library to ensure that a `Person` data type only holds a non-empty string in its `name` field

```scala
import zio.json._
import zio.json.interop.refined._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

case class Person(name: String Refined NonEmpty)

object Person {
  implicit val decoder: JsonDecoder[Person] = DeriveJsonDecoder.gen
}
```

we will get a compiletime error because there is no `JsonDecoder[String Refined NonEmpty]`.

However, we can derive one by requesting the `JsonDecoder[String]` and calling `.mapOrFail`, supplying the constructor for our special `String Refined NonEmpty` type

```scala
implicit val decodeName: JsonDecoder[String Refined NonEmpty] =
  JsonDecoder[String].mapOrFail(refined.refineV[NonEmpty](_))
```

Now the code compiles.

In fact, we do not need to provide `decodeName` for each `Refined` data type; `zio-json` comes with support out of the box, see the Integrations section below.

## Integrations

Integrations are provided several popular libraries, which are published as separate artifacts:

- Akka Http
- HTTP4s
- Refined
- Scalaz 7

[Complete list of interop modules](https://zio.github.io/zio-json/docs/interop/interop_index)

# Performance

The following benchmarks are freely available to run on your hardware with `sbt "zioJsonJVM/jmh:run -prof gc"` and can be extended to include more niche libraries. We only compare `zio-json` against Circe and Play as they are the incumbent solutions used by most of the Scala ecosystem.

`zio-json`, when used in legacy mode (i.e. using a `StringReader`), is typically x2 faster than Circe and x5 faster than Play. When used with Loom, `zio-json` has finished its work before the others even begin. The following benchmarks are therefore only for legacy mode comparisons.

There are two main factors to consider when comparing the performance of JSON libraries: memory usage and operations per second. We perform measurements in one thread at a time but in a real server situation, there are multiple threads each consuming resources.

Here are JMH benchmarks (higher `ops/sec` is better, lower `MB/sec` is better) on a standard Google Maps API performance-testing dataset (stressing array and number parsing). Note that a better metric for memory usage might be `MB` per decode or encode, since it can be misleading to have the same `MB/sec` but be processing more JSON: the library that consumes the least amount of memory is likely to have highest throughput.

<!-- zioJsonJVM/jmh:run -prof gc GoogleMaps.*Success1 -->
<!-- zioJsonJVM/jmh:run -prof gc GoogleMaps.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec      MB/sec
zio    15761 ± 283   1633 ± 29     | 14289 ±  84  2214 ± 12
circe   8832 ± 269   1816 ± 55     | 11980 ± 142  2030 ± 24
play    5756 ±  47   2260 ± 19     |  6669 ± 160  2677 ± 64
```

on a standard Twitter API performance-testing dataset (stressing nested case classes with lots of fields)

<!-- zioJsonJVM/jmh:run -prof gc Twitter.*Success1 -->
<!-- zioJsonJVM/jmh:run -prof gc Twitter.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec      MB/sec
zio    16989 ± 113    827 ±  6     | 23085 ± 641  1791 ± 50
circe  16010 ±  72   1349 ±  6     | 15664 ± 209  1627 ± 22
play    5256 ± 165   1231 ± 39     | 15580 ± 314  2260 ± 45
```

on a standard GeoJSON performance-testing dataset (stressing nested sealed traits that use a discriminator)

<!-- zioJsonJVM/jmh:run -prof gc GeoJSON.*Success1 -->
<!-- zioJsonJVM/jmh:run -prof gc GeoJSON.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec       MB/sec
zio    17104 ± 155   2768 ± 25     | 5372 ± 26      817 ±  4
circe   8388 ± 118   2879 ± 41     | 4762 ± 47      592 ±  6
play     704 ±   9   3946 ± 55     | 2587 ± 24     1091 ± 10
```

and on a standard synthetic performance-testing dataset (stressing nested recursive types)

<!-- zioJsonJVM/jmh:run -prof gc Synthetic.*Success -->
<!-- zioJsonJVM/jmh:run -prof gc Synthetic.*encode* -->

```
       Decoding                    | Encoding
       ops/sec       MB/sec        | ops/sec       MB/sec
zio    59099 ± 1307  2108 ± 46     | 32048 ±  240  2573 ± 19
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

<!-- zioJsonJVM/jmh:run -prof gc GoogleMaps.*Attack0 -->

```
       ops/sec        MB/sec
zio    10104 (14823)  1047 (1537)
circe   7456 ( 8832)  1533 (1816)
play    3589 ( 5756)  1344 (2260)
```

### Redundant Data

Most JSON libraries (but not `zio-json`) first create a representation of the JSON message in an Abstract Syntax Tree (AST) that represents all the objects, arrays and values in a generic way. Their decoders typically read what they need from the AST.

An intermediate AST enables attack vectors that insert redundant data, for example in our Google Maps dataset we can add a new field called `redundant` at top-level containing a 60K `String`. If we do this, and run the benchmarks, we see that Circe is heavily impacted, with a 75% reduction in capacity and an increase in memory usage. Play is also impacted, although not as severely. `zio-json`'s ops/sec are reduced but the memory usage is in line which means that throughput is unlikely to be affected by this kind of attack.

<!-- zioJsonJVM/jmh:run -prof gc GoogleMaps.*Attack1 -->

```
       ops/sec       MB/sec
zio    5999 (10104)   622 (1047)
circe  2224 ( 7456)  1655 (1533)
play   2350 ( 3589)  1854 (1344)
```

The reason why `zio-json` is not as badly affected is because it skips values that are unexpected. We can completely mitigate this kind of attack by using the `@jsonNoExtraFields` annotation which results in the payload being rejected at a rate of 5.5 million ops/sec.

Other kinds of redundant values attacks are also possible, such as using an array of 60K full of high precision decimal numbers that require slow parsing (also known as ["near halfway numbers"](https://www.exploringbinary.com/17-digits-gets-you-there-once-youve-found-your-way/)), attacking the CPU. However, the memory attack afforded to us by a redundant `String` is already quite effective.

### `hashCode` Collisions

Following on from the redundant data attack, we can place redundant data in the location of object fields.

JSON libraries that use an intermediate AST often store JSON objects as a stringy `HashMap` (circe uses a [`java.util.LinkedHashMap`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/LinkedHashMap.html)). If we insert redundant fields that have hashcode collisions with legitimate fields, we can successfully attack both memory and CPU. We need to know the hashing algorithm that is being used, which often falls down to some version of the default Java `String.hashCode` [which is very easy to exploit](https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-benchmark/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/benchmark/HashCodeCollider.scala).

In this malicious payload, we add redundant fields that have hashcode collisions, up to 4 collisions per field; we could add more if we used a bruteforce search.

<!-- zioJsonJVM/jmh:run -prof gc GoogleMaps.*Attack2 -->

Again, `zio-json` completely mitigates this attack if the `@jsonNoExtraFields` annotation is used. Note that even if Circe and Play rejected payloads of this nature, it would be too late because the attack happens at the AST layer, not the decoders. However, for the sake of comparison, let's turn off the `zio-json` mitigation:

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

The most brutal attack of this nature is to trick a deserialization library into constructing a gigantic number as a `BigDecimal` and then to downcast to a `BigInteger`. The JVM will happily attempt to reserve GBs of heap for the conversion. Try this in your REPL:

```scala
new java.math.BigDecimal("1e214748364").toBigInteger
```

Fortunately, this kind of attack is prevented by all the mainstream libraries. But it is possible to perform a much weaker form of the attack on Circe, which (to its credit) goes to great effort to pre-validate numbers.

The Google Maps schema has fields of type `Int` and Circe supports the conversion of floating point numbers to integers if the fractional part is zero: so we can pad an integer with as many zeros as possible.

```
       ops/sec       MB/sec
circe  4529 ( 7456)  2037 (1533)
```

This attack is very effective in schemas with lots of numbers, causing ops/sec to be halved with a 33% increase in memory usage.

`zio-json` is resistant to a wide range of number based attacks because it uses a from-scratch number parser that will exit early when the number of bits of any number exceeds 128 bits, which can be customized by the system property `zio.json.number.bits`.

# Even Moar Performance

If `zio-json` isn't fast enough for you, then try out [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala); whereas `zio-json` is fully integrated into ZIO, including streams and transducer support, jsoniter is library agnostic.

JSON is an inefficient transport format and everybody would benefit from a port of this library to msgpack or protobuf. For legacy services, a port supporting XML is also be possible.

# Documentation
[zio-json Microsite](https://zio.github.io/zio-json/)

# Contributing
[Documentation for contributors](https://zio.github.io/zio-json/docs/about/about_contributing)

## Code of Conduct

See the [Code of Conduct](https://zio.github.io/zio-json/docs/about/about_coc)

## Support

Come chat with us on [![Badge-Discord]][Link-Discord].

## License

[License](LICENSE)

## Acknowledgements

- Uses [JsonTestSuite](https://github.com/nst/JSONTestSuite) to test parsing. (c) 2016 Nicolas Seriot)

- Uses [YourKit Java Profiler](https://www.yourkit.com/java/profiler/) for performance optimisation. ![YourKit Logo](https://www.yourkit.com/images/yklogo.png)

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-json_2.12.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-json_2.12.svg "Sonatype Snapshots"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Badge-GitHub]: https://github.com/zio/zio-json/workflows/CI/badge.svg
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-json_2.12/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-json_2.12/ "Sonatype Snapshots"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Stage]: https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
