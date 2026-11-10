---
id: streaming
title: "Streaming"
---

ZIO JSON can encode and decode values as [ZIO Streams](https://zio.dev/reference/stream/), so you can process large JSON documents, or many small documents, without loading everything into memory at once.
The streaming API has three main forms: a single value as a stream of characters, a JSON array as a pipeline, and [JSON Lines](https://jsonlines.org/) (one value per line) as a pipeline.

The streaming API is available for the JVM only. The JavaScript and Native platforms do not include these methods.

## Encoding a Single Value as a Stream

`JsonEncoder#encodeJsonStream` returns a `ZStream` of characters for one value:

```scala mdoc:silent
import zio._
import zio.stream._
import zio.json._

case class Event(id: Int, kind: String)

object Event {
  implicit val codec: JsonCodec[Event] = DeriveJsonCodec.gen[Event]
}

def runSync[A](zio: ZIO[Any, Throwable, A]): A =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(zio).getOrThrowFiberFailure()
  }
```

To collect the characters into a `String`, run the stream:

```scala mdoc
val single: String =
  runSync(JsonEncoder[Event].encodeJsonStream(Event(1, "created")).runCollect.map(_.mkString))

single
```

## Decoding a Single Value from a Stream

`JsonDecoder#decodeJsonStream` reads one value from a stream of characters, and `JsonDecoder#decodeJsonStreamInput` reads one value from a stream of bytes with a charset (UTF-8 by default):

```scala mdoc
val decodedSingle: Event =
  runSync(JsonDecoder[Event].decodeJsonStream(ZStream.fromIterable("""{"id": 2, "kind": "deleted"}""")))

decodedSingle
```

Both methods expect exactly one value in the stream, and they may not consume the whole stream.

## JSON Lines

Use the JSON Lines format, one value per line, when you have a sequence of values that you want to process one at a time.
`JsonEncoder#encodeJsonLinesPipeline` writes each value followed by a newline, and `JsonDecoder#decodeJsonPipeline` with `JsonStreamDelimiter.Newline` reads them back:

```scala mdoc:silent
val events: ZStream[Any, Nothing, Event] =
  ZStream(Event(1, "created"), Event(2, "updated"), Event(3, "deleted"))
```

The stream is encoded with the line-delimited pipeline:

```scala mdoc

val lines: String =
  runSync(events.via(JsonEncoder[Event].encodeJsonLinesPipeline).runCollect.map(_.mkString))

lines
```

Decoding the same text back into values uses the decoder pipeline:

```scala mdoc
val parsed: Chunk[Event] =
  runSync(
    ZStream
      .fromIterable(lines)
      .via(JsonDecoder[Event].decodeJsonPipeline(JsonStreamDelimiter.Newline))
      .runCollect
  )

parsed.toList
```

## JSON Arrays

`JsonEncoder#encodeJsonArrayPipeline` writes the values as a single JSON array, and `JsonDecoder#decodeJsonPipeline` with its default `JsonStreamDelimiter.Array` reads a JSON array one element at a time:

```scala mdoc
val array: String =
  runSync(events.via(JsonEncoder[Event].encodeJsonArrayPipeline).runCollect.map(_.mkString))

array
```

Decoding that string with the default delimiter reads each element back:

```scala mdoc
val fromArray: Chunk[Event] =
  runSync(
    ZStream
      .fromIterable(array)
      .via(JsonDecoder[Event].decodeJsonPipeline())
      .runCollect
  )

fromArray.toList
```

Choose JSON Lines for logs and exported data that are written and read incrementally, because each line is a complete value. Choose the array form when the consumer expects a single JSON document.

## Reading and Writing Files

The `zio.json` package object provides helpers that combine these pipelines with file streams. They are available on the JVM only.

`readJsonLinesAs` reads a file of JSON Lines into a stream of values of a type with a decoder, and `writeJsonLinesAs` writes a stream of values to a file as JSON Lines. `readJsonLinesAs` has overloads for `File`, `Path`, `String` and `URL`, and `writeJsonLinesAs` has overloads for `File`, `Path` and `String`:

```scala mdoc:compile-only
import zio._
import zio.stream._
import zio.json._
import java.nio.file.Paths

val path = Paths.get("events.jsonl")

val write: RIO[Any, Unit] =
  writeJsonLinesAs(path, ZStream(Event(1, "created"), Event(2, "deleted")))

val read: ZStream[Any, Throwable, Event] =
  readJsonLinesAs[Event](path)
```

`readJsonAs` and `writeJsonLines` work the same way with the JSON AST type `zio.json.ast.Json` instead of a domain type. `readJsonAs` reads JSON Lines, not a single JSON document, so use `fromJson` on the file contents when you want to read one document.

## Errors

Stream operations report failures in the error channel as a `Throwable`. A malformed value in the middle of a stream fails the stream at that point, and the values before it have already been emitted. If you want to skip malformed lines instead, handle the error in the stream, for example with `ZStream#catchAll`.
