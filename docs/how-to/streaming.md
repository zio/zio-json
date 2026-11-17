---
id: streaming
title: "Streaming"
description: "Encode and decode JSON with ZIO Streams on the JVM: single values, JSON Lines, JSON arrays, files, and errors in the middle of a stream."
---

ZIO JSON can encode and decode values as [ZIO Streams](https://zio.dev/reference/stream/), so you can process large JSON documents, or many small documents, without loading everything into memory at once. The streaming API has three main forms: a single value as a stream of characters, a JSON array as a pipeline, and [JSON Lines](https://jsonlines.org/) (one value per line) as a pipeline.

The streaming API is available for the JVM only. This covers every method on this page: `encodeJsonStream`, both encoder pipelines, `decodeJsonStream`, `decodeJsonStreamInput`, `decodeJsonPipeline` and the file helpers. The Scala.js and Scala Native builds do not include them. `JsonStreamDelimiter` exists on every platform, but only the JVM `decodeJsonPipeline` uses it.

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

Both methods read the first value in the stream and stop there. They do not check what follows it, so the rest of the stream may be left unread, and trailing text, even a second value, is not an error.

To decode from bytes, such as a file or an HTTP body, pass a byte stream to `decodeJsonStreamInput`:

```scala mdoc
val fromBytes: Event =
  runSync(
    JsonDecoder[Event].decodeJsonStreamInput(
      ZStream.fromChunk(Chunk.fromArray("""{"id": 3, "kind": "archived"}""".getBytes("UTF-8")))
    )
  )

fromBytes
```

## JSON Lines

Use the JSON Lines format, one value per line, when you have a sequence of values that you want to process one at a time. `JsonEncoder#encodeJsonLinesPipeline` writes each value followed by a newline, and `JsonDecoder#decodeJsonPipeline` with `JsonStreamDelimiter.Newline` reads them back:

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

## How the Encoders Emit Output

The encoder pipelines and `encodeJsonStream` behave the same way in three respects:

- The output is always compact. There is no pretty-printed streaming form.
- The JSON Lines output ends with a newline after the last value, as the `lines` example above shows.
- The characters are buffered and emitted in blocks of about 4096 characters (`ZStream.DefaultChunkSize`), not one value at a time. A block is released when the buffer fills or when the input stream ends.

The buffering matters for slow or infinite sources, such as server-sent events or a log tail. A small value can sit in the buffer until about 4 KB more output arrives, so the consumer sees it late. For example, an `Event` such as `{"id":1,"kind":"created"}` takes about 27 characters with its newline, so about 150 of them fill the buffer. A source that produces one every 10 milliseconds delivers its first character only after roughly 1.5 to 2 seconds. When each value must go out as soon as it is produced, encode the values one by one with `toJson` instead, for example `events.map(_.toJson + "\n")`.

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

Stream operations report failures in the error channel as a `Throwable`. A malformed value in the middle of a stream fails the stream at that point. Here the second line is cut short:

```scala mdoc
val withBadLine: String =
  """{"id":1,"kind":"created"}
    |{"id":2,"kind":
    |{"id":3,"kind":"deleted"}
    |""".stripMargin

runSync(
  ZStream
    .fromIterable(withBadLine)
    .via(JsonDecoder[Event].decodeJsonPipeline(JsonStreamDelimiter.Newline))
    .either
    .runCollect
)
```

Values decoded before the bad one are not guaranteed to arrive either: `decodeJsonPipeline` passes values downstream in batches, and the batch that holds the failure is dropped. A failed stream cannot resume, so `ZStream#catchAll` can only replace the rest of the stream (for example with `ZStream.empty`). The lines after the bad one are lost.

To skip malformed lines and keep going, split the text into lines with `ZPipeline.splitLines` and decode each line on its own with `fromJson`. Every line then gives an `Either`, and a bad line does not affect the others:

```scala mdoc
val perLine: Chunk[Either[String, Event]] =
  runSync(ZStream(withBadLine).via(ZPipeline.splitLines).map(_.fromJson[Event]).runCollect)
```

Keep only the `Right` results to drop the bad lines, or log the `Left` results first if you need to know about them:

```scala mdoc
val validEvents: Chunk[Event] =
  runSync(
    ZStream(withBadLine)
      .via(ZPipeline.splitLines)
      .map(_.fromJson[Event])
      .collect { case Right(event) => event }
      .runCollect
  )
```

For a byte source, put `ZPipeline.utf8Decode` before `ZPipeline.splitLines`. Unlike the pipelines on this page, this approach also works on Scala.js and Scala Native, because it uses only `fromJson`.

## See Also

- [JsonEncoder streaming](../reference/json-encoder.md#streaming) and [JsonDecoder streaming](../reference/json-decoder.md#streaming) list the streaming methods on each type class.
- [Errors](../reference/errors.md) describes the error messages that the streaming methods carry.
