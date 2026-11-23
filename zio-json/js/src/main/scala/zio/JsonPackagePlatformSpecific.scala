package zio

/**
 * Platform-specific helpers of the `zio.json` package.
 *
 * On the JVM they read files and URLs, and write files, as streams of JSON Lines, that is one JSON value per line.
 * Scala.js and Scala Native have none.
 *
 * On the JVM, for example:
 *
 * {{{
 * import zio.json._
 * import zio.json.ast.Json
 * import zio.stream._
 *
 * val write = writeJsonLines("events.jsonl", ZStream(Json.Obj("id" -> Json.Num(1)), Json.Obj("id" -> Json.Num(2))))
 *
 * val read: ZStream[Any, Throwable, Json] = readJsonAs("events.jsonl")
 * }}}
 */
trait JsonPackagePlatformSpecific {}
