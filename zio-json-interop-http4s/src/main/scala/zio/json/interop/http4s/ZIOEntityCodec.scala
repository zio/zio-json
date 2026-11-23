package zio.json.interop.http4s

/**
 * Both http4s entity instances in one import: `import ZIOEntityCodec._` makes an implicit `EntityEncoder[F, A]` and
 * `EntityDecoder[F, A]` available for every `A` that has a `zio.json.JsonEncoder` and a `zio.json.JsonDecoder`.
 *
 * With the import in scope, `String` goes through JSON too, because the imported instances apply to `String` as well: a
 * `String` entity is written as a JSON string with quotes, and `as[String]` reads the body as a JSON string (a plain-text
 * body fails with a `MalformedMessageBodyFailure`).
 *
 * [[ZIOEntityEncoder]] provides the encoder alone and [[ZIOEntityDecoder]] the decoder alone.
 */
object ZIOEntityCodec extends ZIOEntityEncoder with ZIOEntityDecoder
