package zio.json.interop.http4s

/**
 * Both http4s entity instances in one import: `import ZIOEntityCodec._` makes an implicit `EntityEncoder[F, A]` and
 * `EntityDecoder[F, A]` available for every `A` that has a `zio.json.JsonEncoder` and a `zio.json.JsonDecoder`.
 *
 * With the import in scope, a `String` entity is also encoded as JSON (a JSON string with quotes), because the
 * imported instance applies to `String` as well.
 *
 * [[ZIOEntityEncoder]] provides the encoder alone and [[ZIOEntityDecoder]] the decoder alone.
 */
object ZIOEntityCodec extends ZIOEntityEncoder with ZIOEntityDecoder
