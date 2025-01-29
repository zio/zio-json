package zio.json
package internal

import zio.Chunk
import zio.json.ast.Json

private[json] class FieldEncoder[T, P](
  val p: P,
  val name: String,
  val encoder: JsonEncoder[T],
  val withExplicitNulls: Boolean,
  val withExplicitEmptyCollections: Boolean
) {
  private[this] val _encodeOrDefault: T => (
    Either[String, Chunk[(String, Json)]],
    () => Either[String, Chunk[(String, Json)]]
  ) => Either[String, Chunk[(String, Json)]] =
    if (withExplicitNulls && withExplicitEmptyCollections) { _ => (_, encode) =>
      encode()
    } else if (withExplicitNulls) { t => (default, encode) =>
      if (!encoder.isEmpty(t)) encode() else default
    } else if (withExplicitEmptyCollections) { t => (default, encode) =>
      if (!encoder.isNothing(t)) encode() else default
    } else { t => (default, encode) =>
      if (!encoder.isEmpty(t) && !encoder.isNothing(t)) encode() else default
    }
  def encodeOrDefault(t: T)(
    encode: () => Either[String, Chunk[(String, Json)]],
    default: Either[String, Chunk[(String, Json)]]
  ): Either[String, Chunk[(String, Json)]] =
    _encodeOrDefault(t)(default, encode)
}
