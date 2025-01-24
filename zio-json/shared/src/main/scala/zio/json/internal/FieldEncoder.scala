package zio.json
package internal

import zio.Chunk
import zio.json.ast.Json

private[json] class FieldEncoder[T, P](
  val p: P,
  val name: String,
  val encoder: JsonEncoder[T],
  withExplicitNulls: Boolean,
  withExplicitEmptyCollections: Boolean
) {
  private[this] val _encodeOrSkip: T => (() => Unit) => Unit =
    (withExplicitNulls, withExplicitEmptyCollections) match {
      case (true, true) => _ => encode => encode()
      case (false, false) => { t => encode =>
        if (!encoder.isEmpty(t) && !encoder.isNothing(t)) encode() else ()
      }
      case (true, false) => { t => encode =>
        if (!encoder.isEmpty(t)) encode() else ()
      }
      case (false, true) => { t => encode =>
        if (!encoder.isNothing(t)) encode() else ()
      }
    }
  def encodeOrSkip(t: T)(encode: () => Unit): Unit = _encodeOrSkip(t)(encode)

  private[this] val _encodeOrDefault: T => (
    Either[String, Chunk[(String, Json)]],
    () => Either[String, Chunk[(String, Json)]]
  ) => Either[String, Chunk[(String, Json)]] =
    (withExplicitNulls, withExplicitEmptyCollections) match {
      case (true, true) => _ => (_, encode) => encode()
      case (false, false) => { t => (default, encode) =>
        if (!encoder.isEmpty(t) && !encoder.isNothing(t)) encode() else default
      }
      case (true, false) => { t => (default, encode) =>
        if (!encoder.isEmpty(t)) encode() else default
      }
      case (false, true) => { t => (default, encode) =>
        if (!encoder.isNothing(t)) encode() else default
      }
    }
  def encodeOrDefault(t: T)(
    encode: () => Either[String, Chunk[(String, Json)]],
    default: Either[String, Chunk[(String, Json)]]
  ): Either[String, Chunk[(String, Json)]] =
    _encodeOrDefault(t)(default, encode)
}
