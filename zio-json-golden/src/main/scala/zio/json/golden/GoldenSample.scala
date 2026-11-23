package zio.json.golden

import zio.json._
import zio.json.ast.Json

/**
 * The content of a golden file. `goldenTest` stores the encoded samples as a JSON array in `samples`, and the file holds this value as a JSON object with the one field `samples`.
 */
case class GoldenSample(samples: Json)

/** Holds the JSON codec of [[GoldenSample]]. */
object GoldenSample {

  /**
   * Reads and writes a [[GoldenSample]] as a JSON object with the field `samples`. Decoding fails with `.samples(missing)` when that field is absent.
   */
  implicit val jsonCodec: JsonCodec[GoldenSample] = DeriveJsonCodec.gen
}
