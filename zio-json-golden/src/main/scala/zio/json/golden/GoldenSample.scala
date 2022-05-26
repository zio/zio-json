package zio.json.golden

import zio.json._
import zio.json.ast.Json

case class GoldenSample(samples: Json)

object GoldenSample {
  implicit val jsonCodec: JsonCodec[GoldenSample] = DeriveJsonCodec.gen
}
