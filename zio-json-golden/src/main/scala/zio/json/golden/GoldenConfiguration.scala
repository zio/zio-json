package zio.json.golden

final case class GoldenConfiguration(
  relativePath: String,
  sampleSize: Int
)

object GoldenConfiguration {
  implicit val default: GoldenConfiguration = GoldenConfiguration(
    relativePath = "",
    sampleSize = 20
  )
}
