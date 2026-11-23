package zio.json.golden

/**
 * Settings for `goldenTest`: the folder of the golden file and the number of samples it stores. An implicit
 * `GoldenConfiguration` in scope replaces `GoldenConfiguration.default` for the `goldenTest` calls that see it.
 *
 * @param relativePath
 *   the folder under `src/test/resources/golden` that holds the golden file; the empty string means that folder itself
 * @param sampleSize
 *   the number of values `goldenTest` draws from the generator and stores in the golden file
 */
final case class GoldenConfiguration(
  relativePath: String,
  sampleSize: Int
)

/** Holds the default [[GoldenConfiguration]]. */
object GoldenConfiguration {

  /**
   * The configuration `goldenTest` uses when no other one is in implicit scope: no sub-folder (`relativePath` is the
   * empty string) and 20 samples.
   */
  implicit val default: GoldenConfiguration = GoldenConfiguration(
    relativePath = "",
    sampleSize = 20
  )
}
