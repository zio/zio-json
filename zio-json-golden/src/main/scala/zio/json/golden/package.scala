package zio.json

import zio.Tag
import zio.{ test => _, _ }
import zio.json.golden.filehelpers._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.test._
import zio.test.diff._
import zio.test.diff.Diff._
import java.nio.file.{ Paths, Path, Files }

import zio.json.ast._

package object golden {

  implicit private lazy val diffJsonValue: Diff[Json] = {
    case (x: Json.Obj, y: Json.Obj) =>
      mapDiff[String, Json].diff(x.fields.toMap, y.fields.toMap)

    case (x: Json.Arr, y: Json.Arr) =>
      seqDiff[Json].diff(x.elements, y.elements)

    case (x, y) =>
      if (x == y) DiffResult.Identical(x)
      else DiffResult.Different(x, y)
  }

  implicit private lazy val diff: Diff[GoldenSample] = (x: GoldenSample, y: GoldenSample) =>
    Diff[Json].diff(x.samples, y.samples)

  /**
   * Creates a test that compares the JSON encoding of values drawn from `gen` with a golden file, so that a change to
   * the encoder of `A` fails the test. The test is named `golden test for` followed by the short name of `A`.
   *
   * The test draws `sampleSize` values from `gen`, encodes each one with the implicit `JsonEncoder`, and compares the
   * resulting JSON array with the golden file. The file is named after the short name of `A` (`Point.json` for `Point`,
   * `List.json` for `List[Int]`) and lives in the folder `src/test/resources/golden` plus `relativePath` under the
   * module root (see `filehelpers.moduleRootProperty`). An implicit [[GoldenConfiguration]] sets `relativePath` and
   * `sampleSize`; without one, `GoldenConfiguration.default` applies.
   *
   *   - When the golden file doesn't exist, the test writes the samples to `Point_new.json` and fails with
   *     `No existing golden test for`, the path of `Point.json`, and
   *     `. Remove _new from the suffix and re-run the test.`
   *   - When the samples equal the stored ones, the test passes. Two runs of the same test draw the same samples.
   *   - When they differ, the test writes the new samples to `Point_changed.json` and fails with a diff between the
   *     stored and the new samples.
   *
   * {{{
   * import zio.json.golden._
   * import zio.test._
   *
   * final case class Point(x: Int, y: Int)
   * object Point {
   *   implicit val codec: JsonCodec[Point] = DeriveJsonCodec.gen
   * }
   *
   * object PointGoldenSpec extends ZIOSpecDefault {
   *   val genPoint: Gen[Any, Point] =
   *     for {
   *       x <- Gen.int(0, 9)
   *       y <- Gen.int(0, 9)
   *     } yield Point(x, y)
   *
   *   def spec = suite("PointGoldenSpec")(goldenTest(genPoint))
   * }
   * }}}
   *
   * @param gen
   *   the generator of the values to encode
   * @see
   *   [[https://zio.dev/zio-json/golden-testing/ Golden Testing]]
   */
  def goldenTest[A: Tag: JsonEncoder](
    gen: Gen[Sized, A]
  )(implicit
    trace: Trace,
    config: GoldenConfiguration
  ): Spec[TestEnvironment, Throwable] = {
    val _    = disableAutoTrace // TODO: Find a way to suppress the unused import warning
    val name = getName[A]
    test(s"golden test for $name") {
      import config.{ relativePath, sampleSize }
      for {
        resourceDir <- createGoldenDirectory(s"src/test/resources/golden/$relativePath")
        fileName     = Paths.get(s"$name.json")
        filePath     = resourceDir.resolve(fileName)
        assertion   <- ZIO.ifZIO(ZIO.attemptBlocking(Files.exists(filePath)))(
                       validateTest(resourceDir, name, gen, sampleSize),
                       createNewTest(resourceDir, name, gen, sampleSize)
                     )
      } yield assertion
    }
  }

  private def validateTest[A: JsonEncoder](
    resourceDir: Path,
    name: String,
    gen: Gen[Sized, A],
    sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Throwable, TestResult] = {
    val fileName = Paths.get(s"$name.json")
    val filePath = resourceDir.resolve(fileName)

    for {
      currentSample <- readSampleFromFile(filePath)
      sample        <- generateSample(gen, sampleSize)
      assertion     <- if (sample == currentSample) {
                     ZIO.succeed(assertTrue(sample == currentSample))
                   } else {
                     val diffFileName = Paths.get(s"${name}_changed.json")
                     val diffFilePath = resourceDir.resolve(diffFileName)
                     writeSampleToFile(diffFilePath, sample) *>
                       ZIO.succeed(assertTrue(sample == currentSample))
                   }
    } yield assertion
  }

  private def createNewTest[A: JsonEncoder](
    resourceDir: Path,
    name: String,
    gen: Gen[Sized, A],
    sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Throwable, TestResult] = {
    val fileName = s"${name}_new.json"
    val filePath = resourceDir.resolve(fileName)

    val failureString =
      s"No existing golden test for ${resourceDir.resolve(s"$name.json")}. Remove _new from the suffix and re-run the test."

    for {
      sample <- generateSample(gen, sampleSize)
      _      <-
        ZIO
          .ifZIO(ZIO.attemptBlocking(Files.exists(filePath)))(ZIO.unit, ZIO.attemptBlocking(Files.createFile(filePath)))
      _        <- writeSampleToFile(filePath, sample)
      assertion = TestArrow.make((_: Any) => TestTrace.fail(failureString).withLocation(Some(trace.toString)))
    } yield TestResult(assertion)
  }

  /**
   * Implementation inspired by zio-test [[zio.test#check]]
   */
  private def generateSample[A: JsonEncoder](
    gen: Gen[Sized, A],
    sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Exception, GoldenSample] =
    gen.sample.forever
      .map(_.value)
      .map(_.toJsonAST)
      .collectRight
      .take(sampleSize.toLong)
      .runCollect
      .map(jsonElements => GoldenSample(new Json.Arr(jsonElements)))

  private def getName[A](implicit tag: Tag[A]): String =
    tag.tag.shortName
}
