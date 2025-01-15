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
        assertion <- ZIO.ifZIO(ZIO.attemptBlocking(Files.exists(filePath)))(
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
      assertion <- if (sample == currentSample) {
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
      _ <-
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
