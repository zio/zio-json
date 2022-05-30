package zio.json

import scala.annotation.nowarn
import scala.reflect.runtime.universe.TypeTag

import zio.{ test => _, _ }
import zio.json.golden.filehelpers._
import zio.nio.file._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.test._
import zio.test.diff._
import zio.test.diff.Diff._

import zio.json._
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

  @nowarn implicit private lazy val diff: Diff[GoldenSample] = (x: GoldenSample, y: GoldenSample) =>
    Diff[Json].diff(x.samples, y.samples)

  def goldenTest[A: TypeTag: JsonEncoder](
    gen: Gen[Sized, A]
  )(implicit
    trace: Trace,
    config: GoldenConfiguration
  ): Spec[TestEnvironment, Throwable] = {
    val _    = disableAutoTrace // TODO: Find a way to suppress the unused import warning
    val name = getName[A]
    test(s"golden test for $name") {
      import config.{ relativePath, sampleSize }
      val lowerCasedName = name.toLowerCase
      for {
        resourceDir <- createGoldenDirectory(s"src/test/resources/golden/$relativePath")
        fileName     = Path(s"$lowerCasedName.json")
        filePath     = resourceDir / fileName
        assertion <- ZIO.ifZIO(Files.exists(filePath))(
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
    val fileName = Path(s"$name.json")
    val filePath = resourceDir / fileName
    for {
      currentSample <- readSampleFromFile(filePath)
      sample        <- generateSample(gen, sampleSize)
      assertion <- if (sample == currentSample) {
                     ZIO.succeed(assertTrue(sample == currentSample))
                   } else {
                     val diffFileName = Path(s"${name}_changed.json")
                     val diffFilePath = resourceDir / diffFileName
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
    val filePath = resourceDir / Path(fileName)

    val failureString =
      s"No existing golden test for ${resourceDir / Path(s"$name.json")}. Remove _new from the suffix and re-run the test."

    for {
      sample   <- generateSample(gen, sampleSize)
      _        <- ZIO.ifZIO(Files.exists(filePath))(ZIO.unit, Files.createFile(filePath))
      _        <- writeSampleToFile(filePath, sample)
      assertion = TestArrow.make((_: Any) => TestTrace.fail(failureString).withLocation(Some(trace.toString)))
    } yield TestResult(assertion)
  }

  private def generateSample[A: JsonEncoder](
    gen: Gen[Sized, A],
    sampleSize: Int
  )(implicit trace: Trace): ZIO[Sized, Exception, GoldenSample] =
    Gen
      .listOfN(sampleSize)(gen)
      .sample
      .collectSome
      .map(_.value)
      .map { elements =>
        val jsonElements = elements.map(_.toJsonAST).collect { case Right(a) => a }
        val jsonArray    = new Json.Arr(Chunk.fromIterable(jsonElements))
        GoldenSample(jsonArray)
      }
      .runHead
      .someOrFailException

  private def getName[A](implicit typeTag: TypeTag[A]): String =
    typeTag.tpe.typeSymbol.name.decodedName.toString

}
