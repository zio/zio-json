package zio.json.golden

import java.io.File
import java.nio.file.{ Path, Paths }

import zio.{ test => _, _ }

import zio.json._
import zio.json.golden.GoldenSample
import zio.stacktracer.TracingImplicits.disableAutoTrace
import java.nio.file.Files

object filehelpers {

  def getRootDir(file: File)(implicit trace: Trace): Task[File] =
    if (file.getName == "target") ZIO.succeed(file)
    else ZIO.attempt(file.getParentFile).flatMap(getRootDir)

  def createGoldenDirectory(pathToDir: String)(implicit trace: Trace): Task[Path] = {
    val _        = disableAutoTrace // TODO: Find a way to suppress the unused import warning
    val rootFile = new File(getClass.getResource("/").toURI)

    for {
      baseFile <- getRootDir(rootFile)
      goldenDir = new File(baseFile.getParentFile, pathToDir)
      path      = goldenDir.toPath
      _        <- ZIO.attemptBlocking(goldenDir.mkdirs)
    } yield path
  }

  def writeSampleToFile(path: Path, sample: GoldenSample)(implicit trace: Trace): Task[Unit] = {
    val jsonString = sample.toJsonPretty

    ZIO.attemptBlocking {
      Files.write(path, jsonString.getBytes("UTF-8"))
    }
  }

  def readSampleFromFile(path: Path)(implicit trace: Trace): Task[GoldenSample] =
    for {
      json   <- zio.json.readJsonAs(path.toFile).runHead.someOrFailException
      sample <- ZIO.fromEither(json.as[GoldenSample].left.map(error => new Exception(error)))
    } yield sample

}
