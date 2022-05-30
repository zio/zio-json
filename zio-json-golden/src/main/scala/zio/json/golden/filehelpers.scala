package zio.json.golden

import java.io.{ File => JFile }

import zio.{ test => _, _ }
import zio.json.golden.GoldenSample
import zio.nio.file._
import zio.stacktracer.TracingImplicits.disableAutoTrace

object filehelpers {

  def getRootDir(file: JFile)(implicit trace: Trace): Task[JFile] =
    if (file.getName == "target") ZIO.succeed(file)
    else ZIO.attempt(file.getParentFile).flatMap(getRootDir)

  def createGoldenDirectory(pathToDir: String)(implicit trace: Trace): Task[Path] = {
    val _        = disableAutoTrace // TODO: Find a way to suppress the unused import warning
    val rootFile = new JFile(getClass.getResource("/").toURI)
    for {
      baseFile <- getRootDir(rootFile)
      goldenDir = new JFile(baseFile.getParentFile, pathToDir)
      path      = Path.fromJava(goldenDir.toPath)
      _        <- ZIO.attemptBlocking(goldenDir.mkdirs)
    } yield path
  }

  def writeSampleToFile(path: Path, sample: GoldenSample)(implicit trace: Trace): Task[Unit] = {
    val jsonString = sample.toJsonPretty
    Files.writeBytes(path, Chunk.fromArray(jsonString.getBytes("UTF-8")))
  }

  def readSampleFromFile(path: Path)(implicit trace: Trace): Task[GoldenSample] =
    for {
      json   <- zio.json.readJsonAs(path.toFile).runHead.someOrFailException
      sample <- ZIO.fromEither(json.as[GoldenSample].left.map(error => new Exception(error)))
    } yield sample

}
