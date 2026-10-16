package zio.json.golden

import java.io.{ File, IOException }
import java.nio.file.Path
import zio.{ test => _, _ }
import zio.json._

import java.nio.file.Files

object filehelpers {

  /**
   * System property naming the module root (the directory holding `src/test/resources/golden`). Needed when test
   * classes are not under `<module>/target`, e.g. sbt 2's `target/out/`. When unset, the root is found from `target`.
   */
  val moduleRootProperty: String = "zio.json.golden.moduleRoot"

  def getRootDir(file: File)(implicit trace: Trace): Task[File] =
    if (file.getName == "target") ZIO.succeed(file)
    else ZIO.attempt(file.getParentFile).flatMap(getRootDir)

  private[golden] def resolveModuleRoot(moduleRootOverride: Option[String])(implicit trace: Trace): Task[File] =
    moduleRootOverride match {
      case Some(moduleRoot) => ZIO.succeed(new File(moduleRoot))
      case None             =>
        val rootFile =
          try new File(getClass.getResource("/").toURI)
          catch { // fixes "java.lang.IllegalArgumentException: URI is not hierarchical" in unit tests with Scala 2.12.20
            case _: IllegalArgumentException => new File(getClass.getResource(".").toURI)
          }
        getRootDir(rootFile).map(_.getParentFile)
    }

  def createGoldenDirectory(pathToDir: String)(implicit trace: Trace): Task[Path] =
    for {
      moduleRoot <- resolveModuleRoot(sys.props.get(moduleRootProperty))
      goldenDir   = new File(moduleRoot, pathToDir)
      path        = goldenDir.toPath
      _          <- ZIO.attemptBlocking(goldenDir.mkdirs)
    } yield path

  def writeSampleToFile(path: Path, sample: GoldenSample)(implicit trace: Trace): IO[IOException, Unit] = {
    val jsonString = sample.toJsonPretty

    ZIO.attemptBlockingIO {
      Files.write(path, jsonString.getBytes("UTF-8"))
    }.unit
  }

  def readSampleFromFile(path: Path)(implicit trace: Trace): Task[GoldenSample] =
    for {
      json   <- zio.json.readJsonAs(path.toFile).runHead.someOrFailException
      sample <- ZIO.fromEither(json.as[GoldenSample].left.map(error => new Exception(error)))
    } yield sample

}
