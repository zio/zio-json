package zio.json.golden

import java.io.{ File, IOException }
import java.nio.file.Path
import zio.{ test => _, _ }
import zio.json._

import java.nio.file.Files

/** File helpers that `goldenTest` uses to find the golden folder and to read and write golden files. */
object filehelpers {

  /**
   * System property naming the module root (the directory holding `src/test/resources/golden`). Needed when test
   * classes are not under `<module>/target`, e.g. sbt 2's `target/out/`. When unset, the root is found from `target`.
   */
  val moduleRootProperty: String = "zio.json.golden.moduleRoot"

  /**
   * Returns `file` itself when its name is `target`, or else its nearest ancestor named `target`. When neither `file` nor any ancestor is named `target`, the effect dies with a `java.lang.NullPointerException` instead of failing.
   */
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

  /**
   * Creates the folder `pathToDir` inside the module root, with any missing parent folders, and returns its path. The module root is the value of the system property named by `moduleRootProperty` when it is set; otherwise it is the parent of the `target` folder that `getRootDir` finds from the class path.
   *
   * The returned path is relative when the system property holds a relative path. When the property is unset and no `target` folder is found, the effect dies with a `java.lang.NullPointerException`.
   */
  def createGoldenDirectory(pathToDir: String)(implicit trace: Trace): Task[Path] =
    for {
      moduleRoot <- resolveModuleRoot(sys.props.get(moduleRootProperty))
      goldenDir   = new File(moduleRoot, pathToDir)
      path        = goldenDir.toPath
      _          <- ZIO.attemptBlocking(goldenDir.mkdirs)
    } yield path

  /**
   * Writes `sample` to the file at `path` as pretty-printed JSON in UTF-8, creating the file or replacing its content. Fails with a `java.io.IOException` when the file can't be written, for example a `java.nio.file.NoSuchFileException` when its folder doesn't exist.
   */
  def writeSampleToFile(path: Path, sample: GoldenSample)(implicit trace: Trace): IO[IOException, Unit] = {
    val jsonString = sample.toJsonPretty

    ZIO.attemptBlockingIO {
      Files.write(path, jsonString.getBytes("UTF-8"))
    }.unit
  }

  /**
   * Reads the first JSON value in the file at `path` and decodes it as a [[GoldenSample]].
   *
   * Fails with a `java.nio.file.NoSuchFileException` when the file doesn't exist, with a `java.util.NoSuchElementException` when it is empty, and with a `java.lang.Exception` that carries the error message when the content can't be decoded (`.samples(missing)` for a JSON object without a `samples` field).
   */
  def readSampleFromFile(path: Path)(implicit trace: Trace): Task[GoldenSample] =
    for {
      json   <- zio.json.readJsonAs(path.toFile).runHead.someOrFailException
      sample <- ZIO.fromEither(json.as[GoldenSample].left.map(error => new Exception(error)))
    } yield sample

}
