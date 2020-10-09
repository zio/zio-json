package fommil

import sbt._
import sbt.Keys._

import scala.collection.immutable.Set
import scala.util.Try

object NeoJmhKeys {

  val jmhVersion       = settingKey[String]("version of jmh")
  val jmhExtrasVersion = settingKey[String]("version of jmh-extras")

  /** Where you put your JMH code. */
  val Jmh = config("jmh") extend Test

  val neoJmhGenerator = settingKey[String]("Available: `reflection` or `asm`")

  /**
   * If non-empty, yourkit will be enabled.
   *
   * https://www.yourkit.com/docs/java/help/startup_options.jsp
   */
  val neoJmhYourkit =
    settingKey[Seq[String]]("Startup options for Yourkit")

}

/**
 * https://github.com/ktoso/sbt-jmh/ rewritten as an idiomatic sbt
 * Configuration (not requiring a separate Project).
 */
object NeoJmhPlugin extends AutoPlugin {
  import NeoJmhKeys._
  val autoImport = NeoJmhKeys

  val JmhInternal = (config("jmh-internal") extend Test).hide

  val generateJmhSourcesAndResources = taskKey[(Seq[File], Seq[File])](
    "Generate benchmark JMH Java code and resources"
  )

  override def requires              = plugins.JvmPlugin
  override def trigger               = noTrigger
  override def projectConfigurations = Seq(Jmh, JmhInternal)

  override def buildSettings = Seq(
    jmhVersion := "1.26",
    jmhExtrasVersion := "0.3.7"
  )

  override def projectSettings =
    inConfig(Jmh)(
      Defaults.testSettings ++ Seq(
        run := (run in JmhInternal).evaluated,
        neoJmhGenerator := "reflection",
        neoJmhYourkit := Nil,
        javaOptions ++= Seq(
          "-XX:+PerfDisableSharedMem",
          "-XX:+AlwaysPreTouch",
          "-Xms1g",
          "-Xmx1g"
        ),
        javaOptions ++= {
          if (neoJmhYourkit.value.isEmpty) Nil
          else {
            val flags = "=" + neoJmhYourkit.value.mkString(",")
            List(s"-agentpath:${sys.env("YOURKIT_AGENT")}$flags")
          }
        }
      )
    ) ++ inConfig(JmhInternal)(
      Defaults.testSettings ++ Seq(
        javaOptions := (javaOptions in Jmh).value,
        envVars := (envVars in Jmh).value,
        mainClass in run := Some("org.openjdk.jmh.Main"),
        fork in run := true,
        dependencyClasspath ++= (fullClasspath in Jmh).value,
        sourceGenerators += generateJmhSourcesAndResources.map { case (sources, _) =>
          sources
        },
        resourceGenerators += generateJmhSourcesAndResources.map { case (_, res) =>
          res
        },
        generateJmhSourcesAndResources := generateBenchmarkSourcesAndResources.value
      )
    ) ++ Seq(
      libraryDependencies ++= Seq(
        "jmh-core",
        "jmh-generator-bytecode",
        "jmh-generator-reflection",
        "jmh-generator-asm"
      ).map(
        // WORKAROUND: https://github.com/sbt/sbt/issues/1380
        "org.openjdk.jmh" % _ % jmhVersion.value % s"${Jmh.name},test"
      ) :+ "pl.project13.scala" % "sbt-jmh-extras" % jmhExtrasVersion.value % s"${Jmh.name},test"
    ) ++ backCompatProjectSettings

  implicit class BackCompatDepOverrides[A](val deps: Set[A]) {
    def compat: Seq[A] = deps.toSeq
  }

  implicit class BackCompatForkRun(val result: Try[Unit]) {
    def dealWithIt(): Unit = result.failed.foreach(f => sys.error(f.getMessage))
  }

  def backCompatProjectSettings: Seq[Setting[_]] = Seq(
    // WORKAROUND https://github.com/sbt/sbt/issues/3935
    dependencyClasspathAsJars in NeoJmhPlugin.JmhInternal ++= (fullClasspathAsJars in NeoJmhKeys.Jmh).value
  )

  def generateBenchmarkSourcesAndResources: Def.Initialize[Task[(Seq[File], Seq[File])]] = Def.task {
    val s                = streams.value
    val cacheDir         = crossTarget.value / "jmh-cache"
    val bytecodeDir      = (classDirectory in Jmh).value
    val sourceDir        = sourceManaged.value
    val resourceDir      = resourceManaged.value
    val generator        = (neoJmhGenerator in Jmh).value
    val classpath        = dependencyClasspath.value
    val javaHomeV        = (javaHome in Jmh).value
    val outputStrategyV  = (outputStrategy in Jmh).value
    val workingDirectory = Option((baseDirectory in Jmh).value)
    val connectInputV    = (connectInput in Jmh).value
    val envVarsV         = (envVars in Jmh).value
    val javaFlags        = (javaOptions in Jmh).value.toVector

    val inputs: Set[File] = (bytecodeDir ** "*").filter(_.isFile).get.toSet
    val cachedGeneration = FileFunction.cached(cacheDir, FilesInfo.hash) { _ =>
      IO.delete(sourceDir)
      IO.createDirectory(sourceDir)
      IO.delete(resourceDir)
      IO.createDirectory(resourceDir)

      val options = ForkOptions(
        javaHome = javaHomeV,
        outputStrategy = outputStrategyV,
        bootJars = Vector.empty[java.io.File],
        workingDirectory = workingDirectory,
        runJVMOptions = javaFlags,
        connectInput = connectInputV,
        envVars = envVarsV
      )
      new ForkRun(options)
        .run(
          "org.openjdk.jmh.generators.bytecode.JmhBytecodeGenerator",
          Attributed.data(classpath),
          List(
            bytecodeDir.getPath,
            sourceDir.getPath,
            resourceDir.getPath,
            generator
          ),
          s.log
        )
        .dealWithIt()

      ((sourceDir ** "*").filter(_.isFile) +++ (resourceDir ** "*").filter(
        _.isFile
      )).get.toSet
    }
    cachedGeneration(inputs).toSeq
      .partition(f => IO.relativizeFile(sourceDir, f).nonEmpty)
  }

}
