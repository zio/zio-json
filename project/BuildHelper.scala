import com.typesafe.tools.mima.core.Problem
import com.typesafe.tools.mima.core.ProblemFilters.exclude
import com.typesafe.tools.mima.plugin.MimaKeys.{ mimaBinaryIssueFilters, mimaFailOnProblem, mimaPreviousArtifacts }
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport.mimaCheckDirection
import explicitdeps.ExplicitDepsPlugin.autoImport.*
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.*
import sbt.Keys.*
import sbtbuildinfo.*
import sbtbuildinfo.BuildInfoKeys.*
import sbtcrossproject.CrossPlugin.autoImport.*
import sbtdynver.DynVerPlugin.autoImport.previousStableVersion
import scalafix.sbt.ScalafixPlugin.autoImport.scalafixSemanticdb

import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.*

object BuildHelper {
  val Scala212: String = "2.12.21"
  val Scala213: String = "2.13.18"
  val Scala3: String   = "3.3.8"

  val JdkReleaseVersion: String = "17"

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked"
  ) ++ {
    if (sys.env.contains("CI")) {
      // Seq("-Xfatal-warnings") // enable this when we are ready to enforce this
      Nil
    } else {
      Nil // to enable Scalafix locally
    }
  }

  private val std2xOptions = Seq(
    "-language:higherKinds",
    "-language:existentials",
    "-explaintypes",
    "-Yrangepos",
    "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  )

  private def optimizerOptions(optimize: Boolean) =
    if (optimize)
      Seq(
        "-opt:l:inline",
        "-opt-inline-from:zio.internal.**"
      )
    else Nil

  def buildInfoSettings(packageName: String) =
    Seq(
      buildInfoKeys    := Seq[BuildInfoKey](organization, moduleName, name, version, scalaVersion, sbtVersion, isSnapshot),
      buildInfoPackage := packageName
    )

  val dottySettings = Seq(
    crossScalaVersions += Scala3,
    scalacOptions ++= {
      if (scalaVersion.value == Scala3)
        Seq("-noindent")
      else
        Seq()
    },
    scalacOptions --= {
      if (scalaVersion.value == Scala3)
        Seq("-Xfatal-warnings")
      else
        Seq()
    },
    Compile / doc / sources := {
      val old = (Compile / doc / sources).value
      if (scalaVersion.value == Scala3) {
        Nil
      } else {
        old
      }
    },
    Test / parallelExecution := {
      val old = (Test / parallelExecution).value
      if (scalaVersion.value == Scala3) {
        false
      } else {
        old
      }
    }
  )

  // Keep this consistent with the version in .core-tests/shared/src/test/scala/REPLSpec.scala
  val replSettings = makeReplSettings {
    """|import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.Runtime.default._
       |implicit class RunSyntax[A](io: ZIO[ZEnv, Any, A]){ def unsafeRun: A = Runtime.default.unsafeRun(io.provideLayer(ZEnv.live)) }
    """.stripMargin
  }

  // Keep this consistent with the version in .streams-tests/shared/src/test/scala/StreamREPLSpec.scala
  val streamReplSettings = makeReplSettings {
    """|import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.stream._
       |import zio.Runtime.default._
       |implicit class RunSyntax[A](io: ZIO[ZEnv, Any, A]){ def unsafeRun: A = Runtime.default.unsafeRun(io.provideLayer(ZEnv.live)) }
    """.stripMargin
  }

  def makeReplSettings(initialCommandsStr: String) = Seq(
    // In the repl most warnings are useless or worse.
    // This is intentionally := as it's more direct to enumerate the few
    // options we do want than to try to subtract off the ones we don't.
    // One of -Ydelambdafy:inline or -Yrepl-class-based must be given to
    // avoid deadlocking on parallel operations, see
    //   https://issues.scala-lang.org/browse/SI-9076
    Compile / console / scalacOptions := Seq(
      "-Ypartial-unification",
      "-language:higherKinds",
      "-language:existentials",
      "-Yno-adapted-args",
      "-Xsource:2.13",
      "-Yrepl-class-based"
    ),
    Compile / console / initialCommands := initialCommandsStr
  )

  def extraOptions(scalaVersion: String, optimize: Boolean) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros"
        )
      case Some((2, 13)) =>
        Seq(
          "-Ywarn-unused:params,-implicits",
          "-Wconf:msg=Boolean literals should be passed:s"
        ) ++ std2xOptions ++ optimizerOptions(optimize)
      case Some((2, 12)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused:_,imports",
          "-Ywarn-unused:imports",
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Ywarn-unused:params,-implicits",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ std2xOptions ++ optimizerOptions(optimize)
      case _ => Seq.empty
    }

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*) = for {
    platform <- List("shared", platform)
    version  <- "scala" :: versions.toList.map("scala-" + _)
    result    = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
    if result.exists
  } yield result

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File) = {
    val versions = CrossVersion.partialVersion(scalaVer) match {
      case Some((2, 12)) =>
        List("2.12", "2.12+", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case Some((3, _)) =>
        List("dotty", "2.12+", "2.13+", "3.x")
      case _ =>
        List.empty
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings = Seq(
    Compile / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "main",
        baseDirectory.value
      )
    },
    Test / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "test",
        baseDirectory.value
      )
    }
  )

  def stdSettings(prjName: String) = Seq(
    name               := s"$prjName",
    crossScalaVersions := Seq(Scala212, Scala213, Scala3),
    javacOptions ++= Seq("-source", JdkReleaseVersion, "-target", JdkReleaseVersion),
    scalacOptions ++= Seq(s"-release:$JdkReleaseVersion"),
    scalacOptions ++= stdOptions ++ extraOptions(scalaVersion.value, optimize = !isSnapshot.value),
    libraryDependencies ++= {
      if (scalaVersion.value == Scala3) Seq.empty
      else
        Seq(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.full)
        )
    },
    libraryDependencySchemes ++= Seq(
      "org.scala-native" %% "test-interface_native0.5" % VersionScheme.Always
    ),
    versionScheme     := Some("early-semver"),
    semanticdbEnabled := scalaVersion.value == Scala213, // enable SemanticDB
    semanticdbOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("-P:semanticdb:synthetics:on")
      case _            => Seq.empty
    }),
    semanticdbVersion        := scalafixSemanticdb.revision, // use Scalafix compatible version,
    Test / parallelExecution := true,
    incOptions ~= (_.withLogRecompileOnMacro(false)),
    autoAPIMappings := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library"),
    mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %% name.value % _).toSet,
    mimaCheckDirection    := "backward", // TODO: find how we can use "both" for patch versions of 1.x releases
    mimaBinaryIssueFilters ++= Seq(
      exclude[Problem]("zio.json.internal.*"),
      exclude[Problem]("zio.json.yaml.internal.*")
    ),
    mimaFailOnProblem := true
  )

  def macroExpansionSettings = Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-Ymacro-annotations")
        case _             => Seq.empty
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, x)) if x <= 12 =>
          Seq(compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)))
        case _ => Seq.empty
      }
    }
  )

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (scalaVersion.value == Scala3) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided",
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
        )
    }
  )

  val scalaJavaTimeVersion = "2.6.0"

  def nativeSettings = Seq(
    nativeConfig ~= { cfg => cfg.withMode(scala.scalanative.build.Mode.releaseFast) },
    scalacOptions += "-P:scalanative:genStaticForwardersForNonTopLevelObjects",
    Test / fork := false
  )

  val scalaReflectTestSettings: List[Setting[_]] = List(
    libraryDependencies ++= {
      if (scalaVersion.value == Scala3)
        Seq("org.scala-lang" % "scala-reflect" % Scala213 % Test)
      else
        Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Test)
    }
  )

  def welcomeMessage = onLoadMessage := {
    def header(text: String): String = s"${scala.Console.RED}$text${scala.Console.RESET}"

    def item(text: String): String    = s"${scala.Console.GREEN}> ${scala.Console.CYAN}$text${scala.Console.RESET}"
    def subItem(text: String): String = s"  ${scala.Console.YELLOW}> ${scala.Console.CYAN}$text${scala.Console.RESET}"

    s"""|${header(" ________ ___")}
        |${header("|__  /_ _/ _ \\")}
        |${header("  / / | | | | |")}
        |${header(" / /_ | | |_| |")}
        |${header(s"/____|___\\___/   ${version.value}")}
        |
        |Useful sbt tasks:
        |${item("build")} - Prepares sources, compiles and runs tests.
        |${item("prepare")} - Prepares sources by applying scalafmt
        |${item("fmt")} - Formats source files using scalafmt
        |${item("~compileJVM")} - Compiles all JVM modules (file-watch enabled)
        |${item("testJVM")} - Runs all JVM tests
        |${item("testJS")} - Runs all ScalaJS tests
        |${item("testOnly *.YourSpec -- -t \"YourLabel\"")} - Only runs tests with matching term e.g.
        |${subItem("coreTestsJVM/testOnly *.ZIOSpec -- -t \"happy-path\"")}
        |${item("docs/docusaurusCreateSite")} - Generates the ZIO microsite
      """.stripMargin
  }

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }
}
