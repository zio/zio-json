import sbt._
import sbt.Keys._

import explicitdeps.ExplicitDepsPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbtbuildinfo._
import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import BuildInfoKeys._
import scalafix.sbt.ScalafixPlugin.autoImport.scalafixSemanticdb

object BuildHelper {

  private val Scala211 = "2.11.12"
  private val Scala212 = "2.12.10"
  private val Scala213 = "2.13.1"
  val DottyVersion     = "0.23.0-RC1"

  def buildInfoSettings(packageName: String) =
    Seq(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
      buildInfoPackage := packageName,
      buildInfoObject := "BuildInfo"
    )

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked"
  )

  private val std2xOptions = Seq(
    "-language:higherKinds",
    "-language:existentials",
    "-explaintypes",
    "-Yrangepos",
    "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ) ++ customOptions

  private def propertyFlag(property: String, default: Boolean) =
    sys.props.get(property).map(_.toBoolean).getOrElse(default)

  private def customOptions =
    if (propertyFlag("fatal.warnings", true)) {
      Seq("-Xfatal-warnings")
    } else {
      Nil
    }

  private def optimizerOptions(optimize: Boolean) =
    if (optimize)
      Seq(
        "-opt:l:inline",
        "-opt-inline-from:zio.internal.**"
      )
    else Nil

  def extraOptions(scalaVersion: String, optimize: Boolean) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((0, _)) =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros"
        )
      case Some((2, 13)) =>
        Seq(
          "-Ywarn-unused:params,-implicits"
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
      case Some((2, 11)) =>
        Seq(
          "-Ypartial-unification",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Xexperimental",
          "-Ywarn-unused-import",
          "-Xfuture",
          "-Xsource:2.13",
          "-Xmax-classfile-name",
          "242"
        ) ++ std2xOptions
      case _ => Seq.empty
    }

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*) =
    List("scala" :: versions.toList.map("scala-" + _): _*).map { version =>
      baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
    }.filter(_.exists)

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File, isDotty: Boolean) =
    CrossVersion.partialVersion(scalaVer) match {
      case Some((2, x)) if x <= 11 =>
        platformSpecificSources(platform, conf, baseDir)("2.11", "2.x")
      case Some((2, x)) if x >= 12 =>
        platformSpecificSources(platform, conf, baseDir)("2.12+", "2.12", "2.x")
      case _ if isDotty =>
        platformSpecificSources(platform, conf, baseDir)("2.12+", "dotty")
      case _ =>
        Nil
    }

  val dottySettings = Seq(
    // Keep this consistent with the version in .circleci/config.yml
    crossScalaVersions += DottyVersion,
    scalacOptions ++= {
      if (isDotty.value)
        Seq("-noindent")
      else
        Seq()
    },
    libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value)),
    sources in (Compile, doc) := {
      val old = (Compile / doc / sources).value
      if (isDotty.value) {
        Nil
      } else {
        old
      }
    },
    parallelExecution in Test := {
      val old = (Test / parallelExecution).value
      if (isDotty.value) {
        false
      } else {
        old
      }
    }
  )

  val scalaReflectSettings = Seq(
    libraryDependencies ++=
      (if (isDotty.value) Seq()
       else
         Seq(
           "dev.zio" %%% "izumi-reflect" % "0.12.0-M0"
         ))
  )

  lazy val crossProjectSettings = Seq(
    Compile / unmanagedSourceDirectories ++= {
      val platform = crossProjectPlatform.value.identifier
      val baseDir  = baseDirectory.value
      val scalaVer = scalaVersion.value
      val isDot    = isDotty.value

      crossPlatformSources(scalaVer, platform, "main", baseDir, isDot)
    },
    Test / unmanagedSourceDirectories ++= {
      val platform = crossProjectPlatform.value.identifier
      val baseDir  = baseDirectory.value
      val scalaVer = scalaVersion.value
      val isDot    = isDotty.value

      crossPlatformSources(scalaVer, platform, "test", baseDir, isDot)
    }
  )

  def stdSettings(prjName: String) = Seq(
    name := s"$prjName",
    scalacOptions := stdOptions,
    crossScalaVersions := Seq(Scala213, Scala212, Scala211),
    scalaVersion in ThisBuild := crossScalaVersions.value.head,
    scalacOptions := stdOptions ++ extraOptions(scalaVersion.value, optimize = !isSnapshot.value),
    libraryDependencies ++= {
      if (isDotty.value)
        Seq("com.github.ghik" % "silencer-lib_2.13.1" % "1.6.0" % Provided)
      else
        Seq(
          "com.github.ghik" % "silencer-lib" % "1.4.4" % Provided cross CrossVersion.full,
          compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.4.4" cross CrossVersion.full),
          compilerPlugin(scalafixSemanticdb)
        )
    },
    parallelExecution in Test := true,
    incOptions ~= (_.withLogRecompileOnMacro(false)),
    autoAPIMappings := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library"),
    Compile / unmanagedSourceDirectories ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, x)) if x <= 11 =>
          Seq(
            Seq(file(sourceDirectory.value.getPath + "/main/scala-2.11")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.11")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + "-2.11")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.x"))
          ).flatten
        case Some((2, x)) if x >= 12 =>
          Seq(
            Seq(file(sourceDirectory.value.getPath + "/main/scala-2.12")),
            Seq(file(sourceDirectory.value.getPath + "/main/scala-2.12+")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.12+")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + "-2.12+")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.x")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.12-2.13"))
          ).flatten
        case _ =>
          if (isDotty.value)
            Seq(
              Seq(file(sourceDirectory.value.getPath + "/main/scala-2.12")),
              Seq(file(sourceDirectory.value.getPath + "/main/scala-2.12+")),
              CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.12+")),
              CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + "-2.12+")),
              CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-dotty"))
            ).flatten
          else
            Nil
      }
    },
    Test / unmanagedSourceDirectories ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, x)) if x <= 11 =>
          Seq(
            Seq(file(sourceDirectory.value.getPath + "/test/scala-2.11")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + "-2.x"))
          ).flatten
        case Some((2, x)) if x >= 12 =>
          Seq(
            Seq(file(sourceDirectory.value.getPath + "/test/scala-2.12")),
            Seq(file(sourceDirectory.value.getPath + "/test/scala-2.12+")),
            CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + "-2.x"))
          ).flatten
        case _ =>
          if (isDotty.value)
            Seq(
              Seq(file(sourceDirectory.value.getPath + "/test/scala-2.12+")),
              CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + "-2.12+")),
              CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + "-dotty"))
            ).flatten
          else
            Nil
      }

    }
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
    libraryDependencies ++=
      Seq("org.portable-scala" %%% "portable-scala-reflect" % "1.0.0") ++ {
        if (isDotty.value) Seq()
        else
          Seq(
            "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided",
            "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
          )
      }
  )

  def testJsSettings = Seq(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC5" % Test
  )

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }
}
