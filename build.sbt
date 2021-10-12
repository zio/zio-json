import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-json/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    )
  )
)

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "fix; fmt")

addCommandAlias(
  "testJVM",
  "zioJsonJVM/test; zioJsonYaml/test; zioJsonMacrosJVM/test; zioJsonInteropHttp4s/test; zioJsonInteropScalaz7xJVM/test; zioJsonInteropScalaz7xJS/test; zioJsonInteropRefinedJVM/test; zioJsonInteropRefinedJS/test"
)

addCommandAlias(
  "testJVMDotty",
  "zioJsonJVM/test"
)

addCommandAlias("testJS", "zioJsonJS/test")

val zioVersion = "1.0.12"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    docs,
    zioJsonJVM,
    zioJsonJS,
    zioJsonYaml,
    zioJsonMacrosJVM,
    zioJsonMacrosJS,
    zioJsonInteropHttp4s,
    zioJsonInteropRefined.js,
    zioJsonInteropRefined.jvm,
    zioJsonInteropScalaz7x.js,
    zioJsonInteropScalaz7x.jvm
  )

val circeVersion = "0.14.1"

lazy val zioJson = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-json"))
  .settings(stdSettings("zio-json"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.json"))
  .enablePlugins(NeoJmhPlugin)
  .settings(
    scalacOptions -= "-Xfatal-warnings", // not quite ready.

    // as per @fommil, optimization slows things down.
    scalacOptions -= "-opt:l:inline",
    scalacOptions -= "-opt-inline-from:zio.internal.**",
    scalaVersion := ScalaDotty,
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % zioVersion,
      "dev.zio"                %%% "zio-streams"             % zioVersion,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.5.0",
      "dev.zio"                %%% "zio-test"                % zioVersion   % "test",
      "dev.zio"                %%% "zio-test-sbt"            % zioVersion   % "test",
      "io.circe"               %%% "circe-core"              % circeVersion % "test",
      "io.circe"               %%% "circe-generic"           % circeVersion % "test",
      "io.circe"               %%% "circe-parser"            % circeVersion % "test"
    ),
    // scala version specific depenendies
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Vector(
            "com.softwaremill.magnolia" %% "magnolia-core" % "2.0.0-M9"
          )

        case _ =>
          Vector(
            "org.scala-lang"                          % "scala-reflect"         % scalaVersion.value % Provided,
            "com.propensive"                        %%% "magnolia"              % "0.17.0",
            "io.circe"                              %%% "circe-generic-extras"  % circeVersion       % "test",
            "com.typesafe.play"                     %%% "play-json"             % "2.9.2"            % "test",
            "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.9.1"            % "test",
            "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.9.1"            % "test"
          )
      }
    },
    Compile / sourceGenerators += Def.task {
      val dir  = (Compile / sourceManaged).value
      val file = dir / "zio" / "json" / "GeneratedTupleDecoders.scala"
      val decoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonDecoder[A$p]").mkString(", ")
        val work = (1 to i)
          .map(p => s"val a$p = A$p.unsafeDecode(trace :+ traces($p), in)")
          .mkString("\n        Lexer.char(trace, in, ',')\n        ")
        val returns = (1 to i).map(p => s"a$p").mkString(", ")

        s"""implicit def tuple$i[$tparams](implicit $implicits): JsonDecoder[Tuple$i[$tparams]] =
           |    new JsonDecoder[Tuple$i[$tparams]] {
           |      val traces: Array[JsonError] = (0 to $i).map(JsonError.ArrayAccess(_)).toArray
           |      def unsafeDecode(trace: List[JsonError], in: RetractReader): Tuple$i[$tparams] = {
           |        Lexer.char(trace, in, '[')
           |        $work
           |        Lexer.char(trace, in, ']')
           |        Tuple$i($returns)
           |      }
           |    }""".stripMargin
      }
      IO.write(
        file,
        s"""package zio.json
           |
           |import zio.json.internal._
           |
           |private[json] trait GeneratedTupleDecoders { this: JsonDecoder.type =>
           |  ${decoders.mkString("\n\n  ")}
           |}""".stripMargin
      )
      Seq(file)
    }.taskValue,
    Compile / sourceGenerators += Def.task {
      val dir  = (Compile / sourceManaged).value
      val file = dir / "zio" / "json" / "GeneratedTupleEncoders.scala"
      val encoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonEncoder[A$p]").mkString(", ")
        val work = (1 to i)
          .map(p => s"A$p.unsafeEncode(t._$p, indent, out)")
          .mkString("\n        if (indent.isEmpty) out.write(',') else out.write(\", \")\n        ")

        s"""implicit def tuple$i[$tparams](implicit $implicits): JsonEncoder[Tuple$i[$tparams]] =
           |    new JsonEncoder[Tuple$i[$tparams]] {
           |      def unsafeEncode(t: Tuple$i[$tparams], indent: Option[Int], out: internal.Write): Unit = {
           |        out.write('[')
           |        $work
           |        out.write(']')
           |      }
           |    }""".stripMargin
      }
      IO.write(
        file,
        s"""package zio.json
           |
           |private[json] trait GeneratedTupleEncoders { this: JsonEncoder.type =>
           |  ${encoders.mkString("\n\n  ")}
           |}""".stripMargin
      )
      Seq(file)
    }.taskValue,
    Compile / sourceGenerators += Def.task {
      val dir  = (Compile / sourceManaged).value
      val file = dir / "zio" / "json" / "GeneratedTupleCodecs.scala"
      val codecs = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonCodec[A$p]").mkString(", ")

        s"""implicit def tuple$i[$tparams](implicit $implicits): JsonCodec[Tuple$i[$tparams]] =
           |    JsonCodec(JsonEncoder.tuple$i, JsonDecoder.tuple$i)""".stripMargin
      }
      IO.write(
        file,
        s"""package zio.json
           |
           |private[json] trait GeneratedTupleCodecs { this: JsonCodec.type =>
           |  ${codecs.mkString("\n\n  ")}
           |}""".stripMargin
      )
      Seq(file)
    }.taskValue,
    inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % "2.3.0",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.3.0"
    )
  )
  .jvmSettings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Vector(
            "org.typelevel" %% "jawn-ast" % "1.2.0" % "test"
          )

        case _ =>
          Seq(
            "ai.x"          %% "play-json-extensions" % "0.42.0" % "test",
            "org.typelevel" %% "jawn-ast"             % "1.2.0"  % "test"
          )
      }
    }
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonJS = zioJson.js
  .settings(
    scalaJSUseMainModuleInitializer := true,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    coverageEnabled := false
  )

lazy val zioJsonJVM = zioJson.jvm

lazy val zioJsonYaml = project
  .in(file("zio-json-yaml"))
  .settings(stdSettings("zio-json-yaml"))
  .settings(buildInfoSettings("zio.json.yaml"))
  .settings(
    libraryDependencies ++= Seq(
      "org.yaml" % "snakeyaml"    % "1.29",
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zioJsonJVM)
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonMacros = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-json-macros"))
  .settings(stdSettings("zio-json-macros"))
  .settings(crossProjectSettings)
  .settings(macroExpansionSettings)
  .settings(
    crossScalaVersions --= Vector("3.0.2"),
    scalacOptions -= "-Xfatal-warnings", // not quite ready.
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      "dev.zio"      %%% "zio-test"      % zioVersion         % "test",
      "dev.zio"      %%% "zio-test-sbt"  % zioVersion         % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val zioJsonMacrosJVM = zioJsonMacros.jvm.dependsOn(zioJsonJVM)

lazy val zioJsonMacrosJS = zioJsonMacros.js
  .settings(coverageEnabled := false)
  .dependsOn(zioJsonJS)

lazy val zioJsonInteropHttp4s = project
  .in(file("zio-json-interop-http4s"))
  .settings(stdSettings("zio-json-interop-http4s"))
  .settings(buildInfoSettings("zio.json.interop.http4s"))
  .settings(
    crossScalaVersions --= Vector("3.0.2"),
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-dsl"       % "0.21.31",
      "dev.zio"       %% "zio"              % zioVersion,
      "org.typelevel" %% "cats-effect"      % "2.5.4",
      "dev.zio"       %% "zio-interop-cats" % "2.5.1.0"  % "test",
      "dev.zio"       %% "zio-test"         % zioVersion % "test",
      "dev.zio"       %% "zio-test-sbt"     % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zioJsonJVM)
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonInteropRefined = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-json-interop-refined"))
  .jvmConfigure(_.dependsOn(zioJsonJVM))
  .jsConfigure(_.dependsOn(zioJsonJS))
  .settings(stdSettings("zio-json-interop-refined"))
  .settings(buildInfoSettings("zio.json.interop.refined"))
  .settings(
    crossScalaVersions --= Vector("3.0.2"),
    libraryDependencies ++= Seq(
      "eu.timepit" %%% "refined"      % "0.9.27",
      "dev.zio"    %%% "zio-test"     % zioVersion % "test",
      "dev.zio"    %%% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonInteropScalaz7x = crossProject(JSPlatform, JVMPlatform)
  .in(file("zio-json-interop-scalaz7x"))
  .jvmConfigure(_.dependsOn(zioJsonJVM))
  .jsConfigure(_.dependsOn(zioJsonJS))
  .settings(stdSettings("zio-json-interop-scalaz7x"))
  .settings(buildInfoSettings("zio.json.interop.scalaz7x"))
  .settings(
    crossScalaVersions --= Vector("3.0.2"),
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % "7.3.5",
      "dev.zio"    %%% "zio-test"     % zioVersion % "test",
      "dev.zio"    %%% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .enablePlugins(BuildInfoPlugin)

lazy val docs = project
  .in(file("zio-json-docs"))
  .dependsOn(
    zioJsonJVM,
    zioJsonYaml,
    zioJsonMacrosJVM,
    zioJsonInteropHttp4s,
    zioJsonInteropRefined.jvm,
    zioJsonInteropScalaz7x.jvm
  )
  .settings(
    crossScalaVersions --= Vector("3.0.2"),
    publish / skip := true,
    mdocVariables := Map(
      "SNAPSHOT_VERSION" -> version.value,
      "RELEASE_VERSION"  -> previousStableVersion.value.getOrElse("can't find release"),
      "ORG"              -> organization.value,
      "NAME"             -> (zioJsonJVM / name).value,
      "CROSS_VERSIONS"   -> (zioJsonJVM / crossScalaVersions).value.mkString(", ")
    ),
    moduleName := "zio-json-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      zioJsonJVM,
      zioJsonYaml,
      zioJsonMacrosJVM,
      zioJsonInteropHttp4s,
      zioJsonInteropRefined.jvm,
      zioJsonInteropScalaz7x.jvm
    ),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
