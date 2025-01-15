import BuildHelper.*
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

Global / onChangedBuildSource := IgnoreSourceChanges

inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.dev/zio-json/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
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

addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "fmt")

addCommandAlias(
  "testJVM",
  "zioJsonJVM/test; zioJsonYaml/test; zioJsonInteropHttp4s/test; zioJsonInteropScalaz7xJVM/test; zioJsonGolden/test; zioJsonInteropRefinedJVM/test"
)

addCommandAlias(
  "testJS",
  "zioJsonJS/test; zioJsonInteropScalaz7xJS/test; zioJsonInteropRefinedJS/test"
)

addCommandAlias(
  "testNative",
  "zioJsonNative/test; zioJsonInteropScalaz7xNative/test; zioJsonInteropRefinedNative/test"
)

addCommandAlias(
  "testScala2JVM",
  "zioJsonMacrosJVM/test"
)

addCommandAlias(
  "testScala2JS",
  "zioJsonMacrosJS/test"
)

addCommandAlias(
  "testScala2Native",
  "zioJsonMacrosNative/test"
)

val zioVersion = "2.1.14"

lazy val zioJsonRoot = project
  .in(file("."))
  .settings(
    publish / skip        := true,
    mimaPreviousArtifacts := Set(),
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    docs,
    zioJsonJVM,
    zioJsonJS,
    zioJson.native,
    zioJsonYaml,
    zioJsonMacrosJVM,
    zioJsonMacrosJS,
    zioJsonMacros.native,
    zioJsonInteropHttp4s,
    zioJsonInteropRefined.js,
    zioJsonInteropRefined.jvm,
    zioJsonInteropRefined.native,
    zioJsonInteropScalaz7x.js,
    zioJsonInteropScalaz7x.jvm,
    zioJsonInteropScalaz7x.native,
    zioJsonGolden
  )

val circeVersion = "0.14.10"

lazy val zioJson = crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
    Test / scalacOptions ++= {
      if (scalaVersion.value == ScalaDotty)
        Vector("-Yretain-trees")
      else
        Vector.empty
    },
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % zioVersion,
      "dev.zio"                %%% "zio-streams"             % zioVersion,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.12.0",
      "dev.zio"                %%% "zio-test"                % zioVersion   % "test",
      "dev.zio"                %%% "zio-test-sbt"            % zioVersion   % "test",
      "io.circe"               %%% "circe-core"              % circeVersion % "test",
      "io.circe"               %%% "circe-generic"           % circeVersion % "test",
      "io.circe"               %%% "circe-parser"            % circeVersion % "test"
    ),
    // scala version specific dependencies
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Vector(
            "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.7"
          )

        case _ =>
          Vector(
            "org.scala-lang"                          % "scala-reflect"         % scalaVersion.value % Provided,
            "com.softwaremill.magnolia1_2"          %%% "magnolia"              % "1.1.10",
            "io.circe"                              %%% "circe-generic-extras"  % "0.14.4"           % "test",
            "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.33.0"           % "test",
            "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.33.0"           % "test"
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
        val tparamDecls = (1 to i).map(p => s"A$p: JsonEncoder: JsonDecoder").mkString(", ")
        val tparams     = (1 to i).map(p => s"A$p").mkString(", ")

        s"""implicit def tuple$i[$tparamDecls]: JsonCodec[Tuple$i[$tparams]] =
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
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion
    )
  )
  .jvmSettings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Vector(
            "org.typelevel" %% "jawn-ast" % "1.6.0" % "test"
          )

        case Some((2, n)) =>
          if (n >= 13) {
            Seq(
              "com.particeep"      %% "play-json-extensions" % "0.43.1" % "test",
              "com.typesafe.play" %%% "play-json"            % "2.10.6" % "test",
              "org.typelevel"      %% "jawn-ast"             % "1.6.0"  % "test"
            )
          } else {
            Seq(
              "ai.x"               %% "play-json-extensions" % "0.42.0" % "test",
              "com.typesafe.play" %%% "play-json"            % "2.10.6" % "test",
              "org.typelevel"      %% "jawn-ast"             % "1.6.0"  % "test"
            )
          }

        case _ =>
          Seq.empty
      }
    }
  )
  .nativeSettings(nativeSettings)
  .nativeSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonJS = zioJson.js
  .settings(
    scalaJSUseMainModuleInitializer := true,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    coverageEnabled := false
  )

lazy val zioJsonJVM = zioJson.jvm

lazy val zioJsonGolden = project
  .in(file("zio-json-golden"))
  .settings(stdSettings("zio-json-golden"))
  .settings(buildInfoSettings("zio.json.golden"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"               % zioVersion,
      "dev.zio" %% "zio-test"          % zioVersion,
      "dev.zio" %% "zio-test-sbt"      % zioVersion,
      "dev.zio" %% "zio-test-magnolia" % zioVersion
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zioJsonJVM)
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonYaml = project
  .in(file("zio-json-yaml"))
  .settings(stdSettings("zio-json-yaml"))
  .settings(buildInfoSettings("zio.json.yaml"))
  .settings(
    libraryDependencies ++= Seq(
      "org.yaml" % "snakeyaml"    % "2.3",
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zioJsonJVM)
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonMacros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-json-macros"))
  .nativeConfigure(_.dependsOn(zioJson.native))
  .settings(stdSettings("zio-json-macros"))
  .settings(crossProjectSettings)
  .settings(macroExpansionSettings)
  .settings(
    crossScalaVersions -= ScalaDotty,
    scalacOptions -= "-Xfatal-warnings", // not quite ready.
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      "dev.zio"      %%% "zio-test"      % zioVersion         % "test",
      "dev.zio"      %%% "zio-test-sbt"  % zioVersion         % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .nativeSettings(nativeSettings)

lazy val zioJsonMacrosJVM = zioJsonMacros.jvm.dependsOn(zioJsonJVM)

lazy val zioJsonMacrosJS = zioJsonMacros.js
  .settings(coverageEnabled := false)
  .dependsOn(zioJsonJS)

lazy val zioJsonInteropHttp4s = project
  .in(file("zio-json-interop-http4s"))
  .settings(stdSettings("zio-json-interop-http4s"))
  .settings(buildInfoSettings("zio.json.interop.http4s"))
  .settings(
    crossScalaVersions -= ScalaDotty,
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-dsl"       % "0.23.30",
      "dev.zio"       %% "zio"              % zioVersion,
      "org.typelevel" %% "cats-effect"      % "3.5.7",
      "dev.zio"       %% "zio-interop-cats" % "23.1.0.3" % "test",
      "dev.zio"       %% "zio-test"         % zioVersion % "test",
      "dev.zio"       %% "zio-test-sbt"     % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zioJsonJVM)
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonInteropRefined = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-json-interop-refined"))
  .dependsOn(zioJson)
  .settings(stdSettings("zio-json-interop-refined"))
  .settings(buildInfoSettings("zio.json.interop.refined"))
  .settings(
    libraryDependencies ++= Seq(
      "eu.timepit" %%% "refined"      % "0.11.3",
      "dev.zio"    %%% "zio-test"     % zioVersion % "test",
      "dev.zio"    %%% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioJsonInteropScalaz7x = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-json-interop-scalaz7x"))
  .dependsOn(zioJson)
  .settings(stdSettings("zio-json-interop-scalaz7x"))
  .settings(buildInfoSettings("zio.json.interop.scalaz7x"))
  .settings(
    crossScalaVersions -= ScalaDotty,
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % "7.3.8",
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
    zioJsonGolden,
    zioJsonMacrosJVM,
    zioJsonInteropHttp4s,
    zioJsonInteropRefined.jvm,
    zioJsonInteropScalaz7x.jvm
  )
  .settings(
    crossScalaVersions -= ScalaDotty,
    moduleName := "zio-json-docs",
    scalacOptions += "-Ymacro-annotations",
    projectName    := "ZIO JSON",
    mainModuleName := (zioJsonJVM / moduleName).value,
    projectStage   := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      zioJsonJVM,
      zioJsonYaml,
      zioJsonMacrosJVM,
      zioJsonInteropHttp4s,
      zioJsonInteropRefined.jvm,
      zioJsonInteropScalaz7x.jvm,
      zioJsonGolden
    ),
    mimaPreviousArtifacts := Set(),
    readmeAcknowledgement :=
      """|- Uses [JsonTestSuite](https://github.com/nst/JSONTestSuite) to test parsing. (c) 2016 Nicolas Seriot)
         |
         |- Uses [YourKit Java Profiler](https://www.yourkit.com/java/profiler/) for performance optimisation. ![YourKit Logo](https://www.yourkit.com/images/yklogo.png)
         |""".stripMargin
  )
  .enablePlugins(WebsitePlugin)
