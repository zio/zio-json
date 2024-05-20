import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

Global / onChangedBuildSource := IgnoreSourceChanges

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.dev/zio-json/")),
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

addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "fmt")

addCommandAlias(
  "testJVM",
  "zioJsonJVM/test; zioJsonYaml/test; zioJsonInteropHttp4s/test; zioJsonInteropScalaz7xJVM/test; zioJsonGolden/test"
)

addCommandAlias(
  "testScala2JVM",
  "zioJsonMacrosJVM/test; zioJsonInteropRefinedJVM/test"
)

addCommandAlias(
  "testScala2JS",
  "zioJsonMacrosJS/test; zioJsonInteropRefinedJS/test"
)

addCommandAlias(
  "testScala2Native",
  "zioJsonMacrosNative/test; zioJsonInteropRefinedNative/test"
)

addCommandAlias(
  "testJS",
  "zioJsonJS/test; zioJsonInteropScalaz7xJS/test"
)

addCommandAlias(
  "testNative",
  "zioJsonNative/test; zioJsonInteropScalaz7xNative/test"
)

lazy val zioJsonRoot = project
  .in(file("."))
  .settings(
    publish / skip := true,
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
      "dev.zio"                %%% "zio"                     % Dependencies.zio,
      "dev.zio"                %%% "zio-streams"             % Dependencies.zio,
      "org.scala-lang.modules" %%% "scala-collection-compat" % Dependencies.scalaCollectionCompat,
      "dev.zio"                %%% "zio-test"                % Dependencies.zio   % "test",
      "dev.zio"                %%% "zio-test-sbt"            % Dependencies.zio   % "test",
      "io.circe"               %%% "circe-core"              % Dependencies.circe % "test",
      "io.circe"               %%% "circe-generic"           % Dependencies.circe % "test",
      "io.circe"               %%% "circe-parser"            % Dependencies.circe % "test"
    ),
    // scala version specific dependencies
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Vector(
            "com.softwaremill.magnolia1_3" %%% "magnolia" % Dependencies.magnolia3
          )

        case _ =>
          Vector(
            "org.scala-lang"                          % "scala-reflect"         % scalaVersion.value         % Provided,
            "com.softwaremill.magnolia1_2"          %%% "magnolia"              % Dependencies.magnolia2,
            "io.circe"                              %%% "circe-generic-extras"  % Dependencies.circe         % "test",
            "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % Dependencies.jsoniterScala % "test",
            "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % Dependencies.jsoniterScala % "test"
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
      "io.github.cquiroz" %%% "scala-java-time"      % Dependencies.scalaJavaTime,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % Dependencies.scalaJavaTime
    )
  )
  .jvmSettings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Vector(
            "org.typelevel" %% "jawn-ast" % Dependencies.jawnAST % "test"
          )

        case Some((2, n)) =>
          if (n >= 13) {
            Seq(
              "com.particeep"      %% "play-json-extensions" % Dependencies.playJsonExtensions % "test",
              "com.typesafe.play" %%% "play-json"            % Dependencies.playJson           % "test",
              "org.typelevel"      %% "jawn-ast"             % Dependencies.jawnAST            % "test"
            )
          } else {
            Seq(
              "ai.x"               %% "play-json-extensions" % Dependencies.playJsonExtensions2_12 % "test",
              "com.typesafe.play" %%% "play-json"            % Dependencies.playJson               % "test",
              "org.typelevel"      %% "jawn-ast"             % Dependencies.jawnAST                % "test"
            )
          }

        case _ =>
          Seq.empty
      }
    }
  )
  .nativeSettings(Test / fork := false)
  .nativeSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % Dependencies.scalaJavaTime
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
      "dev.zio" %% "zio"               % Dependencies.zio,
      "dev.zio" %% "zio-test"          % Dependencies.zio,
      "dev.zio" %% "zio-test-sbt"      % Dependencies.zio,
      "dev.zio" %% "zio-test-magnolia" % Dependencies.zio
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
      "org.yaml" % "snakeyaml"    % Dependencies.snakeYaml,
      "dev.zio" %% "zio"          % Dependencies.zio,
      "dev.zio" %% "zio-test"     % Dependencies.zio % "test",
      "dev.zio" %% "zio-test-sbt" % Dependencies.zio % "test"
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
      "dev.zio"      %%% "zio-test"      % Dependencies.zio   % "test",
      "dev.zio"      %%% "zio-test-sbt"  % Dependencies.zio   % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .nativeSettings(Test / fork := false)

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
      "org.http4s"    %% "http4s-dsl"       % Dependencies.http4s,
      "dev.zio"       %% "zio"              % Dependencies.zio,
      "org.typelevel" %% "cats-effect"      % Dependencies.catsEffect,
      "dev.zio"       %% "zio-interop-cats" % Dependencies.zioInteropCats % "test",
      "dev.zio"       %% "zio-test"         % Dependencies.zio            % "test",
      "dev.zio"       %% "zio-test-sbt"     % Dependencies.zio            % "test"
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
      "eu.timepit" %%% "refined"      % Dependencies.refined,
      "dev.zio"    %%% "zio-test"     % Dependencies.zio % "test",
      "dev.zio"    %%% "zio-test-sbt" % Dependencies.zio % "test"
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
      "org.scalaz" %%% "scalaz-core"  % Dependencies.scalaz,
      "dev.zio"    %%% "zio-test"     % Dependencies.zio % "test",
      "dev.zio"    %%% "zio-test-sbt" % Dependencies.zio % "test"
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
    projectName := "ZIO JSON",
    mainModuleName := (zioJsonJVM / moduleName).value,
    projectStage := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      zioJsonJVM,
      zioJsonYaml,
      zioJsonMacrosJVM,
      zioJsonInteropHttp4s,
      zioJsonInteropRefined.jvm,
      zioJsonInteropScalaz7x.jvm,
      zioJsonGolden
    ),
    readmeAcknowledgement :=
      """|- Uses [JsonTestSuite](https://github.com/nst/JSONTestSuite) to test parsing. (c) 2016 Nicolas Seriot)
         |
         |- Uses [YourKit Java Profiler](https://www.yourkit.com/java/profiler/) for performance optimisation. ![YourKit Logo](https://www.yourkit.com/images/yklogo.png)
         |""".stripMargin
  )
  .enablePlugins(WebsitePlugin)
