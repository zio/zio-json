import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

Global / onChangedBuildSource := IgnoreSourceChanges

enablePlugins(ZioSbtEcosystemPlugin, ZioSbtCiPlugin)

usefulTasksAndSettings ++= BuildHelper.usefulTasksAndSettings.value

inThisBuild(
  List(
    organization := "dev.zio",
    name         := "zio-json",
    homepage     := Some(url("https://zio.dev/zio-json/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    scalaVersion      := scala213.value,
    javaPlatform      := zio.sbt.JavaVersion.`11`,
    ciEnabledBranches := Seq("series/2.x"),
    checkMima / skip  := true
  )
)

lazy val zioJsonRoot = project
  .in(file("."))
  .settings(
    publish / skip := true
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
  .settings(stdSettings(Some("zio-json"), turnCompilerWarningIntoErrors = false))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.json"))
  .enablePlugins(NeoJmhPlugin)
  .settings(
    // as per @fommil, optimization slows things down.
    scalacOptions -= "-opt:l:inline",
    scalacOptions -= "-opt-inline-from:zio.internal.**",
    Test / scalacOptions ++= {
      if (isScala3.value)
        Vector("-Yretain-trees")
      else
        Vector.empty
    },
    libraryDependencies ++= Seq(
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-streams"             % Version.zio,
      "org.scala-lang.modules" %%% "scala-collection-compat" % "2.12.0",
      "dev.zio"                %%% "zio-test"                % Version.zio   % "test",
      "dev.zio"                %%% "zio-test-sbt"            % Version.zio   % "test",
      "io.circe"               %%% "circe-core"              % Version.circe % "test",
      "io.circe"               %%% "circe-generic"           % Version.circe % "test",
      "io.circe"               %%% "circe-parser"            % Version.circe % "test"
    ),
    // scala version specific dependencies
    libraryDependencies ++= {
      if (isScala3.value)
        Vector(
          "com.softwaremill.magnolia1_3" %%% "magnolia"                        % Version.magnolia1_3,
          "com.github.ghik"                % s"silencer-lib_${scala213.value}" % Version.silencer % Provided
        )
      else
        Vector(
          "com.github.ghik" % "silencer-lib" % Version.silencer % Provided cross CrossVersion.full,
          compilerPlugin("com.github.ghik" % "silencer-plugin" % Version.silencer cross CrossVersion.full),
          compilerPlugin("org.typelevel"  %% "kind-projector"  % "0.13.3" cross CrossVersion.full),
          "org.scala-lang"                          % "scala-reflect"         % scalaVersion.value % Provided,
          "com.softwaremill.magnolia1_2"          %%% "magnolia"              % Version.magnolia1_2,
          "io.circe"                              %%% "circe-generic-extras"  % "0.14.4"           % "test",
          "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % "2.30.9"           % "test",
          "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.30.9"           % "test"
        )
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
    },
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
  .jsSettings(
    jsSettings ++ Seq(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time"      % Version.scalaJavaTime,
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % Version.scalaJavaTime
      ),
      scalaJSUseMainModuleInitializer := true,
      coverageEnabled                 := false
    )
  )
  .jvmSettings(jvmSettings)
  .nativeSettings(
    nativeSettings ++ Seq(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time" % Version.scalaJavaTime
      )
    )
  )
  .jvmSettings(
    libraryDependencies ++= {
      if (isScala3.value)
        Vector(
          "org.typelevel" %% "jawn-ast" % "1.6.0" % "test"
        )
      else if (isScala2_13.value)
        Seq(
          "com.particeep"      %% "play-json-extensions" % "0.43.1" % "test",
          "com.typesafe.play" %%% "play-json"            % "2.9.4"  % "test",
          "org.typelevel"      %% "jawn-ast"             % "1.6.0"  % "test"
        )
      else if (isScala2_12.value)
        Seq(
          "ai.x"               %% "play-json-extensions" % "0.42.0" % "test",
          "com.typesafe.play" %%% "play-json"            % "2.9.4"  % "test",
          "org.typelevel"      %% "jawn-ast"             % "1.6.0"  % "test"
        )
      else
        Seq.empty
    }
  )

lazy val zioJsonJS = zioJson.js

lazy val zioJsonJVM = zioJson.jvm

lazy val zioJsonGolden = project
  .in(file("zio-json-golden"))
  .settings(stdSettings(Some("zio-json-golden"), turnCompilerWarningIntoErrors = false))
  .settings(buildInfoSettings("zio.json.golden"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"               % Version.zio,
      "dev.zio" %% "zio-test"          % Version.zio,
      "dev.zio" %% "zio-test-sbt"      % Version.zio,
      "dev.zio" %% "zio-test-magnolia" % Version.zio
    )
  )
  .dependsOn(zioJsonJVM)

lazy val zioJsonYaml = project
  .in(file("zio-json-yaml"))
  .settings(stdSettings(Some("zio-json-yaml")))
  .settings(buildInfoSettings("zio.json.yaml"))
  .settings(
    libraryDependencies ++= Seq(
      "org.yaml" % "snakeyaml"    % Version.snakeyaml,
      "dev.zio" %% "zio"          % Version.zio,
      "dev.zio" %% "zio-test"     % Version.zio % "test",
      "dev.zio" %% "zio-test-sbt" % Version.zio % "test"
    )
  )
  .dependsOn(zioJsonJVM)

lazy val zioJsonMacros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-json-macros"))
  .dependsOn(zioJson)
  .settings(stdSettings(Some("zio-json-macros"), turnCompilerWarningIntoErrors = false))
  .settings(crossProjectSettings)
  .settings(macroExpansionSettings)
  .settings(
    crossScalaVersions -= scala3.value,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      "dev.zio"      %%% "zio-test"      % Version.zio        % "test",
      "dev.zio"      %%% "zio-test-sbt"  % Version.zio        % "test"
    )
  )
  .jsSettings(
    coverageEnabled := false
  )
  .jsSettings(jsSettings)
  .jvmSettings(jvmSettings)
  .nativeSettings(nativeSettings)

lazy val zioJsonMacrosJVM = zioJsonMacros.jvm

lazy val zioJsonMacrosJS = zioJsonMacros.js

lazy val zioJsonInteropHttp4s = project
  .in(file("zio-json-interop-http4s"))
  .settings(stdSettings(Some("zio-json-interop-http4s"), turnCompilerWarningIntoErrors = false))
  .settings(buildInfoSettings("zio.json.interop.http4s"))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-dsl"       % Version.http4s,
      "dev.zio"       %% "zio"              % Version.zio,
      "org.typelevel" %% "cats-effect"      % Version.catsEffect,
      "dev.zio"       %% "zio-interop-cats" % Version.zioInteropCats % "test",
      "dev.zio"       %% "zio-test"         % Version.zio            % "test",
      "dev.zio"       %% "zio-test-sbt"     % Version.zio            % "test"
    )
  )
  .dependsOn(zioJsonJVM)

lazy val zioJsonInteropRefined = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-json-interop-refined"))
  .dependsOn(zioJson)
  .settings(stdSettings(Some("zio-json-interop-refined"), turnCompilerWarningIntoErrors = false))
  .settings(buildInfoSettings("zio.json.interop.refined"))
  .settings(
    crossScalaVersions -= scala3.value, // no working version of refined for scala 3 yet, published artifacts are incomplete
    libraryDependencies ++= Seq(
      "eu.timepit" %%% "refined"      % Version.refined,
      "dev.zio"    %%% "zio-test"     % Version.zio % "test",
      "dev.zio"    %%% "zio-test-sbt" % Version.zio % "test"
    )
  )
  .jsSettings(jsSettings)
  .jvmSettings(jvmSettings)
  .nativeSettings(nativeSettings)

lazy val zioJsonInteropScalaz7x = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-json-interop-scalaz7x"))
  .dependsOn(zioJson)
  .settings(stdSettings(Some("zio-json-interop-scalaz7x"), turnCompilerWarningIntoErrors = false))
  .settings(buildInfoSettings("zio.json.interop.scalaz7x"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % Version.scalaz,
      "dev.zio"    %%% "zio-test"     % Version.zio % "test",
      "dev.zio"    %%% "zio-test-sbt" % Version.zio % "test"
    )
  )
  .jsSettings(jsSettings)
  .jvmSettings(jvmSettings)
  .nativeSettings(nativeSettings)

lazy val docs = project
  .in(file("zio-json-docs"))
  .settings(macroExpansionSettings)
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
    crossScalaVersions -= scala3.value,
    moduleName     := "zio-json-docs",
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
    readmeAcknowledgement :=
      """|- Uses [JsonTestSuite](https://github.com/nst/JSONTestSuite) to test parsing. (c) 2016 Nicolas Seriot)
         |
         |- Uses [YourKit Java Profiler](https://www.yourkit.com/java/profiler/) for performance optimisation. ![YourKit Logo](https://www.yourkit.com/images/yklogo.png)
         |""".stripMargin
  )
  .enablePlugins(WebsitePlugin)
