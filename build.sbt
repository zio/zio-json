import BuildHelper.*
import com.typesafe.tools.mima.core.Problem
import com.typesafe.tools.mima.core.ProblemFilters.exclude
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    scalaVersion := Scala213,
    organization := "dev.zio",
    homepage     := Some(url("https://zio.dev/zio-json/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    publishTo := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
      else localStaging.value
    }
  )
)

addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "fmt")

addCommandAlias(
  "testJVM",
  "zioJsonJVM/test; zioJsonYaml/test; zioJsonInteropHttp4s/test; zioJsonGolden/test; zioJsonInteropRefinedJVM/test"
)

addCommandAlias(
  "testJS",
  "zioJsonJS/test; zioJsonInteropRefinedJS/test"
)

addCommandAlias(
  "testNative",
  "zioJsonNative/test; zioJsonInteropRefinedNative/test"
)

addCommandAlias(
  "testScala2JVM",
  "zioJsonMacrosJVM/test; zioJsonInteropScalaz7xJVM/test"
)

addCommandAlias(
  "testScala2JS",
  "zioJsonMacrosJS/test; zioJsonInteropScalaz7xJS/test"
)

addCommandAlias(
  "testScala2Native",
  "zioJsonMacrosNative/test; zioJsonInteropScalaz7xNative/test"
)

val zioVersion = "2.1.24"

lazy val zioJsonRoot = project
  .in(file("."))
  .settings(
    publish / skip        := true,
    mimaPreviousArtifacts := Set(),
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library"),
    crossScalaVersions := Nil // https://www.scala-sbt.org/1.x/docs/Cross-Build.html#Cross+building+a+project+statefully,
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

val circeVersion = "0.14.15"

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
      if (scalaVersion.value == Scala3)
        Vector("-Yretain-trees", "-Xmax-inlines:128")
      else
        Vector.empty
    },
    libraryDependencies ++= Seq(
      "dev.zio"                               %%% "zio"                     % zioVersion,
      "dev.zio"                               %%% "zio-streams"             % zioVersion,
      "org.scala-lang.modules"                %%% "scala-collection-compat" % "2.14.0"     % Test,
      "dev.zio"                               %%% "zio-test"                % zioVersion   % Test,
      "dev.zio"                               %%% "zio-test-sbt"            % zioVersion   % Test,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"     % "2.38.8"     % Test,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros"   % "2.38.8"     % Test,
      "io.circe"                              %%% "circe-core"              % circeVersion % Test,
      "io.circe"                              %%% "circe-generic"           % circeVersion % Test,
      "io.circe"                              %%% "circe-parser"            % circeVersion % Test,
      "org.typelevel"                         %%% "jawn-ast"                % "1.6.0"      % Test
    ),
    // scala version specific dependencies
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq(
            "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18"
          )
        case _ =>
          Seq(
            "org.scala-lang"                 % "scala-reflect" % scalaVersion.value % Provided,
            "com.softwaremill.magnolia1_2" %%% "magnolia"      % "1.1.10"
          )
      }
    },
    Compile / sourceGenerators += Def.task {
      val dir      = (Compile / sourceManaged).value
      val file     = dir / "zio" / "json" / "GeneratedTupleDecoders.scala"
      val decoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonDecoder[A$p]").mkString(", ")
        val work      = (1 to i)
          .map(p => s"val a$p = A$p.unsafeDecode(traces(${p - 1}) :: trace, in)")
          .mkString("\n        Lexer.char(trace, in, ',')\n        ")
        val work2 = (1 to i)
          .map(p => s"val a$p = A$p.unsafeFromJsonAST(traces(${p - 1}) :: trace, arr($p - 1))")
          .mkString("\n            ")
        val returns = (1 to i).map(p => s"a$p").mkString(", ")
        s"""implicit def tuple$i[$tparams](implicit $implicits): JsonDecoder[Tuple$i[$tparams]] =
           |    new JsonDecoder.AbstractJsonDecoder[Tuple$i[$tparams]] {
           |      private[this] val traces: Array[JsonError] = (0 to ${i - 1}).map(JsonError.ArrayAccess(_)).toArray
           |      def unsafeDecode(trace: List[JsonError], in: RetractReader): Tuple$i[$tparams] = {
           |        Lexer.char(trace, in, '[')
           |        $work
           |        Lexer.char(trace, in, ']')
           |        new Tuple$i($returns)
           |      }
           |      override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Tuple$i[$tparams] = {
           |        json match {
           |          case Json.Arr(arr) if arr.length == $i =>
           |            $work2
           |            new Tuple$i($returns)
           |          case _ => Lexer.error("Expected array of size $i", trace)
           |        }
           |      }
           |    }""".stripMargin
      }
      IO.write(
        file,
        s"""package zio.json
           |
           |import zio.json.internal._
           |import zio.json.ast._
           |
           |private[json] trait GeneratedTupleDecoders { this: JsonDecoder.type =>
           |  ${decoders.mkString("\n\n  ")}
           |}""".stripMargin
      )
      Seq(file)
    }.taskValue,
    Compile / sourceGenerators += Def.task {
      val dir      = (Compile / sourceManaged).value
      val file     = dir / "zio" / "json" / "GeneratedTupleEncoders.scala"
      val encoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonEncoder[A$p]").mkString(", ")
        val work      = (1 to i)
          .map(p => s"A$p.unsafeEncode(t._$p, indent, out)")
          .mkString("\n        if (indent.isEmpty) out.write(',') else out.write(\", \")\n        ")
        s"""implicit def tuple$i[$tparams](implicit $implicits): JsonEncoder[Tuple$i[$tparams]] =
           |    new JsonEncoder.AbstractJsonEncoder[Tuple$i[$tparams]] {
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
      val dir    = (Compile / sourceManaged).value
      val file   = dir / "zio" / "json" / "GeneratedTupleCodecs.scala"
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
    inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings(Jmh)),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    mimaBinaryIssueFilters ++= Seq(
      exclude[Problem]("zio.json.CaseObjectDecoder.*") // FIXME: false negative reported by mima
    )
  )
  .jsSettings(
    mimaBinaryIssueFilters ++= Seq(
      exclude[Problem]("zio.JsonPackagePlatformSpecific.*"),
      exclude[Problem]("zio.json.JsonDecoderPlatformSpecific.*"),
      exclude[Problem]("zio.json.JsonEncoderPlatformSpecific.*"),
      exclude[Problem]("zio.json.package.*")
    ),
    libraryDependencies ++= Seq(
      ("org.scala-js"     %%% "scalajs-weakreferences" % "1.0.0").cross(CrossVersion.for3Use2_13),
      "io.github.cquiroz" %%% "scala-java-time"        % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb"   % scalaJavaTimeVersion % Test
    )
  )
  .nativeSettings(nativeSettings)
  .nativeSettings(
    mimaBinaryIssueFilters ++= Seq(
      exclude[Problem]("zio.JsonPackagePlatformSpecific.*"),
      exclude[Problem]("zio.json.JsonDecoderPlatformSpecific.*"),
      exclude[Problem]("zio.json.JsonEncoderPlatformSpecific.*"),
      exclude[Problem]("zio.json.package.*")
    ),
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time"      % scalaJavaTimeVersion,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion % Test
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
      "org.yaml"                % "snakeyaml"               % "2.5",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.14.0",
      "dev.zio"                %% "zio"                     % zioVersion,
      "dev.zio"                %% "zio-test"                % zioVersion % Test,
      "dev.zio"                %% "zio-test-sbt"            % zioVersion % Test
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
    crossScalaVersions -= Scala3,
    scalacOptions -= "-Xfatal-warnings", // not quite ready.
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      "dev.zio"      %%% "zio-test"      % zioVersion         % Test,
      "dev.zio"      %%% "zio-test-sbt"  % zioVersion         % Test
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
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-dsl"       % "0.23.33",
      "dev.zio"       %% "zio"              % zioVersion,
      "org.typelevel" %% "cats-effect"      % "3.6.3",
      "dev.zio"       %% "zio-interop-cats" % "23.1.0.13" % Test,
      "dev.zio"       %% "zio-test"         % zioVersion  % Test,
      "dev.zio"       %% "zio-test-sbt"     % zioVersion  % Test
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
      "dev.zio"    %%% "zio-test"     % zioVersion % Test,
      "dev.zio"    %%% "zio-test-sbt" % zioVersion % Test
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
    crossScalaVersions -= Scala3,
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core"  % "7.3.8",
      "dev.zio"    %%% "zio-test"     % zioVersion % Test,
      "dev.zio"    %%% "zio-test-sbt" % zioVersion % Test
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
    crossScalaVersions -= Scala3,
    moduleName := "zio-json-docs",
    scalacOptions += "-Ymacro-annotations",
    projectName                                := "ZIO JSON",
    mainModuleName                             := (zioJsonJVM / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
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
