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
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-json/"), "scm:git:git@github.com:zio/zio-json.git")
    )
  )
)

addCommandAlias("fix", "scalafixAll")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("prepare", "fix; fmt")
addCommandAlias("testJVM", "zioJsonJVM/test")
addCommandAlias("testJS", "zioJsonJS/test")

val zioVersion = "1.0.4-2"

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    zioJsonJVM,
    zioJsonJS
  )

val circeVersion = "0.13.0"

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
    libraryDependencies ++= Seq(
      "com.propensive"                        %%% "magnolia"                % "0.17.0",
      "org.scalaz"                            %%% "scalaz-core"             % "7.3.3" intransitive (),
      "eu.timepit"                            %%% "refined"                 % "0.9.20" intransitive (),
      "org.scala-lang"                          % "scala-reflect"           % scalaVersion.value % Provided,
      "dev.zio"                               %%% "zio"                     % zioVersion,
      "dev.zio"                               %%% "zio-streams"             % zioVersion,
      "org.scala-lang.modules"                %%% "scala-collection-compat" % "2.3.2",
      "dev.zio"                               %%% "zio-test"                % zioVersion         % "test",
      "dev.zio"                               %%% "zio-test-sbt"            % zioVersion         % "test",
      "io.circe"                              %%% "circe-core"              % circeVersion       % "test",
      "io.circe"                              %%% "circe-generic"           % circeVersion       % "test",
      "io.circe"                              %%% "circe-parser"            % circeVersion       % "test",
      "io.circe"                              %%% "circe-generic-extras"    % circeVersion       % "test",
      "com.typesafe.play"                     %%% "play-json"               % "2.9.2"            % "test",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"     % "2.6.2"            % "test",
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros"   % "2.6.2"            % "test"
    ),
    sourceGenerators in Compile += Def.task {
      val dir  = (sourceManaged in Compile).value
      val file = dir / "zio" / "json" / "GeneratedTupleDecoders.scala"
      val decoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonDecoder[A$p]").mkString(", ")
        val work = (1 to i)
          .map(p => s"val a$p = A$p.unsafeDecode(trace :+ traces($p), in)")
          .mkString("\n        Lexer.char(trace, in, ',')\n        ")
        val returns = (1 to i).map(p => s"a$p").mkString(", ")

        s"""implicit def tuple${i}[$tparams](implicit $implicits): JsonDecoder[Tuple${i}[$tparams]] =
           |    new JsonDecoder[Tuple${i}[$tparams]] {
           |      val traces: Array[JsonError] = (0 to $i).map(JsonError.ArrayAccess(_)).toArray
           |      def unsafeDecode(trace: List[JsonError], in: RetractReader): Tuple${i}[$tparams] = {
           |        Lexer.char(trace, in, '[')
           |        $work
           |        Lexer.char(trace, in, ']')
           |        Tuple${i}($returns)
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
    sourceGenerators in Compile += Def.task {
      val dir  = (sourceManaged in Compile).value
      val file = dir / "zio" / "json" / "GeneratedTupleEncoders.scala"
      val encoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: JsonEncoder[A$p]").mkString(", ")
        val work = (1 to i)
          .map(p => s"A$p.unsafeEncode(t._$p, indent, out)")
          .mkString("\n        if (indent.isEmpty) out.write(',') else out.write(\", \")\n        ")

        s"""implicit def tuple${i}[$tparams](implicit $implicits): JsonEncoder[Tuple${i}[$tparams]] =
           |    new JsonEncoder[Tuple${i}[$tparams]] {
           |      def unsafeEncode(t: Tuple${i}[$tparams], indent: Option[Int], out: internal.Write): Unit = {
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
    inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "ai.x"          %% "play-json-extensions" % "0.42.0" % "test",
      "org.typelevel" %% "jawn-ast"             % "1.1.0"  % "test"
    )
  )

lazy val zioJsonJS = zioJson.js
  .settings(
    scalaJSUseMainModuleInitializer := true,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val zioJsonJVM = zioJson.jvm

lazy val docs = project
  .in(file("zio-json-docs"))
  .dependsOn(zioJsonJVM)
  .settings(
    skip.in(publish) := true,
    moduleName := "zio-json-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio"    %% "zio"     % zioVersion,
      "eu.timepit" %% "refined" % "0.9.20"
    ),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(root),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
