organization := "dev.zio"
name := "zio-json"

crossScalaVersions := Seq("2.12.11", "2.13.3")
scalaVersion := crossScalaVersions.value.last

scalacOptions ++= Seq(
  "-language:_",
  //"-Xfatal-warnings", // the deprecations cause the compile to fail
  "-deprecation",
  // optimisations slow things down...
  //"-opt:l:inline",
  //"-opt-inline-from:**"
)

scalacOptions in (Compile, console) -= "-Xfatal-warnings"
scalacOptions in (Test, console) -= "-Xfatal-warnings"

libraryDependencies += "com.propensive" %% "magnolia" % "0.16.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")
libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.8.0" % "test"
parallelExecution in Test := false // scalaprops does not support parallel execution

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.5.0" % "test"
libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.5.0" % "test"

// circe is super easy to install (e_e)
val circeVersion = "0.13.0"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion % "test")
libraryDependencies += "io.circe" %% "circe-generic-extras" % "0.13.0" % "test"
libraryDependencies += "org.typelevel" %% "jawn-ast" % "1.0.0" // matches circe

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.0" % "test"
libraryDependencies += "ai.x" %% "play-json-extensions" % "0.42.0" % "test"

// scalafmtOnCompile := true

enablePlugins(NeoJmhPlugin)
inConfig(Jmh)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)

sourceGenerators in Compile += Def.task {
  val dir = (sourceManaged in Compile).value
  val file = dir / "zio" / "json" / "GeneratedTuples.scala"
  val decoders = (2 to 22).map { i =>
    val tparams = (1 to i).map(p => s"A$p").mkString(", ")
    val implicits = (1 to i).map(p => s"A$p: Decoder[A$p]").mkString(", ")
    val work = (1 to i).map { p =>
      s"val a$p = A$p.unsafeDecode(traces($p) :: trace, in)"
    }.mkString("\n        Lexer.char(trace, in, ',')\n        ")
    val returns = (1 to i).map(p => s"a$p").mkString(", ")

    s"""implicit def tuple${i}[$tparams](implicit $implicits): Decoder[($tparams)] =
       |    new Decoder[($tparams)] {
       |      val traces: Array[JsonError] = (0 to $i).map(JsonError.ArrayAccess(_)).toArray
       |      def unsafeDecode(trace: List[JsonError], in: RetractReader): ($tparams) = {
       |        Lexer.char(trace, in, '[')
       |        $work
       |        Lexer.char(trace, in, ']')
       |        ($returns)
       |      }
       |    }""".stripMargin
  }
  IO.write(
    file,
    s"""package zio.json
       |
       |import zio.json.internal._
       |
       |private[json] trait GeneratedTuples { this: Decoder.type =>
       |  ${decoders.mkString("\n\n  ")}
       |}""".stripMargin)
  Seq(file)
}.taskValue
