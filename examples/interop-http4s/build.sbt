val Http4sVersion  = "0.21.21"
val ZioJsonVersion = "0.1.3+8-6eb41b5a-SNAPSHOT"

lazy val zioJsonHttp4sExample = (project in file("."))
  .settings(
    name         := "zio-json-http4s-example",
    version      := "1.0",
    scalaVersion := "2.13.6",

    scalacOptions ++= Seq("-Xlint:_"),

    // Only required when using a zio-json snapshot
    resolvers ++= Seq("public", "snapshots", "releases").map(Resolver.sonatypeRepo),

    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server"     % Http4sVersion,
      "org.http4s" %% "http4s-dsl"              % Http4sVersion,
      "dev.zio"    %% "zio-json"                % ZioJsonVersion,
      "dev.zio"    %% "zio-json-interop-http4s" % ZioJsonVersion,
    )
  )
