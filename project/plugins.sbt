val zioSbtVersion = "0.0.0+529-d9aba4fa-SNAPSHOT"

addSbtPlugin("pl.project13.scala" % "sbt-jcstress"      % "0.2.0")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"     % "2.0.11")
addSbtPlugin("nl.thijsbroersen"   % "zio-sbt-ci"        % zioSbtVersion)
addSbtPlugin("nl.thijsbroersen"   % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("nl.thijsbroersen"   % "zio-sbt-website"   % zioSbtVersion)

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
