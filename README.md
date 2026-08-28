libraryDependencies += "dev.zio" %% "zio-json" % "0.10.0"
```

For cross-platform projects with Scala.js and Scala Native need to replace `%%` operator by `%%%`, 
and optionally when using `java.time.ZoneId` and `java.time.ZonedDateTime` types need to add 
the dependency on the latest version of Timezone DB:

```scala
libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "latest.integration"
```

Continue with the [Getting Started](docs/getting-started.mdx) guide for a walkthrough of encoding and decoding JSON with ZIO JSON.
