---
id: load-yaml-config
title: "Load a YAML Configuration File"
description: "Read a YAML configuration file into case classes with zio-json-yaml, handle errors and defaults, write it back, and know the limits."
keywords:
  - "YAML"
  - "Configuration"
  - "Config File"
  - "fromYaml"
  - "YamlOptions"
---

In this guide, we load an application's configuration from a YAML file into Scala case classes with the `zio-json-yaml` module. By the end, you will have a `loadConfig` function that reads a file, applies default values for keys the file leaves out and reports clear errors for bad input. You will also know how to write a configuration back as YAML.

We cover these tasks in order:

- Add the dependency and define the configuration type with a derived codec.
- Read YAML from a string and from a file.
- Handle errors, default values and optional keys.
- Write a configuration back as YAML with `YamlOptions`.
- Know the limits of the YAML support.

## The Problem

A service reads its settings, such as the HTTP port, the database URL and a list of feature flags, from a YAML file at startup. Parsing YAML with a plain YAML library gives you untyped nested maps, and every value needs a cast:

```scala mdoc:compile-only
import org.yaml.snakeyaml.Yaml

val raw =
  """server:
    |  port: 8080
    |""".stripMargin

val root   = new Yaml().load[java.util.Map[String, Object]](raw)
val server = root.get("server").asInstanceOf[java.util.Map[String, Object]]
val port   = server.get("port").asInstanceOf[Int]
```

This code throws a `NullPointerException` when `server` is missing, and a `ClassCastException` when `port` is written as text. Default values and optional keys need more hand-written checks, and every new setting needs more casts.

With `zio-json-yaml`, the case classes describe the configuration once. The same `JsonDecoder` that reads JSON reads the YAML, fills in defaults, and returns an error that names the bad key.

## Prerequisites

Add the module to `build.sbt`. It depends on `zio-json` and on [SnakeYAML](https://bitbucket.org/snakeyaml/snakeyaml), and it is available on the JVM only, not on Scala.js or Scala Native:

```scala
libraryDependencies += "dev.zio" %% "zio-json-yaml" % "@VERSION@"
```

All examples use these imports. `zio.json.yaml._` adds the `String#fromYaml` extension method, and a `toYaml` method to every value that has a `JsonEncoder`:

```scala mdoc:silent
import zio.json._
import zio.json.yaml._
```

This guide assumes you know how to derive a codec for a case class, as shown in the [Getting Started](../getting-started.md) tutorial. The [YAML](../reference/yaml.md) reference page documents every option of the module.

## Define the Configuration Type

The configuration has a server section, a database section and a list of enabled features. Default values in the case classes become the values for keys that the file leaves out, and an `Option` field is a key that may be absent:

```scala mdoc:silent
final case class ServerConfig(host: String, port: Int)

final case class DatabaseConfig(url: String, user: Option[String], poolSize: Int = 10)

final case class AppConfig(
  server: ServerConfig,
  database: DatabaseConfig,
  features: List[String] = Nil,
  logLevel: String = "info"
)

implicit val serverConfigCodec: JsonCodec[ServerConfig]     = DeriveJsonCodec.gen[ServerConfig]
implicit val databaseConfigCodec: JsonCodec[DatabaseConfig] = DeriveJsonCodec.gen[DatabaseConfig]
implicit val appConfigCodec: JsonCodec[AppConfig]           = DeriveJsonCodec.gen[AppConfig]
```

A `JsonCodec` holds both a `JsonDecoder`, which `String#fromYaml` needs, and a `JsonEncoder`, which `toYaml` needs. If you only read the configuration, `DeriveJsonDecoder.gen` is enough. In a project, put each codec in the companion object of its case class, so the compiler finds it without an import.

## Read YAML from a String

A typical configuration file sets the server address, the database connection and the enabled features:

```scala mdoc:silent
val yaml: String =
  """server:
    |  host: 0.0.0.0
    |  port: 8080
    |database:
    |  url: jdbc:postgresql://localhost:5432/shop
    |  user: shop
    |features:
    |  - search
    |  - export
    |""".stripMargin
```

`String#fromYaml[A]` parses the text as YAML, turns it into a [`Json`](../reference/json-ast.md) value, and decodes that with the `JsonDecoder[A]`. It returns `Either[String, A]`:

```scala mdoc
yaml.fromYaml[AppConfig]
```

The file leaves out `database.poolSize` and `logLevel`, so the decoder uses their default values `10` and `"info"`.

## Use Defaults and Optional Keys

A key that the file leaves out is decoded as its default value when the field has one, and as `None` when the field is an `Option`. A minimal file only needs the keys without defaults. YAML flow style (`{ ... }`) works too:

```scala mdoc
"""server: { host: localhost, port: 80 }
  |database: { url: "jdbc:h2:mem:test" }
  |""".stripMargin.fromYaml[AppConfig]
```

Keys that the case classes do not declare are ignored. A misspelled optional key, such as `poolsize` instead of `poolSize`, is therefore not an error: the field keeps its default value. To reject unknown keys, annotate the case classes with `@jsonNoExtraFields`, as described in [Optional, default and null fields](optional-and-default-fields.md).

## Handle Errors

A configuration error should stop the service at startup with a message that says what to fix. The `Left` of `String#fromYaml` carries that message. A required key that is missing is reported with its path:

```scala mdoc
"""server:
  |  host: localhost
  |database:
  |  url: jdbc:h2:mem:test
  |""".stripMargin.fromYaml[AppConfig]
```

A value of the wrong type is reported the same way:

```scala mdoc
"""server:
  |  host: localhost
  |  port: eighty
  |database:
  |  url: jdbc:h2:mem:test
  |""".stripMargin.fromYaml[AppConfig]
```

Invalid YAML returns SnakeYAML's message, which spans several lines and points at the line and column of the problem:

```scala mdoc
"server: [localhost".fromYaml[AppConfig]
```

The paths in these messages use the same format as JSON decoding errors, described in [Errors](../reference/errors.md).

:::tip
SnakeYAML reads unquoted `yes`, `no`, `on`, `off`, `true` and `false` as booleans, so `host: on` fails with `.server.host(expected string)`. Quote such values (`host: "on"`) when the field is a `String`.
:::

## Read the Configuration from a File

`String#fromYaml` takes the text, not a path. Read the file first and turn a read failure into a `Left`, so both kinds of error reach the caller in the same way:

```scala mdoc:silent
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import scala.util.Try

def loadConfig(path: Path): Either[String, AppConfig] =
  Try(new String(Files.readAllBytes(path), StandardCharsets.UTF_8)).toEither.left
    .map(error => s"Cannot read ${path.getFileName}: ${error.getClass.getSimpleName}")
    .flatMap(_.fromYaml[AppConfig])
```

To try `loadConfig`, we write the YAML from earlier into a temporary file, load it, and delete the file and its directory afterwards:

```scala mdoc
{
  val dir  = Files.createTempDirectory("config-demo")
  val file = dir.resolve("app.yaml")

  try {
    Files.write(file, yaml.getBytes(StandardCharsets.UTF_8))
    println(loadConfig(file))
    println(loadConfig(dir.resolve("missing.yaml")))
  } finally {
    Files.deleteIfExists(file)
    Files.deleteIfExists(dir)
  }
}
```

At startup, a service typically turns the `Left` into a failure, for example with `loadConfig(path).fold(error => sys.error(s"Invalid configuration: $error"), identity)`, or with `ZIO.fromEither` in a ZIO application.

## Write the Configuration as YAML

`toYaml` encodes a value with its `JsonEncoder` and renders the result as YAML. It returns `Either[String, String]`. This is useful to print the effective configuration at startup or to generate a template file. We start from the configuration loaded earlier:

```scala mdoc:silent
val config: AppConfig = yaml.fromYaml[AppConfig].fold(sys.error, identity)
```

Default values are written out, and fields that are `None` are left out:

```scala mdoc
println(config.toYaml().getOrElse(""))
```

`YamlOptions` controls the layout. To indent with four spaces and put the `-` of list items in the same column as their key:

```scala mdoc
println(config.toYaml(YamlOptions.default.copy(indentation = 4, sequenceIndentation = 0)).getOrElse(""))
```

The other settings, such as `maxScalarWidth`, `dropNulls` and the style functions, are described in [Options](../reference/yaml.md#options).

## Know the Limits

The module covers configuration files well, but a few limits apply. `String#fromYaml` parses with SnakeYAML's default settings, and there is no parameter to change them:

- A file must hold one YAML document. A second document after `---` fails with `expected a single document in the stream`.
- Nesting deeper than 50 levels fails with `Nesting Depth exceeded max 50`.
- Input larger than 3,145,728 code points (3 MB) fails with `The incoming YAML document exceeds the limit: 3145728 code points.`
- More than 50 aliases (`*name`) to mappings or sequences fail.

The exact limits and their messages are listed in [Decoding Limits](../reference/yaml.md#decoding-limits). For example, a stream with two documents:

```scala mdoc
"""server: { host: a, port: 1 }
  |database: { url: x }
  |---
  |server: { host: b, port: 2 }
  |database: { url: y }
  |""".stripMargin.fromYaml[AppConfig].isLeft
```

An empty file, or a file that holds only comments, does not produce a helpful message. The parser finds no document, and the conversion to `Json` fails with a `NullPointerException`, whose message `String#fromYaml` returns:

```scala mdoc
println("".fromYaml[AppConfig])

println("# all settings commented out\n".fromYaml[AppConfig])
```

The text comes from the JVM, so it can differ between JDK versions. To give a better message, check for an empty file before decoding:

```scala mdoc:silent
def parseConfig(raw: String): Either[String, AppConfig] =
  if (raw.linesIterator.forall(line => line.trim.isEmpty || line.trim.startsWith("#")))
    Left("The configuration file is empty")
  else raw.fromYaml[AppConfig]
```

An empty file now gets a message that says what is wrong:

```scala mdoc
parseConfig("")
```

When writing YAML, `toYaml` does not write integers outside the `Long` range correctly. Take a setting that holds a large integer:

```scala mdoc:silent
final case class Limits(maxUploadBytes: BigInt)

implicit val limitsCodec: JsonCodec[Limits] = DeriveJsonCodec.gen[Limits]
```

`toYaml` keeps only the low 64 bits of the number, and reading the YAML back gives the wrong value without an error:

```scala mdoc
val limits = Limits(BigInt("123456789012345678901234567890"))

limits.toYaml()

limits.toYaml().flatMap(_.fromYaml[Limits])
```

Store such numbers as strings in the case class if they must go through YAML. Numbers with a fractional part are not affected; see [Number Rendering](../reference/yaml.md#number-rendering).

## Going Further

- The [YAML](../reference/yaml.md) reference covers every `YamlOptions` setting, how YAML scalars map to `Json` values, and the SnakeYAML node tree returned by `toYamlAST`.
- To change a configuration document without a case class, for example to apply overrides before decoding, parse it with `fromYaml[Json]` and edit it as shown in [Read and Edit JSON of Unknown Shape](edit-json-with-cursors.md).
- For fields that may be missing, `null` or empty, see [Optional, default and null fields](optional-and-default-fields.md).
