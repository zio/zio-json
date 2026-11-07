---
id: yaml
title: "YAML"
---

The `zio-json-yaml` module converts between YAML and any type that has a `JsonEncoder` or `JsonDecoder`.
It goes through the [JSON AST](./json-ast.md): values are encoded to a `Json` value and then rendered as YAML, and YAML is parsed into a `Json` value before decoding.
Because of this, the same codec that you use for JSON also works for YAML, and there is no separate YAML codec to write.

The module is available for the JVM only, since it depends on [SnakeYAML](https://bitbucket.org/snakeyaml/snakeyaml).

## Installation

Add the module to your `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-json-yaml" % "@VERSION@"
```

## Encoding to YAML

Importing `zio.json.yaml._` adds the `toYaml` extension method to any value that has a `JsonEncoder` in scope.
We define a case class with a derived codec first:

```scala mdoc:silent
import zio.json._
import zio.json.ast.Json
import zio.json.yaml._

case class Person(name: String, age: Int, email: Option[String])

object Person {
  implicit val codec: JsonCodec[Person] = DeriveJsonCodec.gen[Person]
}
```

Calling `toYaml` returns an `Either[String, String]`. The `email` field is `None`, so it is absent from the output:

```scala mdoc
Person("Alice", 30, None).toYaml().getOrElse("")
```

`toYaml` also accepts a `Json` value directly, which is useful when you already have an AST:

```scala mdoc
Json.Obj("name" -> Json.Str("Alice"), "tags" -> Json.Arr(Json.Str("admin"), Json.Str("ops"))).toYaml().getOrElse("")
```

## Decoding from YAML

The `String#fromYaml` extension method parses YAML and decodes the result with a `JsonDecoder`:

```scala mdoc
val yaml: String =
  """name: Bob
    |age: 41
    |email: bob@example.com""".stripMargin

yaml.fromYaml[Person]
```

If the input is not valid YAML, or the resulting structure does not match the decoder, `String#fromYaml` returns a `Left` with an error message:

```scala mdoc
"name: [unclosed".fromYaml[Person]
```

## Options

`YamlOptions` controls how YAML is rendered. `YamlOptions.default` is used when no options are given, and you can change individual settings with `copy`:

```scala mdoc
val nested: Json = Json.Obj(
  "server" -> Json.Obj("host" -> Json.Str("localhost"), "port" -> Json.Num(8080)),
  "debug"  -> Json.Null
)

nested.toYaml(YamlOptions.default.copy(dropNulls = false, indentation = 4)).getOrElse("")
```

The most commonly used settings are:

- `dropNulls` (default `true`): removes object fields whose value is `Json.Null`. Option fields that are `None` are controlled by the codec, not by this setting.
- `indentation` (default `2`) and `sequenceIndentation` (default `2`): the number of spaces used for nested mappings and sequences.
- `maxScalarWidth` (default `Some(80)`): long string scalars are split across lines. Set it to `None` to keep each scalar on one line.
- `lineBreak`: the line separator. The default follows the platform.
- `flowStyle`, `scalarStyle` and `keyStyle`: functions that choose the YAML presentation style for each value or key. They let you, for example, force flow style (`{a: 1}`) for small objects.
- `nonPrintableStyle`: how strings that contain non-printable characters are written. The default escapes them, and `NonPrintableStyle.BINARY` writes them as base64 `!!binary` values.

If you need an option that is not exposed by `YamlOptions`, use `newDumperOptions` to supply your own SnakeYAML `DumperOptions`.

Strings that contain line breaks are written as literal block scalars (`|`) automatically, even when the `scalarStyle` is `PLAIN`.

## Working with the YAML Node Tree

The `toYamlAST` method returns the SnakeYAML `Node` tree instead of a string. Use it when you need to change the document before it is emitted:

```scala mdoc
val node = Person("Carol", 25, None).toYamlAST()
node.isRight
```

To go from a YAML string to a `Json` value without decoding into a domain type, use `String#fromYaml` with `Json` as the type:

```scala mdoc
"""items:
  |  - 1
  |  - true
  |  - null""".stripMargin.fromYaml[Json]
```
