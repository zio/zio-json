---
id: index
title: "How-to guides"
---

How-to guides answer one question each: "How do I do X?" Each guide starts with a goal and gives numbered steps. If you are new to ZIO JSON, start with the [Getting Started](../tutorial/getting-started.md) tutorial first.

## Encoding and decoding

- [Encoding](encoding.md): turn Scala values into JSON, with derived and manual encoders.
- [Decoding](decoding.md): read JSON into Scala values, and handle the most common decoding cases.
- [Manual instances](manual-instances.md): write encoders and decoders by hand and reuse existing ones.
- [Encode a type you do not own](third-party-types.md): write a codec for a class from a library you cannot change.
- [Use your own type as a Map key](custom-map-keys.md): write `JsonFieldEncoder` and `JsonFieldDecoder` instances for dates, wrapper types and enumerations used as map keys.
- [Write a fast encoder or decoder by hand](low-level-codecs.md): implement `unsafeEncode` and `unsafeDecode` for hot paths or JSON shapes derivation cannot produce.
- [Validate data while decoding](validate-on-decode.md): reject bad values during decoding.

## Shaping the JSON

- [Field names](field-names.md): rename fields, use snake_case or kebab-case, and add aliases.
- [Sealed traits](sealed-traits.md): choose how a sum type is written and read, with type hints or a discriminator field.
- [Optional, default and null fields](optional-and-default-fields.md): control missing fields, default values and nulls.

## Deriving codecs

- [Derive codecs on Scala 3](scala-3-derivation.md): use `derives`, a `given` configuration, enums, union types, generic classes and opaque types.
- [Derive codecs for generic types](generic-types.md): type-parameterised case classes and sealed traits, including recursive ones, on Scala 2 and 3.

## Working with the JSON tree and YAML

- [Read and edit JSON of unknown shape](edit-json-with-cursors.md): navigate, update, delete and merge parts of a `Json` value with cursors.
- [Load a YAML configuration file](load-yaml-config.md): decode YAML into case classes and write case classes back as YAML.

## Working with data at scale

- [Streaming](streaming.md): read and write large JSON without loading it all into memory.
- [Golden testing](golden-testing.md): check that your JSON output does not change by accident.

## Developer tools

- [Generate case classes from sample JSON](generate-case-classes.md): print case class definitions and codecs inferred from a JSON sample.

For every annotation and option, see the [Reference](../reference/annotations.md) section.
