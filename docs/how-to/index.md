---
id: index
title: "How-to guides"
---

How-to guides answer one question each: "How do I do X?" Each guide starts with a goal and gives numbered steps. If you are new to ZIO JSON, start with the [Getting Started](../tutorial/getting-started.mdx) tutorial first.

## Encoding and decoding

- [Encoding](encoding.md): turn Scala values into JSON, with derived and manual encoders.
- [Decoding](decoding.md): read JSON into Scala values, and handle the most common decoding cases.
- [Manual instances](manual-instances.md): write encoders and decoders by hand and reuse existing ones.
- [Encode a type you do not own](third-party-types.md): write a codec for a class from a library you cannot change.
- [Validate data while decoding](validate-on-decode.md): reject bad values during decoding.

## Shaping the JSON

- [Field names](field-names.md): rename fields, use snake_case or kebab-case, and add aliases.
- [Sealed traits](sealed-traits.md): choose how a sum type is written and read, with type hints or a discriminator field.
- [Optional, default and null fields](optional-and-default-fields.md): control missing fields, default values and nulls.

## Working with data at scale

- [Streaming](streaming.md): read and write large JSON without loading it all into memory.
- [Golden testing](golden-testing.md): check that your JSON output does not change by accident.

For every annotation and option, see the [Reference](../reference/annotations.md) section.
