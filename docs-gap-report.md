# ZIO JSON documentation gap report

Audit of `docs/` against the source on `series/2.x` at `d59b926ea40` (2026-11-17). This file is an internal tracking document. It lives at the repository root so the website does not publish it.

## How the audit was done

1. The `docs-find-documentation-gaps` scanner listed 121 public types and matched 40 of them by name in `docs/` (33%). The scanner only matches type names, so it misses methods, behaviour and wrong statements. Most of its 81 "undocumented" types are internal (`zio.json.internal.*`, number option types, platform traits).
2. Four read-only audits then compared every page with the source, one slice each:
   - encoding, codecs and streaming ([Appendix A](#appendix-a-encoding-codecs-and-streaming))
   - decoding and errors ([Appendix B](#appendix-b-decoding-and-errors))
   - derivation, annotations, configuration, Scala 2 vs 3, `zio-json-macros` ([Appendix C](#appendix-c-derivation-and-configuration))
   - JSON AST, cursors, codegen, YAML, golden testing, interop modules ([Appendix D](#appendix-d-ast-and-modules))
3. The runtime claims were checked by running small programs with scala-cli against the released `zio-json` 1.1.2 (Scala 2.13.18 and 3.9.0). No library source has changed between `v1.1.2` and `d59b926ea40`. The results are in [Runtime checks](#runtime-checks). Where an appendix claim turned out to be wrong, the appendix says so.
4. Baseline `sbt docs/mdoc`: 0 errors, 32 warnings. 30 are "The outer reference in this type test cannot be checked at run time" (mdoc wrapper noise). 2 are broken links: `index.md:39` and `how-to/index.md:6` link to `tutorial/getting-started.mdx`, which mdoc cannot resolve.

## Headline numbers

| Kind | Count |
|---|---|
| Wrong or outdated statements in existing pages | 66 (12 encoding, 21 decoding, 14 derivation, 19 AST and modules) |
| Public API with no documentation | 74 rows (4 Critical, 11 High, 28 Medium, 31 Low) |
| Partially documented API | 38 |
| Missing examples or cross-links | 43 |
| Conceptual gaps (missing guides, explanations, tutorials) | 30, merged into 14 work items below |
| Library bugs confirmed at runtime | 10 (reported to the maintainers' side, not fixed here) |

What is already in good shape: every annotation, all six `JsonMemberFormat` cases, all seven `JsonCodecConfiguration` fields and their Scala 2 and Scala 3 defaults, the main `JsonEncoder`/`JsonDecoder`/`JsonCodec` combinators, streaming basics, golden testing and the YAML entry points.

## 1. Fixes to existing pages

Each item has page:line, the source that proves it, and the exact replacement text in the appendix named in brackets.

### Examples that are wrong or do not compile

- [ ] `interop/enumeratum.md:54, 103, 106, 109`: plain `scala` blocks that do not compile. `ShirtSize.Small.toJson` finds no encoder because `JsonEncoder` is invariant; `3.fromJson[...]` calls `fromJson` on an `Int`. Convert to mdoc so they are compiled. [D]
- [ ] `how-to/decoding.md:121-125`: `"Apple".fromJson[...]` is not valid JSON and fails; the text says "Pear", the code says "Peer". [B, C]
- [ ] `how-to/decoding.md:160`: the `mdoc:fail` block fails because `Quote` has no decoder, not for the reason the text gives. `:164`: approach 3 keeps the JSON quotes because `Json#toString` renders JSON. [B]
- [ ] `tutorial/getting-started.mdx:125`: says subtypes of a sealed trait need their own codecs. They are derived automatically on Scala 2 and Scala 3. [C]
- [ ] `how-to/streaming.md:140`: suggests `catchAll` to skip malformed lines; a failed stream cannot resume. [A]

### Wrong facts

- [ ] `how-to/optional-and-default-fields.md:32`, `how-to/decoding.md:80`: Scala 3 no longer needs `-Yretain-trees` for default values (checked at runtime on 3.9.0). [B, C]
- [ ] `reference/json-codec-configuration.md:196`: `@jsonExplicitEmptyCollections` on a field affects encoding only. [C]
- [ ] `reference/json-codec.md:173`, `reference/json-decoder.md:278`, `reference/json-encoder.md:215`: `derives` does not need a given `JsonCodecConfiguration`; the companion has a default. [C]
- [ ] `reference/json-codec.md:36`: there is no `JsonCodec` for `IndexedSeq`, `LinearSeq`, `ListSet`, `TreeSet`, `mutable.Map`. [A]
- [ ] `reference/json-ast.md:114`: `relocate` needs the target to exist already, otherwise `Left("No such field: ...")` (checked at runtime). Also `merge` on arrays, `intersect`, the `as*` accessors. [D]
- [ ] `reference/yaml.md:84-85, 91, 93`: `sequenceIndentation`, `indentWithIndicator`, `newDumperOptions` and the switch to `|` are described wrongly. `toYaml` returns `Either[YAMLException, String]`. [D]
- [ ] `reference/errors.md:41, 63, 77, 82` and about 12 missing messages: discriminator case renders as `(Name)`; pipelines stop at the first bad element; `Map` decoders keep the last duplicate; the Scala 3 message differs. [B]
- [ ] `reference/json-decoder.md:42`: decoding can throw when a user function inside `map` throws. `:252`: collection decoders return empty for a missing field. [B]
- [ ] `explanation/performance.md:6-8`: the JMH sources compare with Circe and jsoniter-scala, not Play; there is no "Loom" mode. [A]
- [ ] `explanation/security.md:97`: overstates the 256-bit cap; `Map[String, V]` hash collisions are not covered. [B]
- [ ] `how-to/encoding.md:62`: "new decoders" should be "new encoders". [A]

### Links and wiring

- [ ] `reference/annotations.md:16-25`: in all 10 how-to links the `#anchor` sits outside the link, so it renders as text. [C]
- [ ] `index.md:39`, `how-to/index.md:6`: link to `tutorial/getting-started.mdx` breaks the mdoc link check. Use the doc id.
- [ ] `interop/http4s.md:38`: example link points to `tree/develop`, which does not exist; the example project uses http4s 0.21 and cats-effect 2, while the module builds against http4s 0.23 and cats-effect 3. [D]
- [ ] Platform and Scala version notes missing: `yaml`, `golden`, `http4s` are JVM only; `scalaz7x` and `zio-json-macros` have no Scala 3 build; `refined`, `scalaz7x`, `enumeratum` also build for Scala.js and Native but pages show only `%%`; enumeratum on Scala 3 needs `-Yretain-trees` for `findValues`. [D]

## 2. Reference: missing or thin

| Item | Scope | Priority | Source |
|---|---|---|---|
| Map keys: `JsonFieldEncoder` and `JsonFieldDecoder` (trait, `contramap`/`map`/`mapOrFail`, built-ins are only `String`, `Int`, `Long`, `UUID`, plus Scala 3 `T <: String`; `None` values dropped from maps) | new page | Critical | `JsonFieldEncoder.scala`, `scala-2.x`/`scala-3` `JsonFieldDecoder.scala` |
| Built-in instances: one table of every type with an encoder/decoder/codec, the JSON shape and leniencies (about 30 encoder instances are listed nowhere, including 15 of 16 `java.time` types, collections, tuples, `Either` wire format, `Char`, `Symbol`, `Currency`, `ArraySeq`, `IArray`) | new page | High | `JsonEncoder.scala`, `JsonDecoder.scala`, `javatime/` |
| Low-level instances: `unsafeEncode` + `Write`, `unsafeDecode` + `Lexer` + `RetractReader`, `AbstractJsonEncoder`/`AbstractJsonDecoder`, `unsafeFromJsonAST`/`toJsonAST` overrides, `bump`/`pad` | new sections in `json-encoder.md` and `json-decoder.md` | Critical | `JsonEncoder.scala:93`, `internal/writers.scala:21`, `JsonDecoder.scala:241-268`, `internal/lexer.scala` |
| JSON AST: `Json.Obj` field operations (`add` deep-merges), equality rules (field order ignored, `1 != 1.0`), per-case codecs for `Json` fields, `JsonCursor` instance methods, `JsonType`, `mapXxx` table, error strings | sections in `json-ast.md` | High | `ast/ast.scala`, `ast/JsonCursor.scala`, `ast/JsonType.scala` |
| `zio-json-macros` (`@jsonDerive`): move out of the annotations page into its own page with install, generated member names, generic classes, Scala 2 only | new page | Medium | `zio-json-macros/.../jsonDerive.scala` |
| Modules and platforms: one matrix of artifacts × JVM/JS/Native × Scala 2.12/2.13/3 | new section on `index.md` | Medium | `build.sbt`, `project/BuildHelper.scala` |
| http4s: `jsonOf`, `ZIOEntityCodec`/`ZIOEntityEncoder`/`ZIOEntityDecoder` | rewrite page | High | `zio-json-interop-http4s/` |
| refined: `encodeRefined`, key instances. scalaz: `ilistJsonDecoder`. enumeratum: the 9 string-enum builders, value-enum `ZioJson` helpers | enrich pages | Medium | interop modules |
| YAML decoding limits inherited from SnakeYAML defaults (single document, depth 50, 3 MB); number rendering | section in `yaml.md` | Medium | `zio-json-yaml/.../package.scala` |
| Golden file format (`GoldenSample`) | section in `golden-testing.md` | Low | `zio-json-golden/.../GoldenSample.scala` |

Out of scope by design: `zio.json.internal.*` number option types, platform-specific traits, `DeriveJsonEncoder.NestedWriter` (leaks on Scala 3, should be private), `zio.json.codegen` (see open question below).

## 3. How-to guides to add

- [ ] Use your own type as a `Map` key.
- [ ] Write a fast encoder or decoder by hand (`unsafeEncode`/`unsafeDecode`, with the `Write.write(cs, from, to)` end-index pitfall and the "no compatibility guarantee for `zio.json.internal`" warning).
- [ ] Read and edit JSON of unknown shape with `Json` and cursors (get nested values, update, delete, rename, merge defaults).
- [ ] Derive codecs on Scala 3: `derives`, enums, string-literal unions, generic ADTs, nested sealed traits.
- [ ] Derive codecs for generic (type-parameterised) case classes and ADTs (Scala 2 and 3).
- [ ] Load a YAML config file into a case class.

## 4. Tutorials and explanations to add

- [ ] Second tutorial: model a realistic API payload end to end (sealed traits with a discriminator, renamed fields, defaults, map keys, round-trip check, golden test).
- [ ] Explanation: Scala 2 vs Scala 3 differences and migration (the `enumValuesAsStrings` default flips; empty case classes count as enum values on Scala 2 but not on Scala 3; `@jsonDerive` is Scala 2 only; Magnolia replaced by native macros; `-Yretain-trees` no longer needed).
- [ ] Explanation: how strict the decoder is (trailing input after the top-level value is ignored, so `"1e3".fromJson[Int]` is `Right(1)`; unknown fields are skipped without validation; quoted numbers, leading zeros and quoted `NaN` are accepted) and how to get stricter checks.
- [ ] Explanation: wire format overview (case class, case object, empty case class, Scala 3 enum × wrapper / discriminator / `enumValuesAsStrings`) and round-trip pitfalls (`Option` field with a `Some` default cannot round-trip `None`; `encoding = false` with `decoding = true`; discriminator with `allowExtraFields = false`).
- [ ] Security page: nesting depth, huge strings, `Map` hash collisions, and the `Chunk[Byte]` decoding path.

## Pending upstream (not documented here)

These are open PRs on `series/2.x`. Their features are not merged, so this work does not document them.

| PR | What it adds | Doc impact once merged |
|---|---|---|
| #1740 | named tuple support | Scala 3 derivation guide, built-in instances |
| #1656 (draft) | `toJsonBytes` / `toJsonBytesArray` | `json-encoder.md`, encoding how-to |
| #1694 | `Json.Obj.equals` with duplicate keys (fixes #1600) | equality rules in `json-ast.md` |
| #1648 | `Left` for a negative array index in `delete` and `transformAt` | cursor error strings in `json-ast.md` |

## Runtime checks

Run with scala-cli against `dev.zio::zio-json:1.1.2` (and `zio-json-yaml:1.1.2`).

| Claim | Result |
|---|---|
| Scala 3 default values decode without `-Yretain-trees` | Confirmed: `{"a":1}` → `Right(D(1,7))` on 3.9.0 |
| `"1.5".fromJson[Int]` | `Right(1)`. Also `"1e3".fromJson[Int]` → `Right(1)`, `"12x".fromJson[Int]` → `Right(12)`, `"truex".fromJson[Boolean]` → `Right(true)`. Inside objects and arrays the same inputs fail correctly. Related to open issue #55 |
| `Double.NaN.toJsonAST` | Throws `NumberFormatException` (`toJson` gives `"NaN"`) |
| `Json.Obj("a" -> Json.Num(1)).delete(JsonCursor.field("a").isString)` | `Right({})`: the type filter is ignored |
| `Json.Num(BigInt("123456789012345678901234567890")).toYaml()` | `-4362896299872285998`: wrong number |
| `"".fromYaml[Json]` | `Left(Cannot invoke "...Node.getClass()" because "node" is null)` |
| `Generator.printCaseClasses("""{"a": []}""")` | Throws `UnsupportedOperationException: empty.reduceLeft` |
| Unknown field `"x":tzzz` in an object | Accepted: `Right(Name(Ada))` |
| `A(None)` where `case class A(o: Option[Int] = Some(1))`, round-trip | `Right(A(Some(1)))` |
| `enumValuesAsStrings = true` with `case class X()` subtypes | Scala 2: `"X"`; Scala 3: `{"X":{}}` |
| Two `@jsonAliases` on one field, Scala 3 | Works on both versions. The claim in Appendix C that Scala 3 reads only the first one is wrong |
| `decodeJsonPipeline` on `[{"a":1},{"a":` | `Right(Chunk(Map(a -> 1)))`: truncated element silently dropped. Same for newline-delimited input |
| `writeJsonLinesAs` with an emoji across the 4096-char boundary | File contains `??` instead of the emoji |
| `"1e2000000000".fromJson[Json].flatMap(_.as[BigInt])` | Fast `Left((expected a 256-bit BigInt))`. The DoS claim in Appendix B is wrong |
| `{"+5":1}` as `Map[Int, Int]` | `Right(Map(5 -> 1))` while `"+5".fromJson[Int]` fails |
| `relocate` to a missing target field | `Left(No such field: 'b')` |

## Open questions for the maintainers

- `zio.json.codegen.Generator.printCaseClasses` is public, untested on Scala 3, and crashes on empty or mixed arrays. Document it as a developer utility, or mark it internal?
- Scala 3 code in the docs cannot be compiled by mdoc, because the `docs` project is Scala 2.13 only (`build.sbt`, `crossScalaVersions -= Scala3`).

---

## Appendix A: encoding, codecs and streaming


Scope: `JsonEncoder`, `JsonFieldEncoder`, `JsonCodec`, `JsonStreamDelimiter`, package syntax, platform-specific streaming, `internal.Write`, `javatime.serializers`.
Pages: `docs/reference/json-encoder.md`, `docs/reference/json-codec.md`, `docs/how-to/encoding.md`, `docs/how-to/streaming.md`, `docs/how-to/manual-instances.md`, `docs/how-to/third-party-types.md`, `docs/explanation/performance.md`.
All source paths are relative to `zio-json/` unless they start with `build.sbt` or `docs/`. `S/` = `zio-json/shared/src/main/scala/zio/json/`.

Every finding below was checked against the source on this branch. Nothing was compiled or run.

---

### 0. API inventory (Task 1)

#### JsonEncoder trait (`S/JsonEncoder.scala`)

| Member | Line | Status |
|---|---|---|
| `contramap[B](f: B => A)` | 34 | Documented: json-encoder.md "contramap", encoding.md, manual-instances.md, third-party-types.md |
| `either[B](that)` | 49 | Documented: json-encoder.md "either, orElseEither and eitherWith" |
| `orElseEither[B](that)` | 56 | Documented (same section) |
| `eitherWith[B, C](that)(f)` | 63 | Documented (same section) |
| `encodeJson(a, indent = None): CharSequence` | 69 | Documented: json-encoder.md "encodeJson" |
| `isNothing(a)` | 81 | Documented: json-encoder.md "Missing and Empty Values" |
| `isEmpty(a)` | 86 | Documented (same section) |
| `narrow[B <: A]` | 91 | Documented: json-encoder.md "narrow" |
| **`unsafeEncode(a, indent, out: Write)`** (the only abstract method) | 93 | **NOT documented anywhere** (`grep unsafeEncode docs/` = 0 hits) |
| `toJsonAST(a)` (overridable trait method) | 101 | Partial: json-encoder.md:76-77 says it can be overridden, but gives no signature or example |
| `zip[B](that)` | 107 | Documented: json-encoder.md "zip and zipWith" |
| `zipWith[B, C](that)(f)` | 113 | Documented (same section) |

#### JsonEncoder companion (`S/JsonEncoder.scala`)

| Member | Line | Status |
|---|---|---|
| `AbstractJsonEncoder[A]` | 119 | Not documented |
| `apply[A]` | 150 | Documented: json-encoder.md "Instances and Summoning" |
| `suspend[A](encoder0)` | 219 | Documented: json-encoder.md "Recursive Encoders" |
| `bump(indent)` (public pretty-print helper) | 313 | Not documented |
| `pad(indent, out)` (public pretty-print helper) | 317 | Not documented |
| `either[A, B]` (implicit `Either` instance) | 332 | Partial: the tagged shape `{"Left":…}` is shown only through the trait method |
| `orElseEither[A, B]` (companion, not implicit) | 372 | Not documented as a companion function |
| `iterable[A, T[X] <: Iterable[X]]` (implicit, generic) | 532 | Not documented |
| `keyValueIterable[K, A, T]` (not implicit) | 585 | Not documented |
| `keyValueChunk[K, A]` (not implicit) | 652 | Not documented |
| `fromCodec` | 864 | Documented: json-encoder.md:209, json-codec.md:52 |
| `tuple1` … `tuple22` (generated) | build.sbt:205-231 | Partial: only `zip` (= `tuple2`) is documented |
| `arraySeq` (2.13 and 3) | scala-2.13/JsonEncoderVersionSpecific.scala:9, scala-3/…:10 | Not documented |
| `derived` (Scala 3) | scala-3/JsonEncoderVersionSpecific.scala:23 | Documented: json-encoder.md "Scala 2 vs Scala 3" |
| `iArray` (Scala 3) | scala-3/JsonEncoderVersionSpecific.scala:26 | Not documented |
| `unionOfStringEnumeration` (Scala 3 given) | scala-3/JsonEncoderVersionSpecific.scala:32 | Partial: decoding.md:112-125 only (decoding page) |

#### JsonEncoderPlatformSpecific

| Member | Line | Platform | Status |
|---|---|---|---|
| `encodeJsonStream(a): ZStream[Any, Throwable, Char]` | jvm/src/main/scala/zio/json/JsonEncoderPlatformSpecific.scala:12 | JVM only (js/native traits are empty, line 3) | Documented: streaming.md "Encoding a Single Value as a Stream", json-encoder.md "Streaming" |
| `encodeJsonLinesPipeline: ZPipeline[Any, Throwable, A, Char]` (lazy val) | …:77 | JVM | Documented: streaming.md "JSON Lines". Signature not shown |
| `encodeJsonArrayPipeline` (lazy val) | …:80 | JVM | Documented: streaming.md "JSON Arrays". Signature not shown |
| `encodeJsonDelimitedPipeline` | …:15 | private | skip |

#### JsonFieldEncoder (`S/JsonFieldEncoder.scala`)

| Member | Line | Status |
|---|---|---|
| trait `JsonFieldEncoder[-A]` (contravariant) | 19 | Mentioned only in interop/enumeratum.md:73 |
| `contramap[B](f)` | 22 | Not documented |
| `unsafeEncodeField(in: A): String` | 26 | Not documented |
| `apply[A]` | 30 | Not documented |
| instances `string`, `int`, `long`, `uuid` | 32-46 | Not documented |

#### JsonCodec (`S/JsonCodec.scala`)

| Member | Line | Status |
|---|---|---|
| case class `JsonCodec(encoder, decoder)` | 35 | Documented: json-codec.md "Construction" |
| `<>` / `orElse` | 40 / 61 | Documented |
| `<+>` / `orElseEither` | 45 / 64 | Documented |
| `<*>` / `zip` | 50 / 73 | Documented |
| `*>` / `zipRight` | 53 / 76 | Documented |
| `<*` / `zipLeft` | 55 / 79 | Documented |
| `decodeJson(str: CharSequence)` | 57 | Documented |
| `encodeJson(a, indent)` (no default) | 59 | Documented (json-codec.md:69-71 note) |
| `transform`, `transformOrFail` | 67, 70 | Documented |
| `apply[A]` | 85 | Documented |
| `fromEncoderDecoder` | 87 | Documented (json-codec.md:39) |
| implicit instances (primitives, Option, Either, collections, java.time, UUID, Currency) | 98-196 | Partial: "instances for the standard types", no list |
| `arraySeq` (2.13 and 3), `iArray` (3), `derived` (3) | scala-2.13/JsonCodecVersionSpecific.scala:6; scala-3/…:6, 9, 11 | `derived` documented, the others not |

#### JsonStreamDelimiter (`S/JsonStreamDelimiter.scala`)

| Member | Line | Status |
|---|---|---|
| `Newline`, `Array` | 21-22 | Documented by usage: streaming.md "JSON Lines" and "JSON Arrays". The type exists on every platform, but only the JVM `decodeJsonPipeline` uses it. That is not stated |

#### Package syntax (`S/package.scala`)

| Member | Line | Status |
|---|---|---|
| `EncoderOps.toJson` | 22 | Documented: json-encoder.md "Extension methods" |
| `EncoderOps.toJsonPretty` | 25 | Documented (same) |
| `EncoderOps.toJsonAST` | 27 | Documented (same) |

#### JVM package helpers (`jvm/src/main/scala/zio/JsonPackagePlatformSpecific.scala`; JS and Native are empty, line 3)

| Member | Lines | Status |
|---|---|---|
| `readJsonAs(File \| Path \| String \| URL)` | 12, 15, 18, 21 | Documented in one sentence: streaming.md:136 |
| `readJsonLinesAs[A](File \| Path \| String \| URL)` | 24, 27, 36, 39 | Documented: streaming.md "Reading and Writing Files" |
| `writeJsonLines(File \| Path \| String, ZStream[R, Throwable, Json])` | 53, 56, 59 | Documented in one sentence: streaming.md:136 |
| `writeJsonLinesAs[R, A](File \| Path \| String, stream)` | 62, 65, 74 | Documented |

#### Low-level extension point (`S/internal/writers.scala`)

| Member | Line | Status |
|---|---|---|
| trait `Write` (12 `write` overloads) | 21-33 | Not documented |
| `WriteWriter(out: java.io.Writer)` | 36 | Not documented |

---

### Undocumented API

| Symbol | Source file:line | Priority | Suggested home page/section |
|---|---|---|---|
| `JsonEncoder#unsafeEncode(a: A, indent: Option[Int], out: Write): Unit` (the only abstract member; needed for any `new JsonEncoder[A] { … }`) | S/JsonEncoder.scala:93 | Critical | json-encoder.md, new section "Writing an Encoder from Scratch". Also add it as the first row of the summary table |
| `zio.json.internal.Write` trait | S/internal/writers.scala:21-33 | Critical | Same new section. Pitfalls to state: (1) `write(cs, from, to)` takes an exclusive end index, not a length as in `java.io.Writer` (writers.scala:24, 39-44; FastStringWrite.scala:36-50). (2) The `write(s: Short)` overloads write two packed chars, low byte first (writers.scala:68-71). Users should not call them. (3) The package is named `internal` |
| `JsonFieldEncoder` trait, `unsafeEncodeField`, `contramap`, `apply` | S/JsonFieldEncoder.scala:19-30 | High | New section "Map keys" in json-encoder.md, or a new reference page `json-field-encoder.md` covering both `JsonFieldEncoder` and `JsonFieldDecoder` |
| Built-in `JsonFieldEncoder` instances: `String`, `Int`, `Long`, `UUID` only | S/JsonFieldEncoder.scala:32-46 | High | Same. State that `Map[K, V]` needs a `JsonFieldEncoder[K]`, and that other key types (`Short`, `Boolean`, enums, newtypes) need one built with `JsonFieldEncoder[String].contramap(...)` |
| Generated tuple encoders `tuple1`…`tuple22` (encode as JSON arrays; pretty mode separates elements with `", "` and keeps them on one line) | build.sbt:205-231 | Medium | json-encoder.md "zip and zipWith", plus a built-in instances table |
| `keyValueIterable[K, A, T]` / `keyValueChunk[K, A]` (encode a `List[(K, V)]` or `Chunk[(K, V)]` as a JSON object; deliberately not implicit) | S/JsonEncoder.scala:585, 652 | Medium | json-encoder.md "Map keys" section |
| `JsonEncoder.bump(indent)` / `JsonEncoder.pad(indent, out)` (public helpers for pretty printing inside a custom `unsafeEncode`; `pad` writes a newline plus 2 spaces per level) | S/JsonEncoder.scala:313-330 | Medium | json-encoder.md "Writing an Encoder from Scratch" |
| `JsonEncoder.AbstractJsonEncoder[A]` (base class used by every built-in instance; reduces class-file size) | S/JsonEncoder.scala:119 | Medium | Same section |
| `WriteWriter(out: java.io.Writer)` (adapter that lets `unsafeEncode` write straight into a `java.io.Writer` without building a `String`) | S/internal/writers.scala:36 | Medium | Same section, or explanation/performance.md |
| Implicit generic `iterable[A, T[X] <: Iterable[X]]` (any `Iterable` subtype, e.g. `Queue`, gets an array encoder) | S/JsonEncoder.scala:532 | Low | Built-in instances table |
| `JsonEncoder.orElseEither[A, B]` (companion, explicit) | S/JsonEncoder.scala:372 | Low | json-encoder.md "either …" |
| `arraySeq` encoder/codec (2.13 and 3), `iArray` encoder/codec (Scala 3) | scala-2.13/JsonEncoderVersionSpecific.scala:9; scala-3/JsonEncoderVersionSpecific.scala:10, 26; scala-3/JsonCodecVersionSpecific.scala:6, 11 | Low | Built-in instances table, "Scala 2 vs Scala 3" |
| `unionOfStringEnumeration` (Scala 3: `"a" \| "b"` encodes as a string) | scala-3/JsonEncoderVersionSpecific.scala:32 | Low | json-encoder.md "Scala 2 vs Scala 3" (currently only in how-to/decoding.md:112) |
| `JsonFieldEncoder` being contravariant (`[-A]`), so `JsonFieldEncoder[String]` also serves `T <: String` keys, such as Scala 3 string-literal unions | S/JsonFieldEncoder.scala:19 | Low | "Map keys" section |

#### Built-in encoder instances (Task 2)

All are in `S/JsonEncoder.scala` unless noted. "Listed?" says whether any page under `docs/` lists the type as supported.

| Instance | Source line | JSON shape | Listed in docs? |
|---|---|---|---|
| `String`, `Char`, `Symbol` | 152, 196, 244 | string | String yes (implicitly). Char and Symbol no |
| `Boolean` | 232 | `true`/`false` | implicit only |
| `Unit` | 239 | `{}` | Only indirectly (json-codec.md:150-160 example) |
| `Byte`, `Short`, `Int`, `Long` | 245-264 | number | Not listed |
| `java.math.BigInteger`, `BigInt` | 265, 271 | number | No (third-party-types.md:15 mentions `BigDecimal` only) |
| `Double`, `Float` | 278, 283 | number. **NaN / ±Infinity are written as the quoted strings `"NaN"`, `"Infinity"`, `"-Infinity"`** (jvm-native/…/SafeNumbers.scala:287-292, 393-395; EncoderSpec.scala:91, 166) | **No. The NaN behaviour is not documented anywhere** (`grep NaN docs/` = 0) |
| `java.math.BigDecimal`, `BigDecimal` | 288, 294 | number | third-party-types.md:15 only |
| `Option[A]` | 300 | `null`, or omitted as a field (`isNothing`). `Some(None)` also counts as nothing | json-encoder.md:180 (Option) |
| `Either[A, B]` | 332 | `{"Left":…}` / `{"Right":…}` | json-encoder.md "either" section (through the trait method) |
| `Array[A]` (needs `ClassTag`) | 386 | array | No |
| `Seq`, `Chunk`, `NonEmptyChunk`, `IndexedSeq`, `LinearSeq`, `ListSet`, `TreeSet`, `List`, `Vector`, `Set`, `HashSet`, `SortedSet`, any `Iterable` | 439-512, 532 | array | **No** (`grep "NonEmptyChunk\|Vector\|Set\[" docs/` = 0) |
| `Map`, `HashMap`, `mutable.Map`, `collection.SortedMap`, `ListMap` (key needs `JsonFieldEncoder`) | 514-526 | object. **Entries whose value `isNothing` (e.g. `None`) are dropped** (604, 618) | **No** (`grep "Map\[" docs/` = 0) |
| Tuples 1-22 | build.sbt:205-231 | array | Only `zip` (pair) |
| java.time: `DayOfWeek`, `Duration`, `Instant`, `LocalDate`, `LocalDateTime`, `LocalTime`, `Month`, `MonthDay`, `OffsetDateTime`, `OffsetTime`, `Period`, `Year`, `YearMonth`, `ZonedDateTime`, `ZoneId`, `ZoneOffset` (16 types) | 664-838 | ISO-8601 strings (S/javatime/serializers.scala) | Only `Instant` is named (third-party-types.md:15). The other 15 are not listed |
| `java.util.UUID` | 840 | string | third-party-types.md:15 |
| `java.util.Currency` | 851 | string (currency code) | No |
| `Json` and each subtype (`Obj`, `Arr`, `Bool`, `Str`, `Num`, `Null`) | S/ast/ast.scala:402-624 | as is | reference/json-ast.md |
| `ArraySeq` (2.13/3), `IArray` (3), string-literal unions (3) | see above | array / string | Unions only in decoding.md |
| `JsonCodec` instances: same set **minus** `Unit`, `IndexedSeq`, `LinearSeq`, `ListSet`, `TreeSet`, `mutable.Map` | S/JsonCodec.scala:98-196 | — | Only `Unit` is called out (json-codec.md:145) |

---

### Partially documented

| Item | Where | What is missing |
|---|---|---|
| `toJsonAST` as an overridable trait method | json-encoder.md:76-77 | No signature (`def toJsonAST(a: A): Either[String, Json]`, S/JsonEncoder.scala:101). It does not say that every built-in instance overrides it, or that tuple encoders (build.sbt:216-222) and `JsonEncoder.orElseEither` (S/JsonEncoder.scala:372-379) do not, and so fall back to encode-then-parse |
| `isEmpty` | json-encoder.md:181 | "The collection encoders override `isEmpty`". The map encoders (`keyValueIterable`, line 589), `Array` (388), `List` (455) and `ArraySeq` override it too. `NonEmptyChunk` inherits it through `contramap` (443). No link to `json-codec-configuration.md#explicitemptycollections` / `#explicitnulls` |
| `Map` value omission | none | `keyValueIterable` skips entries where `A.isNothing(v)` (S/JsonEncoder.scala:604, 618), so `Map("a" -> None).toJson == "{}"`. This is not documented anywhere |
| Streaming encoders | streaming.md, json-encoder.md:224-228 | No signatures. Not stated that (a) output is always compact: the pipeline calls `unsafeEncode(x, indent = None, …)` (jvm/…/JsonEncoderPlatformSpecific.scala:65). (b) Output is buffered and released in blocks of `ZStream.DefaultChunkSize` (4096) chars, not per element (…:42, 49, 70). (c) JSON Lines output ends with a trailing `\n` (…:78, `endWith = None`) |
| `readJsonAs` / `writeJsonLines` | streaming.md:136 | One sentence, no example. Not stated that `writeJsonLinesAs` writes UTF-8 (JsonPackagePlatformSpecific.scala:80-87) |
| `JsonCodec` companion instances | json-codec.md:36 | "instances for the standard types". There is no codec for `IndexedSeq`, `LinearSeq`, `ListSet`, `TreeSet` or `mutable.Map`, although both `JsonEncoder` (445-451, 519) and `JsonDecoder` (JsonDecoder.scala:807-831, 887) have instances. `JsonCodec[immutable.TreeSet[Int]]` therefore does not resolve. `JsonCodec.sortedMap` needs `Ordering[K]` (JsonCodec.scala:151-155), `JsonEncoder.sortedMap` does not |
| `fromEncoderDecoder` | json-codec.md:39 | It is declared `implicit`, but it takes explicit (non-implicit) parameters, so implicit search never uses it. It is just an alias for `JsonCodec(e, d)`. Readers may assume it derives a codec automatically |
| Scala 3 `derived` config | json-encoder.md:214-215, json-codec.md:173 | "needs a given `JsonCodecConfiguration`". The companion already supplies `implicit val default` (scala-3/JsonCodecConfiguration.scala:158), so users need a given only to change the defaults |
| `JsonStreamDelimiter` | streaming.md | Not stated that the type is shared across platforms (S/JsonStreamDelimiter.scala) while only the JVM `decodeJsonPipeline` consumes it |
| Pretty-print format | json-encoder.md:41 | "two spaces per level" is correct (pad writes `8224` = two spaces per level, S/JsonEncoder.scala:317-330). Not stated: fields render as `"k" : v` with spaces around the colon (S/JsonEncoder.scala:623, 356), tuples stay on one line with `", "` |
| Union string types (Scala 3) encoder side | how-to/decoding.md:112-125 | Not mentioned on json-encoder.md or encoding.md |

---

### Wrong or outdated statements

| Page:line | Claim | Truth + source | Corrected statement |
|---|---|---|---|
| explanation/performance.md:6 | "We only compare `zio-json` against Circe and Play" | The JMH sources compare zio-json against **Circe and jsoniter-scala**: `encodeCirce`, `encodeJsoniter`, `encodeZio` in zio-json/jvm/src/jmh/scala/zio/json/{GoogleMapsAPI,TwitterAPI,GeoJSON,Synthetic}Benchmarks.scala (e.g. GoogleMapsAPIBenchmarks.scala:108-192). build.sbt has no Play JSON dependency (only circe 119/144-146 and jsoniter-scala 142-143). So the `play` rows in all four tables cannot be reproduced with the command given | "The benchmarks compare `zio-json` against Circe and jsoniter-scala. The Play JSON rows in the tables below come from an older version of the benchmark suite and can no longer be reproduced with it." (Better: re-run and replace the tables with circe and jsoniter rows) |
| explanation/performance.md:8 | "When used with Loom, `zio-json` has finished its work before the others even begin." | No Loom mode exists. `grep -rni loom` over all `*.scala`/`*.sbt` = 0 hits | Delete the sentence |
| explanation/performance.md:6 (also 14-15, 27-28, 40-41, 53-54) | `sbt "zioJsonJVM/jmh:run -prof gc"` | sbt is 1.13.0 (project/build.properties). The `config:task` colon syntax is deprecated in sbt 1.x in favour of slash syntax. It still works today (build.sbt:36 uses `jmh:compile`) | `sbt "zioJsonJVM/Jmh/run -prof gc"` (Low) |
| explanation/performance.md:70 | "a port supporting XML is also be possible" | Grammar | "a port supporting XML is also possible" |
| how-to/streaming.md:140 | "If you want to skip malformed lines instead, handle the error in the stream, for example with `ZStream#catchAll`." | A failed `ZStream` cannot resume. `catchAll` only replaces the rest of the stream, so lines after the bad one are lost. `decodeJsonPipeline` turns the decode error into a stream failure (jvm/…/JsonDecoderPlatformSpecific.scala:121-124, failure pushed to `outQueue`) | "A malformed value fails the stream at that point, and the values before it have already been emitted. A failed stream cannot resume, so `catchAll` can only replace the remainder (for example with `ZStream.empty`). To skip malformed lines and keep going, split the text with `ZPipeline.splitLines` and decode each line with `fromJson`, keeping the `Right` results." |
| how-to/encoding.md:62 | "convenience methods on the `JsonEncoder` typeclass to *derive* new decoders" | Encoders, not decoders (the method is `contramap`, S/JsonEncoder.scala:34) | "…convenience methods on the `JsonEncoder` typeclass to *derive* new encoders:" |
| reference/json-codec.md:36 | "The companion has instances for the standard types." | There are no codec instances for `Unit`, `IndexedSeq`, `immutable.LinearSeq`, `immutable.ListSet`, `immutable.TreeSet` or `mutable.Map` (S/JsonCodec.scala:84-196), although encoders and decoders exist for all of them | "The companion has codec instances for the primitive types, `Option`, `Either`, tuples, the common collections (`List`, `Vector`, `Seq`, `Set`, `Chunk`, `NonEmptyChunk`, `Array`, `Map` and others), java.time types, `UUID` and `Currency`. A few types have an encoder and a decoder but no codec: `Unit`, `IndexedSeq`, `LinearSeq`, `ListSet`, `TreeSet` and `mutable.Map`. Build those with `JsonCodec(JsonEncoder[T], JsonDecoder[T])`." |
| reference/json-codec.md:144 | "`zipRight` is only available when `A` is `Unit`" | The constraint is `implicit ev: Unit <:< A` (S/JsonCodec.scala:76), so it also compiles for supertypes of `Unit` such as `Any` or `AnyVal` | "`zipRight` is only available when `Unit` is a subtype of `A` (usually `A` is `Unit`), because it encodes `()` for the dropped side." |
| reference/json-encoder.md:215 and reference/json-codec.md:173 | "It needs a given `JsonCodecConfiguration`." / "which needs a given `JsonCodecConfiguration`" | `JsonCodecConfiguration.default` is an implicit in the companion (scala-3/JsonCodecConfiguration.scala:158), so a given is always found | "It takes a `using JsonCodecConfiguration`. The default configuration is found automatically, so define your own given only to change it." |
| reference/json-encoder.md:181 | "The collection encoders override `isEmpty`." | Also the map encoders (S/JsonEncoder.scala:589), and `Array`/`List`/`ArraySeq`. Not wrong but incomplete | "The collection and map encoders (`Array`, `List`, every `Iterable`, `Map` and its variants) override `isEmpty`." |
| how-to/manual-instances.md:41 | "wheres we really expect" | Typo | "whereas we really expect" |
| how-to/manual-instances.md:60 | "Another use case is if we want to *encode* a `case class` as an array of values" | Only a decoder is shown (lines 62-68). Nothing on the page encodes `Things` | Add after line 67: `implicit val encoder: JsonEncoder[Things] = JsonEncoder[(String, Int, Boolean)].contramap(t => (t.s, t.i, t.b))`, and say "which writes and parses the following JSON" |

Verified correct (no change needed): json-encoder.md:36-42 (signature and 2 spaces per level), :62-68 (EncoderOps, S/package.scala:21-28), :77 (default `toJsonAST`, S/JsonEncoder.scala:101), :88/:185 (`contramap` forwards `isNothing`/`isEmpty`, and also `toJsonAST`, :34-43), :104-108 (`narrow` is a cast, :91), :118-124 (five combinators, final, by-name), :180, :190-195, :209, :226-227 (JVM only; js/native `JsonEncoderPlatformSpecific` are empty). json-codec.md:15, :30, :52 (both `fromCodec`s: S/JsonEncoder.scala:864, S/JsonDecoder.scala:1244), :63-71, :83-90, :117-120 (`orElse` really does encode with `that`, S/JsonCodec.scala:62), :141-145, :150-160 (`JsonEncoder.unit` writes `{}`, :240). streaming.md:9, :13, :43, :57, :90 (default `JsonStreamDelimiter.Array`, JsonDecoderPlatformSpecific.scala:57), :117-119 (overloads match JsonPackagePlatformSpecific.scala:24-75), :136. third-party-types.md:15, :79-81, :119. performance.md:8 ratios (Circe 1.06x-3.01x, Play 1.48x-24.3x, both match the tables) and :64 (zio has the lowest decode MB/sec in all 4 tables and trails Circe on encode MB/sec in all 4).

---

### Missing examples / cross-links

1. **json-encoder.md: no example of a hand-written encoder** (`new JsonEncoder[A] { def unsafeEncode(...) }`) even though line 13 says "Use it directly when you write an encoder by hand". Add one showing `out.write`, delegating to `JsonEncoder.string.unsafeEncode` for escaping, `JsonEncoder.pad`/`bump` for pretty output, and overriding `toJsonAST`.
2. **json-encoder.md: no `Map` example and no key-type example.** Show `Map[UUID, Int]`, a custom key via `JsonFieldEncoder[String].contramap(_.value)`, and `Map("a" -> None).toJson == "{}"`.
3. **json-encoder.md "Missing and Empty Values"** should link to `json-codec-configuration.md#explicitnulls` and `#explicitemptycollections`, not only the page root.
4. **No table of built-in instances** on any page (see Task 2 table). json-encoder.md "Instances and Summoning" is the natural home. encoding.md:56 ("Almost all of the standard library data types are supported") should link to it.
5. **encoding.md**: no `toJsonPretty` example, and no output for the sealed-trait wrapper shape (it only links to sealed-traits.md). No links to reference/json-encoder.md, optional-and-default-fields.md, field-names.md or json-codec-configuration.md. Its "Manual instances / .contramap" section duplicates manual-instances.md and json-encoder.md "contramap". Replace it with a link.
6. **manual-instances.md**: no See Also. Should link to reference/json-encoder.md#contramap, reference/json-decoder.md#map-and-maporfail, third-party-types.md, json-codec.md#transforming-a-codec. Missing the encoder half of the tuple example (see Wrong table).
7. **third-party-types.md**: does not cover using the foreign type as a **Map key** (`JsonFieldEncoder.contramap` / `JsonFieldDecoder.mapOrFail`). The "See also" lacks reference/json-encoder.md and reference/json-decoder.md.
8. **streaming.md**: no link to reference/json-encoder.md#streaming / json-decoder.md#streaming. No `readJsonAs`/`writeJsonLines` example. No `decodeJsonStreamInput` example (it is mentioned at line 43).
9. **performance.md**: no link to the benchmark sources (`zio-json/jvm/src/jmh/...`). No encoder-side tips (see Conceptual gaps).
10. **json-codec.md**: no example of `orElse`'s surprising encode direction, even though line 120 describes it. A two-line mdoc example would make it concrete.

---

### Conceptual gaps

1. **No guide to writing a fast low-level encoder.** `unsafeEncode`, `Write`, `AbstractJsonEncoder`, `bump`/`pad`, the `toJsonAST` override, and the rule "escape strings by delegating to `JsonEncoder.string`" appear nowhere. Suggest a how-to "Write a custom low-level encoder", or a section in json-encoder.md. Include the `Write.write(cs, from, to)` exclusive-end pitfall and the warning that `zio.json.internal` has no compatibility guarantee.
2. **No page on map keys** (`JsonFieldEncoder` / `JsonFieldDecoder`). Users hit "could not find implicit JsonFieldEncoder" for `Map[MyId, V]` with no docs to turn to. Suggest a reference page `json-field-encoder.md` (covering both directions), or a how-to "Use a custom type as a Map key".
3. **Non-finite numbers.** `Double`/`Float` NaN and ±Infinity are encoded as quoted strings (`"NaN"`), which is not standard JSON numbers, and this is undocumented. It also interacts with the `toJsonAST` bug below.
4. **Streaming encoder semantics.** Undocumented: (a) the output is always compact; (b) output is buffered and emitted in roughly 4096-char blocks, so `encodeJsonLinesPipeline` on a slow or infinite source (SSE, websockets, log tailing) emits nothing until about 4 KB accumulate; (c) the JSON Lines pipeline ends with a trailing newline; (d) every chunk runs on the blocking pool (`ZIO.attemptBlocking`, JsonEncoderPlatformSpecific.scala:53, 63).
5. **Encoder performance tips** are missing from explanation/performance.md: the thread-local pooled `FastStringWrite` behind `encodeJson` (S/JsonEncoder.scala:120-148), why `toJsonPretty` is slower, overriding `toJsonAST` to avoid the encode-then-parse default, and `WriteWriter` for writing straight to a `java.io.Writer`.
6. **Platform matrix.** No single place says what is JVM-only (`encodeJsonStream`, both pipelines, `decodeJsonStream*`, `decodeJsonPipeline`, all file helpers in `zio.json` package object) versus cross-platform (everything else, including `JsonStreamDelimiter`). Pages repeat "JVM only" separately.
7. **`Option` nesting.** `Some(None)` counts as "nothing" (S/JsonEncoder.scala:306), so `Option[Option[A]]` cannot distinguish an absent field from an explicit null on encode. Worth a note in optional-and-default-fields.md.

---

### Library bugs noticed

| # | Severity | Location | Bug | Minimal repro idea |
|---|---|---|---|---|
| B1 | Medium | S/JsonEncoder.scala:278-282 (`double.toJsonAST`) and :283-287 (`float`) → S/ast/ast.scala:542-543 | `Json.Num(value: Double)` does `new java.math.BigDecimal(SafeNumbers.toString(value))`. For NaN/±Infinity, `SafeNumbers.write` emits the *quoted* text `"NaN"` (jvm-native/…/SafeNumbers.scala:287-292; same on JS, js/…/SafeNumbers.scala:285-287), so `BigDecimal` throws `NumberFormatException`. `toJsonAST` **throws** instead of returning `Left`/`Right`, and is inconsistent with `toJson` (which gives `"\"NaN\""`, EncoderSpec.scala:166). It also hits any derived case class with a NaN `Double` field via `toJsonAST`, and `Json.Num(Double.NaN)` directly | `Double.NaN.toJsonAST` → expect `Right(Json.Str("NaN"))` or `Left(...)`, get `NumberFormatException` |
| B2 | Medium | jvm/src/main/scala/zio/JsonPackagePlatformSpecific.scala:80-87 (`charsToUtf8`) with JsonEncoderPlatformSpecific.scala:27-45 | `writeJsonLinesAs` converts each `Char` chunk to bytes on its own with `new String(chunk.toArray).getBytes(UTF_8)`. The encoder pipeline releases chunks at arbitrary 4096-char `BufferedWriter` flush boundaries, so a surrogate pair (emoji or other non-BMP character) can be split across two chunks. Each lone surrogate then encodes as `?`, which silently corrupts the file | `writeJsonLinesAs(path, ZStream("a" * 4094 + "😀"))`: the opening quote plus 4094 `a` put the high surrogate at index 4095, so the first flush ends with it. The file contains `??` instead of the emoji. Fix idea: use `ZPipeline.utf8Encode` on a `String` stream, or carry a trailing high surrogate over to the next chunk |
| B3 | Medium (behavioural) | jvm/src/main/scala/zio/json/JsonEncoderPlatformSpecific.scala:42, 49, 70 | The pipelines emit only what the internal `BufferedWriter` (size `ZStream.DefaultChunkSize`) has flushed. Nothing is flushed per push, so for a slow or infinite source no output appears until about 4096 chars accumulate or the stream ends. This hurts SSE and streaming responses | `ZStream.repeatWithSchedule(Event(1,"x"), Schedule.spaced(1.second)).via(JsonEncoder[Event].encodeJsonLinesPipeline).take(1).runCollect` takes about (4096 / line length) seconds instead of 1 |
| B4 | Low (inconsistency) | S/JsonEncoder.scala:600-611 vs 630-648 (`keyValueIterable`) | `unsafeEncode` drops entries where `A.isNothing(v)`, but `toJsonAST` drops entries whose AST is `Json.Null`. For `Map[String, Json]` (the `Json` encoder does not override `isNothing`, S/ast/ast.scala:610-622), `toJson` and `toJsonAST` disagree | `val m = Map("a" -> (Json.Null: Json))`; `m.toJson == """{"a":null}"""` but `m.toJsonAST == Right(Json.Obj())` |
| B5 | Low (design surprise) | S/JsonCodec.scala:61-62 | `a.orElse(b)` (`<>`) encodes **every** value with `b`'s encoder (`transform(_.merge, Right(_))`), not with the primary codec `a`. Documented correctly at json-codec.md:120, but most users would expect `Left(_)` (encode with the primary) | `(JsonCodec[Int].transform[Int](identity, identity) <> JsonCodec(JsonEncoder.int.contramap[Int](_ * 10), JsonDecoder.int)).encodeJson(1, None)` gives `10` |
| B6 | Low (latent) | jvm/src/main/scala/zio/json/JsonEncoderPlatformSpecific.scala:32-35 | `System.arraycopy(buffer, offset, copy, 0, len)` already applies `offset`, then `Chunk.fromArray(copy).drop(offset).take(len)` drops `offset` chars a second time. Unreachable today: `BufferedWriter.flushBuffer` always calls `write(cb, 0, n)`, and `WriteWriter` only uses `write(String)`/`write(int)`. It would lose data if the inner `Writer` ever received a non-zero offset | Call the anonymous writer's `write(Array('a','b','c'), 1, 2)` directly; expect `bc`, get `c` |
| B7 | Low (scaladoc) | S/JsonCodec.scala:31-33 | The scaladoc example `intCodec.encodeJson(intCodec.encodeJson(42)) == Right(42)` is wrong: it nests `encodeJson` twice, and `JsonCodec#encodeJson` requires an `indent` argument | Should read `intCodec.decodeJson(intCodec.encodeJson(42, None)) == Right(42)` |
| B8 | Low (API gap) | S/JsonCodec.scala:84-196 | No `JsonCodec` instances for `Unit`, `IndexedSeq`, `LinearSeq`, `ListSet`, `TreeSet` or `mutable.Map`, though both halves exist (S/JsonEncoder.scala:239, 445-451, 519; S/JsonDecoder.scala:327, 807-831, 887) | `JsonCodec[scala.collection.immutable.TreeSet[Int]]` does not compile |


## Appendix B: decoding and errors

> Runtime check: bug 1 below (BigInt DoS through `Json`) did not reproduce; `as[BigInt]` returns `Left((expected a 256-bit BigInt))` at once.


Source: `series/2.x` at `d59b926ea40`.
All paths below are relative to the repo root. `JD` = `zio-json/shared/src/main/scala/zio/json/JsonDecoder.scala`,
`LX` = `zio-json/shared/src/main/scala/zio/json/internal/lexer.scala`, `RD` = `.../internal/readers.scala`,
`FD2`/`FD3` = `zio-json/shared/src/main/scala-2.x|scala-3/zio/json/JsonFieldDecoder.scala`,
`PS` = `zio-json/jvm/src/main/scala/zio/json/JsonDecoderPlatformSpecific.scala`,
`UN` = `zio-json/jvm-native/src/main/scala/zio/json/internal/UnsafeNumbers.scala`.
Everything was checked by reading source; nothing was compiled or run. Items marked "(code reading)" should get
an mdoc example to confirm before the docs state them.

---

### 0. Coverage inventory (task 1)

#### JsonDecoder trait (JD:34-263)

| Symbol | Source | Status | Where |
|---|---|---|---|
| `decodeJson(CharSequence / Chunk[Byte] / Array[Byte])` | JD:91, 108, 124 | Documented | json-decoder.md "decodeJson"; decoding.md "Decoding from bytes" |
| `fromJsonAST` | JD:256 | Documented | json-decoder.md "fromJsonAST" |
| `map`, `mapOrFail` | JD:180, 195 | Documented | json-decoder.md "map and mapOrFail"; validate-on-decode.md |
| `widen` | JD:129 | Documented | json-decoder.md "widen" |
| `zip`/`zipLeft`/`zipRight`/`zipWith`, `<*>`, `<*`, `*>` | JD:222-237, 50-60 | Documented | json-decoder.md "zip, zipLeft..." |
| `both`/`bothLeft`/`bothRight`/`bothWith` | JD:62-83 | Documented | json-decoder.md "both..." |
| `orElse`/`orElseEither`, `<>`, `<+>` | JD:145, 174, 40, 45 | Documented | json-decoder.md "orElse and orElseEither" |
| `unsafeDecodeMissing` | JD:239 | Partial | json-decoder.md "Missing Fields" (signature only, no how-to-override example, collections not mentioned) |
| `unsafeDecode(trace, in: RetractReader)` | JD:245 | Partial | Named in errors.md "Where errors appear" only; no signature, no contract, no example |
| `unsafeFromJsonAST(trace, json)` | JD:247 | Not documented | (default re-encodes AST to text and re-parses) |

#### JsonDecoder companion (JD:265-730 + low-priority traits)

| Symbol | Source | Status | Where |
|---|---|---|---|
| `apply[A]` | JD:272 | Documented (one sentence) | json-decoder.md intro |
| `AbstractJsonDecoder[A]` | JD:268 | Not documented | - |
| `type JsonError` / `val JsonError` aliases | JD:269-270 | Not documented | - |
| `UnsafeJson(trace)` | JD:279 | Partial | errors.md "Where errors appear" (name only) |
| `peekChar` | JD:283 | Documented | json-decoder.md "peekChar" |
| `suspend` | JD:294 | Documented | json-decoder.md "Recursive Decoders" |
| `keyValueChunk[K, A]` (non-implicit) | JD:943 | Not documented | - |
| `fromCodec` (low-priority implicit) | JD:1244 | Documented | json-codec.md (line 52) |
| `derived` (Scala 3) | scala-3/JsonDecoderVersionSpecific.scala:21 | Documented | json-decoder.md "Scala 2 vs Scala 3" |
| Built-in instances (see section 2) | JD:307-1240, build.sbt:161-203 | Not documented as a list | - |

#### JsonFieldDecoder (FD2 / FD3)

| Symbol | Source | Status |
|---|---|---|
| trait `JsonFieldDecoder[+A]`, `unsafeDecodeField` | FD2/FD3:22, 42 | Not documented (only named in interop/enumeratum.md:73) |
| `map`, `mapOrFail` | FD2/FD3:25, 32 | Not documented |
| `JsonFieldDecoder.apply` | FD2/FD3:46 | Not documented |
| instances `string`, `int`, `long`, `uuid` | FD2/FD3:48-74 | Not documented |
| Scala 3 only `stringLike[T <: String]` (low priority) | FD3:83 | Not documented (Scala 2 `LowPriorityJsonFieldDecoder` is empty, FD2:81) |

#### JsonError (zio-json/shared/src/main/scala/zio/json/JsonError.scala)

| Symbol | Source | Status |
|---|---|---|
| `JsonError` sealed abstract class | :22 | Documented (errors.md "JsonError") |
| `render` | :25 | Documented |
| `Message`, `ArrayAccess`, `ObjectAccess`, `SumType` | :37-43 | Documented (table) |

#### Package-object syntax (zio-json/shared/src/main/scala/zio/json/package.scala)

| Symbol | Source | Status |
|---|---|---|
| `DecoderOps.fromJson` (CharSequence) | :30, 41 | Documented (json-decoder.md "Extension methods") |
| `DecoderBytesOps.fromJson` (Chunk[Byte]) | :44, 51 | Documented |
| `DecoderByteArrayOps.fromJson` (Array[Byte]) | :54, 61 | Documented |
| `fromJsonAST` syntax | - | Does not exist; the AST side is `Json#as[A]` (ast.scala:39), documented in json-ast.md |

#### Platform-specific

| Symbol | Source | Platform | Status |
|---|---|---|---|
| `decodeJsonStreamInput(stream, charset = UTF_8)` | PS:31 | JVM only (js/native traits empty) | Documented (streaming.md, json-decoder.md "Streaming") |
| `decodeJsonStream(stream)` | PS:53 | JVM | Documented |
| `decodeJsonPipeline(delimiter = Array)` | PS:56 | JVM | Documented |
| `readJsonAs` x4 (File/Path/String/URL) | zio-json/jvm/src/main/scala/zio/JsonPackagePlatformSpecific.scala:12-22 | JVM (js/native `JsonPackagePlatformSpecific` empty) | Documented (streaming.md:136) |
| `readJsonLinesAs` x4 | same file :24-50 | JVM | Documented (streaming.md:119) |

#### Low-level extension points (zio.json.internal)

| Symbol | Source | Visibility | Status |
|---|---|---|---|
| `RetractReader` (`readChar`, `nextNonWhitespace`, `retract`, `read`) | RD:84-88 (+ inherited RD:31-62) | public sealed trait; parent `OneCharReader` is `private[zio]` | Named only in how-it-works.md as a concept |
| `WithRetractReader(java.io.Reader)` | RD:319 | public final class | Named in how-it-works.md:24 (and called "internal") |
| `FastStringReader`, `Utf8ChunkReader`, `RecordingReader` | RD:99, 156, 349 | `private[zio]` | Named in how-it-works.md (correctly not user API) |
| `Lexer.error(msg, trace)` | LX:33 | public | Partial (errors.md:52) |
| `Lexer.error(c: Char, got: Char, trace)` | LX:1795 | public | Not documented |
| `Lexer.char(trace, in, c)` | LX:1790 | public | Not documented |
| `Lexer.firstField`, `nextField`, `firstArrayElement`, `nextArrayElement` | LX:43-69 | public | Not documented |
| `Lexer.field`, `enumeration`, `field128`, `enumeration128` + `StringMatrix` | LX:71-124, 1816 | public | Not documented |
| `Lexer.skipValue`, `skipNumber` | LX:126, 139 | public | Not documented |
| `Lexer.string`, `boolean`, `char`, `byte`..`bigDecimal`, `uuid`, java.time readers, `readChars` | LX:178-1804 | public | Not documented |
| `Lexer.NumberMaxBits = 256` | LX:31 | public | Not documented by name (security.md:97 mentions "256 bits") |
| `UnsafeNumbers` / `SafeNumbers` | UN, jvm-native/.../SafeNumbers.scala | public objects | Not documented |
| `javatime.parsers`, `uuid.UUIDParser` | parsers.scala:22, UUIDParser.scala:20 | `private[json]` | Correctly not documented |

---

### 1. Undocumented API

| Symbol | Source file:line | Priority | Suggested home |
|---|---|---|---|
| `JsonFieldDecoder` trait, `unsafeDecodeField`, `map`, `mapOrFail`, `apply` | FD2/FD3:22-46 | Critical | New section "Map keys" in json-decoder.md (or a small reference page `json-field-decoder.md` paired with `JsonFieldEncoder`), plus a how-to step in manual-instances.md "Use your own type as a map key" |
| Built-in `JsonFieldDecoder` instances: `String`, `Int`, `Long`, `UUID` only (+ Scala 3 `stringLike` for `T <: String` with a `JsonDecoder[T]`) | FD2:48-74, FD3:48-88 | Critical | Same section; state explicitly that `Map[LocalDate, V]`, `Map[Short, V]`, enum keys etc. need a hand-written key decoder |
| Full list of built-in `JsonDecoder` instances (section 2) | JD:307-1240; build.sbt:161-203; scala-2.13/3 VersionSpecific | High | json-decoder.md new section "Predefined instances" (table: type, JSON shape accepted, notes) |
| `unsafeDecode` contract (who calls it, `trace` meaning, must throw via `Lexer.error`, must leave reader after the value) | JD:241-245 | High | json-decoder.md "Writing a low-level decoder" + new how-to "Write a streaming decoder by hand" |
| `unsafeFromJsonAST` and its default (re-encode + re-parse) | JD:247-248 | High | json-decoder.md, next to `fromJsonAST`; explain overriding it for speed |
| `AbstractJsonDecoder` (what to extend for hand-written decoders; reduces class-file size) | JD:267-268 | Medium | Same low-level section |
| `JsonDecoder.UnsafeJson` (catchable case class, `trace` field, NoStackTrace) | JD:279-281 | Medium | errors.md "Where errors appear" |
| `Lexer` helpers for hand-written decoders: `error`, `char`, `firstField`/`nextField`, `firstArrayElement`/`nextArrayElement`, `string`, `skipValue`, `field` + `StringMatrix` | LX:33-139, 267, 1790-1797, 1816 | Medium | Low-level how-to (one worked example: decode `{"x":..,"y":..}` into a `Point` with `Lexer.char`, `Lexer.string`, `Lexer.int`) |
| `RetractReader` / `WithRetractReader` as the only way to feed `unsafeDecode` from user code (e.g. in unit tests: `new WithRetractReader(new java.io.StringReader(s))`) | RD:84, 319 | Medium | Same low-level how-to |
| `JsonDecoder.keyValueChunk[K, A]` (decodes an object to `Chunk[(K, A)]` keeping order and duplicates; not implicit because it overlaps with list-of-tuples) | JD:942-953 | Medium | json-decoder.md "Predefined instances" |
| `Either` wire format: object with one key out of `a`/`Left`/`left` or `b`/`Right`/`right`; errors `missing fields`, `ambiguous either, zip present`, `duplicate` | JD:654-689 | Medium | json-decoder.md "Predefined instances" (and json-encoder.md for the write side) |
| Tuples `Tuple1`..`Tuple22` decode from a JSON array of exactly N elements | build.sbt:164-203 | Medium | json-decoder.md "Predefined instances" (zip section already alludes to it) |
| `JsonDecoder.unit` accepts and skips any JSON value | JD:327-334 | Low | Predefined instances |
| `Char` (single-character string), `Symbol`, `java.util.Currency` (ISO code) | JD:336-346, 1222-1240 | Low | Predefined instances |
| Scala 2.13/3 `ArraySeq`, Scala 3 `IArray` | scala-2.13/JsonDecoderVersionSpecific.scala:9; scala-3/JsonDecoderVersionSpecific.scala:11, 24 | Low | Predefined instances |
| Scala 3 `unionOfStringEnumeration` given (string literal unions) | scala-3/JsonDecoderVersionSpecific.scala:30-35 | Low | Already shown in decoding.md; add to Predefined instances with error text `expected one of: ...` |
| `type JsonError` / `val JsonError` aliases in `JsonDecoder` companion | JD:269-270 | Low | errors.md one line |
| `Lexer.NumberMaxBits` (256) as the named limit | LX:31 | Low | security.md last paragraph |

---

### 2. Built-in decoder instances and behaviours (task 2)

No page lists the built-in instances. Only tutorial/getting-started.mdx:105 ("`Option`, `List`, `Map`, `String`, and the numeric types"), decoding.md:128 ("Almost all of the standard library data types") and third-party-types.md:15 (`UUID`, `Instant`, `BigDecimal`) mention a few.

| Group | Instances (source) | Behaviour worth documenting |
|---|---|---|
| Text | `String` JD:307, `Char` JD:336, `Symbol` JD:346 | `Char` needs a one-character string; escapes allowed |
| Boolean / Unit | `Boolean` JD:317, `Unit` JD:327 | `Unit` skips any value without validating it |
| Integers | `Byte` 348, `Short` 378, `Int` 408, `Long` 437, `java.math.BigInteger` 467, `BigInt` 497 | Accept quoted numbers (`"42"` decodes as `42`, JD:410-417). Reject `+1` (test DecoderSpec.scala:69). Accept leading zeros (`007` -> 7, UN:91-116, code reading). Overflow is an error. `BigInteger`/`BigInt` limited to < 256 bits (UN:151-188); no exponent form accepted when streaming. |
| Floating | `Float` 526, `Double` 552, `java.math.BigDecimal` 578, `BigDecimal` 605 | Quoted numbers accepted. Quoted `"NaN"`, `"Infinity"`, `"+Infinity"`, `"-Infinity"` accepted, bare `NaN`/`Infinity` rejected (UN:360-374, 631-640; DecoderSpec.scala:100-107). BigDecimal mantissa limited to 256 bits but the exponent is limited only by `Int` range (UN:279-310). Out-of-range floats become +/-Infinity (DecoderSpec.scala:91-92). |
| Option | `option` JD:638 | `null` -> `None`; missing field -> `None` via `unsafeDecodeMissing`; `Option[Option[A]]` cannot tell `null` from missing (outer decoder consumes the `null`). |
| Either | `either` JD:657 | See section 1. |
| Sequences | `Array` 741, `Seq` 773, `Chunk` 781, `NonEmptyChunk` 804 (error `Chunk was empty`), `IndexedSeq` 807, `LinearSeq` 815, `List` 839, `Vector` 847, `Iterable` 934, `ArraySeq`/`IArray` (version specific) | All return an empty collection from `unsafeDecodeMissing` (e.g. JD:743, 775), but derived decoders only use that when `ExplicitEmptyCollections(decoding = false)` (default `decoding = true`, scala-2.x/JsonCodecConfiguration.scala:10) |
| Sets | `Set` 855, `HashSet` 863, `ListSet` 823, `TreeSet` 831, `SortedSet` 895 | Duplicate elements collapse silently |
| Maps | `Map` 871, `HashMap` 879, `mutable.Map` 887, `SortedMap` 903, `ListMap` 911, `keyValueChunk` 943 | Keys need a `JsonFieldDecoder`. Duplicate keys: no error, last value wins (builder semantics, JD:709-729); `keyValueChunk` and `Json.Obj` keep all duplicates in order (ast.scala:390-393). Error path for a bad key is `.theKey(Invalid Int: theKey)` (FD2:56). |
| Tuples | `tuple1`..`tuple22` (build.sbt:164-203) | JSON array of exactly N; AST error `Expected array of size N` |
| java.time | `DayOfWeek` 961, `Duration` 976, `Instant` 991, `LocalDate` 1006, `LocalDateTime` 1021, `LocalTime` 1036, `Month` 1051, `MonthDay` 1066, `OffsetDateTime` 1081, `OffsetTime` 1096, `Period` 1111, `Year` 1126, `YearMonth` 1141, `ZonedDateTime` 1156, `ZoneId` 1171, `ZoneOffset` 1190 | ISO-8601 strings. `DayOfWeek` and `Month` are case-insensitive (`"MonDay"` ok, LX:1748-1788, JavaTimeSpec.scala:259). On JS/Native `ZoneId`/`ZonedDateTime` need tzdb (index.md:31-37, correct). |
| UUID | `uuid` 1206 | Accepts the canonical 36-char form and also non-canonical "extended" forms with shorter or zero-padded sections, like `UUID.fromString` (LX:353-401; DecoderSpec.scala:523, 573) |
| Currency | `currency` 1222 | ISO 4217 code via `Currency.getInstance` |
| AST | `Json` ast.scala:590, `Json.Obj` 391, `Arr` 444, `Bool` 477, `Str` 501, `Num` 545, `Null` 567 | Instances live in the `Json` companions, not `JsonDecoder`; `Json.Num` decodes via the `BigDecimal` decoder (256-bit mantissa) |
| Fallback | `fromCodec` JD:1244 | Any `JsonCodec[A]` in scope provides `JsonDecoder[A]` (documented in json-codec.md) |

Other decoding behaviours, all undocumented:

1. Top-level trailing input is ignored (JD:89, 106; documented at json-decoder.md:45). The surprising result is not mentioned: `"1.5".fromJson[Int]` returns `Right(1)` and `"42abc".fromJson[Int]` returns `Right(42)`. The integer parser stops at the first non-digit and does not check what follows (UN:91-116, consume = false). Inside an object the same input fails, but with a syntax message that names no field: `{"age":1.5}` -> `(expected ',' or '}' got '.')` (LX:50-55). (code reading)
2. Unknown fields are skipped by `Lexer.skipValue`, which does not validate them (LX:126-176). `t`/`n` skips 3 chars and `f` skips 4 without checking them. Objects and arrays are skipped by counting brackets, and numbers by any run of `+-.eE0-9`. So `{"name":"Ada","age":1,"x":txyz}` decodes. (code reading)
3. Whitespace is only space, `\t`, `\r` and `\n` (RD:55-62). A UTF-8 BOM is not skipped and causes an error. (code reading)
4. There is no nesting-depth limit. Deep nesting ends in `StackOverflowError`, which `decodeJson` and `fromJsonAST` turn into `Unexpected structure` (JD:96, 261). The JVM stream readers do not catch it (PS:13-20).
5. Derived case-class decoders fail on duplicate keys with `(duplicate)`, at the object's path rather than the key's. Unknown keys give `(invalid extra field)` without the key name (scala-2.x/macros.scala:336-337; scala-3/macros.scala:887-898; DecoderSpec.scala:284).

---

### 3. Wrong or outdated statements (task 3)

| page:line | Claim | Truth + source | Corrected statement |
|---|---|---|---|
| how-to/decoding.md:158-162 | "Without any customization, decoding this input fails because the field names do not match the case class:" followed by an ```` ```scala mdoc:fail ```` block | `Quote` has no `JsonDecoder` in scope at that point (it is declared at :150 with no companion), so the block fails at **compile time** with "could not find implicit value ... JsonDecoder[Quote]", not at runtime because of field names | Derive a decoder first and show the runtime failure: `object Quote { implicit val decoder: JsonDecoder[Quote] = DeriveJsonDecoder.gen[Quote] }` then a plain `mdoc` block whose output is `Left(".symbol(missing)")`. Text: "With a derived decoder and no customization, decoding fails because the JSON keys do not match the field names:" |
| how-to/decoding.md:164 | "Each approach below makes this input decode to `Right(Quote(IBM,182.4300,182.8000))`." | Approach 3 (:224-240) calls `_._2.toString()`, and `Json#toString` renders JSON (ast/ast.scala:320), so each field keeps its quotes: `Quote("\"IBM\"", "\"182.4300\"", "\"182.8000\"")` | Either fix approach 3 to use `.flatMap(_._2.asString)` (ast.scala:44, 497), or change :164 to "Approaches 1, 2 and 4 decode this input to `Right(Quote(IBM,182.4300,182.8000))`; approach 3 keeps the JSON quotes in each value, as explained below." Preferred: fix the code, `.flatMap(_._2.asString)`, and drop the toString warning at :214-215 |
| how-to/decoding.md:123 | "Decoding fail because 'Pear' is not a valid value" | Example at :125 uses `"Peer"` | "Decoding fails because `Peer` is not a valid value", or change the example to `"Pear"` |
| how-to/decoding.md:59 | "On the JVM, malformed input decodes to the replacement character, as `new String(bytes, UTF_8)` does." | `Utf8ChunkReader` is shared code and "behaves the same on every platform" (RD:147-155) | "Malformed UTF-8 decodes to the replacement character U+FFFD on every platform, matching what `new String(bytes, UTF_8)` produces on the JVM." |
| how-to/decoding.md:61 | "use `JsonDecoder[A].decodeJsonStreamInput(stream)` to decode straight from a `ZStream[R, Throwable, Byte]`." | JVM only (js/native `JsonDecoderPlatformSpecific` are empty) | "On the JVM, use `JsonDecoder[A].decodeJsonStreamInput(stream)` ... See [Streaming](streaming.md)." |
| how-to/decoding.md:80 | "If you're using Scala 3 and your case class is defining default parameters, `-Yretain-trees` needs to be added" | **Needs verification, not proven wrong.** Since #1565 the Scala 3 derivation reads defaults by selecting the companion's `$lessinit$greater$default$N` methods (scala-3/macros.scala:281-294), not by reading trees. That normally does not need `-Yretain-trees`. The build still sets the flag (build.sbt:132), so tests cannot show whether it is still needed. | Check with a small Scala 3 project without the flag. If it works, delete the note here and in optional-and-default-fields.md:32 |
| reference/json-decoder.md:42 | "All three overloads return `Left(message)` on failure, never throw." | Only `UnsafeJson`, `UnexpectedEnd` and `StackOverflowError` are caught (JD:93-97, 110-114). An exception thrown by a user function in `map`/`zipWith`/`bothWith`, or any other runtime exception, propagates | "All three overloads return `Left(message)` for input that is malformed or does not match `A`. Exceptions thrown by your own functions, for example inside `map`, are not caught." |
| reference/json-decoder.md:231 | "decoding fails with a `missing case in peekChar` message" | Actual text is ``missing case in `peekChar` for 'x''`` (JD:290; note the backticks and the stray trailing quote) | "If no case matches, decoding fails with ``(missing case in `peekChar` for 'x'')``, where `x` is the character read." (Or fix the stray quote in the library first, see Bugs.) |
| reference/json-decoder.md:252-255 | "The default fails with `missing`. The `Option` decoder overrides it to return `None` ..." | Collection decoders also override it and return an empty collection (JD:743, 775, 785, 809, 817, 825, 833, 841, 849, 857, 865, 873, 881, 889, 897, 905, 913, 936, 945). Derived decoders ignore that unless `ExplicitEmptyCollections(decoding = false)` (scala-2.x/macros.scala:271-292; default `decoding = true`, scala-2.x/JsonCodecConfiguration.scala:10) | Add: "Collection decoders (`List`, `Map`, `Chunk`, ...) return an empty collection here, but derived decoders only use it when `explicitEmptyCollections` has `decoding = false`. By default a missing collection field is an error. `zip`, `both*` and `peekChar` do not forward this call." |
| reference/json-decoder.md:278 | "`JsonDecoder.derived` ... It needs a given `JsonCodecConfiguration`." | `derived[A: Mirror.Of](using config: JsonCodecConfiguration)` (scala-3/JsonDecoderVersionSpecific.scala:21). The companion provides `implicit val default` (scala-3/JsonCodecConfiguration.scala:158), so nothing has to be in scope | "It takes a `JsonCodecConfiguration`. The default one from the companion is used unless you put another given in scope." |
| reference/errors.md:41 | "Errors inside a sum type case show the case name as an ordinary `.Name` object key, because the default encoding wraps the case in an object." | True for the wrapper encoding only. With a discriminator field (`@jsonDiscriminator` / `DiscriminatorField`), the case name is pushed as a `Message` step (scala-2.x/macros.scala:642, 653; scala-3/macros.scala:1097), so it renders in parentheses: `(Banana).curvature(expected a Double)` | Append: "With a discriminator field, the case name is added as a message step instead, so the path reads `(Banana).curvature(expected a Double)`." |
| reference/errors.md:63 | "`decodeJsonPipeline` does the same for each element it fails on." | The pipeline fails the whole stream at the first element that does not decode (PS:122-125, 137-147). It does not go on to later elements. A value cut off by the end of input is not reported at all (see Bugs) | "`decodeJsonPipeline` fails the stream with the same exception at the first element that does not decode. Elements after it are not emitted." |
| reference/errors.md:77 | "`duplicate` — The same key appears twice in an object." | Only derived case-class decoders and the `Either` decoder check this (scala-2.x/macros.scala:336, 361; scala-3/macros.scala:887, 927, 934; JD:675, 678). `Map` decoders keep the last value without an error (JD:709-729). `keyValueChunk` and `Json.Obj` keep both entries. The message is attached to the object, not the key | "`duplicate` — A derived case class (or an `Either`) sees the same key twice. Shown at the object's path, without the key name. `Map` decoders do not check this: the last value wins." |
| reference/errors.md:82 | "`expected single field object` — Same sum type read from a `Json` AST that is not a one-key object." | Scala 2 only (scala-2.x/macros.scala:606, 633). Scala 3 says `expected object with a single field` (scala-3/macros.scala:1090). For a non-object, both say `expected object` (scala-3/macros.scala:1091) | "`expected single field object` (Scala 2) / `expected object with a single field` (Scala 3) — A wrapper-encoded sum type is read from a `Json.Obj` that does not have exactly one key." |
| reference/errors.md:83-84 | "`expected an Int` ... Same shape for `Long`, `Double`" / "`expected a Boolean`" | The text parser says `expected an Int` (LX:209), but `fromJsonAST` says `expected a Int` (JD:434). For Boolean the AST path says `expected boolean` (JD:323). BigInt/BigInteger/BigDecimal messages include the limit: `expected a 256-bit BigInt`, `expected a BigDecimal with 256-bit mantissa` (LX:228, 237, 264) | Add a note: "Decoding from a `Json` AST uses slightly different wording for some types (`expected a Int`, `expected boolean`). `BigInt`, `BigInteger` and `BigDecimal` name the 256-bit limit." |
| reference/errors.md:96 | "`missing`, `duplicate`, `invalid extra field` and the enumeration messages come from derived codecs." | `missing` is the default of every `JsonDecoder.unsafeDecodeMissing` (JD:239). `duplicate` also comes from `Either` (JD:675) | "`invalid extra field` and the enumeration messages come from derived codecs. `missing` comes from any decoder asked for an absent field, and `duplicate` also from the `Either` decoder." |
| reference/errors.md:52-53 | "`Lexer.error(msg, trace)` ... throws `JsonDecoder.UnsafeJson(trace)`" | It throws `UnsafeJson(JsonError.Message(msg) :: trace)` (LX:33-34) | "It throws `JsonDecoder.UnsafeJson` with `Message(msg)` added to the front of the trace." |
| explanation/how-it-works.md:26 | "All of them are internal and live in `zio.json.internal`. You reach them through `fromJson` ..." | `FastStringReader` and `Utf8ChunkReader` are `private[zio]` (RD:99, 156), but `WithRetractReader` is a public class (RD:319) and `RetractReader` is the public parameter type of `unsafeDecode` (RD:84; JD:245) | "`FastStringReader` and `Utf8ChunkReader` are private to the library. `RetractReader` is the public type that `unsafeDecode` receives, and `WithRetractReader` is public, so a hand-written decoder can be called directly on any `java.io.Reader`, for example in a test." |
| explanation/how-it-works.md:48 | "That is why every error looks like `.address.city(...)` and why you never see an exception from decoding." | The JVM streaming APIs fail with `java.lang.Exception` (PS:17-18, 122-123). User functions inside `map` can throw (JD:93-97) | "...and why `fromJson` returns a `Left` instead of throwing. The JVM streaming methods are the exception: they fail the effect with a `java.lang.Exception` whose message is the same rendered path." |
| explanation/security.md:97 | "uses a from-scratch number parser that will exit early when the number of bits of any number exceeds 256 bits." | The 256-bit cap applies to `BigInteger`/`BigInt` and to the mantissa of `BigDecimal`/`Float`/`Double` (LX:31, UN:151-310). The `BigDecimal` exponent is only bounded by `Int`, so `1e2147483647` decodes to a `BigDecimal` and to a `Json.Num`. `Int`/`Long` use overflow checks, not the bit cap | "...exits early when an integer, or the mantissa of a decimal, needs more than 256 bits (`Lexer.NumberMaxBits`). The decimal exponent is not capped, so a decoded `BigDecimal` such as `1e2147483647` is accepted: do not call `toBigInteger` on untrusted `BigDecimal` values." |
| explanation/security.md:31-33, 52 | Only AST-based libraries are exposed to hash-collision attacks | Derived case classes match keys with `StringMatrix` and do no hashing (LX:1816), which supports the claim. But decoding into `Map[String, V]` / `HashMap` / `mutable.Map` builds a hash map from attacker-chosen keys (JD:709-729, 871-893), so the same collision attack applies there | Add: "Decoding into a `Map[String, V]` builds a hash map from the payload's keys, so it is open to the same attack. Prefer a case class, or `JsonDecoder.keyValueChunk` / `Json.Obj`, which keep keys in a `Chunk` without hashing." |

Statements checked and found correct (no action): json-decoder.md:45, 81, 141, 155-156, 181-183, 204-206, 255 (the forwarding list), 289; errors.md:18-30 (render order verified at JsonError.scala:25-35), 40 (no `SumType` use anywhere in main code), 49-50, 58-62, 64 (lowercase `unexpected end of input`, PS:18), 85-91, 92; how-it-works.md:18-24, 34-35 (Magnolia; `genBoth` builds on both, scala-2.x/macros.scala:1058-1064), 46; validate-on-decode.md:44 (`transformOrFail(f: A => Either[String, B], g: B => A)`, JsonCodec.scala:70), 147 (message matches RefinedSpec.scala:16); index.md:31-37 (tzdb is Test-only in build.sbt:297, 314, so users must add it).

---

### 4. Missing examples / cross-links (task 4)

1. json-decoder.md has no example of a hand-written `unsafeDecode`, nor of overriding `unsafeFromJsonAST` or `unsafeDecodeMissing`. The "Missing Fields" section shows only a signature.
2. json-decoder.md "Missing Fields" should link to how-to/optional-and-default-fields.md, which has the user-facing table. Today it links only to annotations.md and json-codec-configuration.md.
3. errors.md "Common messages" has no row for many messages users hit:
   - `expected object` (AST path, both macros)
   - `expected 'null'` (field with a default given a value that starts with `n`, scala-2.x/macros.scala:335)
   - `missing hint 'type'` (discriminator, scala-2.x/macros.scala:657; scala-3/macros.scala:1130)
   - `Chunk was empty` (JD:805)
   - `Invalid Int: x` / `Invalid Long: x` for map keys (FD2:56, 64)
   - `expected single character string` (LX:1717)
   - `invalid charcode in string` (LX:1742)
   - `unexpected 'x'` (LX:135, ast.scala:603)
   - `Not an array` / `Not an object` / `Not a number` (AST decoders)
   - `Expected array of size N` (tuples)
   - `missing fields` / `ambiguous either, zip present` (JD:684-685)
   - ``missing case in `peekChar` ...``
4. errors.md has no example of an error inside a map value or key, e.g. `.someKey(expected an Int)`.
5. No page shows `Json#as[A]` or `fromJsonAST` failing with a path. errors.md:45 mentions `fromJsonAST` but has no example. Cross-link json-ast.md "Converting Between Types and the AST".
6. decoding.md "Decoding from bytes" should link to json-decoder.md#decodejson and to streaming.md. security.md "Larger Payload" (:14-16) should recommend `fromJson` on `Chunk[Byte]` / `Array[Byte]` and link there, since that is the library's answer to "frameworks consume the payload into a `String`".
7. decoding.md:128 ("Almost all of the standard library data types are supported") should link to a predefined-instances list once one exists.
8. validate-on-decode.md could show `mapOrFail` on a map-key decoder (`JsonFieldDecoder[String].mapOrFail`) for validated keys. interop/refined.md shows the refined key decoder exists (refined/package.scala:18).
9. decoding.md uses several H1 headings (`# Parsing custom JSON` :136, `# More custom decoder examples` :271, `# JSON AST and Cursors` :323) inside a page whose title is already an H1. Demote them to `##` and the sub-approaches to `###`.
10. how-it-works.md "Failing early" should link to errors.md for the format.
11. reference/index.md and how-to/index.md have no entry for map keys or low-level decoders, because those pages do not exist yet (see section 5).

---

### 5. Conceptual gaps (task 4)

1. **Map keys / `JsonFieldDecoder`**: no page. Users who decode `Map[LocalDate, V]`, `Map[MyEnum, V]` or `Map[Short, V]` get an implicit-not-found error and the docs never explain why. Needed:
   - which key types are built in (String, Int, Long, UUID, plus Scala 3 `T <: String`)
   - how to make one with `JsonFieldDecoder[String].mapOrFail(...)`
   - the encoder counterpart
   - that duplicate keys are not detected
2. **Low-level custom decoders**: no guide covers implementing `unsafeDecode` with `Lexer`/`RetractReader`. The pieces exist as public API but are undocumented:
   - `AbstractJsonDecoder`
   - `Lexer.char`, `firstField`/`nextField`, `firstArrayElement`/`nextArrayElement`, `string`, `skipValue`, `error`
   - `StringMatrix`
   - `WithRetractReader` for testing

   The guide should cover the reader contract (`retract` once after a read), the `trace` convention (prepend `ObjectAccess`/`ArrayAccess` before decoding children), and overriding `unsafeFromJsonAST` so `Json#as` does not re-encode and re-parse.
3. **Predefined instances**: no list of types, JSON shapes accepted, or leniencies. Leniencies to cover:
   - quoted numbers accepted
   - quoted NaN/Infinity accepted for Float/Double
   - leading zeros accepted
   - case-insensitive `DayOfWeek`/`Month`
   - non-canonical UUIDs accepted
   - `Unit` accepts anything
4. **Strictness model**: never explained. Trailing input is ignored, unknown fields are skipped without validation, and top-level `"1.5"` decodes as `Int` 1. Users who expect strict RFC 8259 validation should be told how to get it: `@jsonNoExtraFields`, or decode to `Json` first.
5. **`fromJsonAST` vs text decoding**: never explained. Some instances implement `unsafeFromJsonAST` natively (primitives, `Chunk`, `Option`, derived). Others (`List`, `Vector`, `Map`, `Either`, `Set`, `peekChar`, hand-written decoders) fall back to re-encoding to text (JD:247-248). This matters for performance, and for security (see Bugs #1).
6. **Security**: security.md does not discuss several attacks, nor the mitigations the library already has:
   - nesting depth: no limit, relies on stack overflow
   - huge strings: no limit
   - whitespace floods: LX:28-29 has a TODO admitting no early exit
   - unbounded `BigDecimal` exponent
   - `Map` hash collisions
   - the `Chunk[Byte]` decoding path as the answer to "frameworks read the payload into a String"
7. **index pages**: reference/index.md and how-to/index.md are adequate link lists. Neither mentions errors-for-streaming, map keys, or custom low-level decoders, because those pages are missing. index.md (landing) is fine.
8. **Platform matrix**: there is no single statement of what differs on JS/Native for decoding. Today the only differences are:
   - streaming APIs and file helpers are JVM only
   - `ZoneId`/`ZonedDateTime` need tzdb on JS/Native
   - JS has its own `UnsafeNumbers`/`SafeNumbers` (zio-json/js/src/main/scala/zio/json/internal/)

   A short "Platform notes" section in json-decoder.md would cover it.

---

### 6. Library bugs noticed (task 5)

| # | Severity | Location | Problem | Minimal repro idea |
|---|---|---|---|---|
| 1 | High (DoS) | JD:483 (`bigInteger.unsafeFromJsonAST`), JD:512 (`scalaBigInt.unsafeFromJsonAST`) | `n.value.toBigIntegerExact` on a `Json.Num` whose `BigDecimal` has a huge negative scale materialises 10^N. The streaming `BigDecimal`/`Json.Num` decoders accept such numbers because only the mantissa is capped (UN:279-310), so untrusted text -> `Json` -> `as[BigInt]` reproduces the `1e214748364` attack that security.md:80-86 says is prevented. `Int`/`Long` are safe because `longValueExact` bails out early. | `"1e2000000000".fromJson[Json].flatMap(_.as[BigInt])` (or `JsonDecoder[BigInt].fromJsonAST(Json.Num(new java.math.BigDecimal("1e2000000000")))`). Expect a long CPU spike or OOM instead of `Left("(expected a 256-bit BigInt)")`. Fix idea: check `n.value.precision - n.value.scale` (digits before the point) against the bit limit before calling `toBigIntegerExact`. (code reading) |
| 2 | Medium | PS:137-147 with PS:172 | `decodeJsonPipeline` swallows `UnexpectedEnd` whenever `ended` is true. `ended` is always set before `Take.end` reaches the reader, so a truncated last element (or a missing `]`) ends the stream successfully and the partial value is silently dropped. | `ZStream.fromIterable("""[{"a":1},{"a":""".toSeq).via(JsonDecoder[Map[String,Int]].decodeJsonPipeline()).runCollect`. Expected failure, actual `Chunk(Map(a -> 1))`. Same for `JsonStreamDelimiter.Newline` with `"1\n{\"a\":"`. (code reading) |
| 3 | Medium | LX:126-176 (`skipValue` and helpers) | Skipped (unknown) fields are not validated. `t`/`n`/`f` skip a fixed number of arbitrary chars, and numbers accept any `+-.eE` digit mix, so invalid JSON is accepted. | `"""{"name":"Ada","x":tzzz}""".fromJson[Name]` returns `Right(Name(Ada))`. Same with `"x":1-2e+` (code reading) |
| 4 | Low-Medium | FD3:83-87 (`stringLike`) | Re-wraps the already unescaped key as `"` + key + `"` without re-escaping it. A key containing `"` is truncated at the quote (trailing input ignored), a key with `\` is re-interpreted as an escape, and a key with a control character fails. | Scala 3: `opaque type Name <: String = String; given JsonDecoder[Name] = JsonDecoder.string.map(identity)`; `"""{"a\"b":1}""".fromJson[Map[Name, Int]]` gives `Map("a" -> 1)` instead of `Map("a\"b" -> 1)` (code reading) |
| 5 | Low | JD:290 | `peekChar` error text has a stray quote: ``missing case in `peekChar` for 'x''`` | `JsonDecoder.peekChar[Int]{ case '1' => JsonDecoder[Int] }.decodeJson("2")` |
| 6 | Low | JD:434 vs LX:209 | Inconsistent wording: AST path `expected a Int`, text path `expected an Int`. Similarly `expected boolean` (JD:323) vs `expected a Boolean` (LX:183) | `JsonDecoder[Int].fromJsonAST(Json.Str("x"))` |
| 7 | Low | FD2/FD3:52-66 | Key decoders use `String.toInt`/`toLong`, which accept `+5` and non-ASCII Unicode digits (`Character.digit`), while value decoders reject `+1` (DecoderSpec.scala:69) | `"""{"+5":1}""".fromJson[Map[Int,Int]]` gives `Right(Map(5 -> 1))`, while `"+5".fromJson[Int]` is a Left |
| 8 | Low | PS:13-20 | `readAll` (used by `decodeJsonStream`/`decodeJsonStreamInput`) does not catch `StackOverflowError`, unlike `decodeJson` (JD:96). Deeply nested streamed input becomes a fatal error instead of a normal failure. | `JsonDecoder[Json].decodeJsonStream(ZStream.fromIterable("[" * 100000))` |
| 9 | Low | JD:62-83 (`bothWith`), JD:283-292 (`peekChar`) | Neither forwards `unsafeDecodeMissing`, so an `Option`-typed decoder built with `both`/`peekChar` reports `missing` for an absent field instead of `None`. This may be intended, but it is inconsistent with `map`/`orElse`/`suspend`. | `final case class C(x: Option[Int])` with a field decoder `JsonDecoder.peekChar[Option[Int]]{ case _ => JsonDecoder[Option[Int]] }`, decoding `{}` |

Not verified, worth checking on Scala.js: `catch { case _: StackOverflowError => ... }` (JD:96). On Scala.js a JS stack overflow surfaces as a `RangeError`, which may not map to `StackOverflowError`. If it does not, `Unexpected structure` (errors.md:50, 94) is JVM/Native only.

## Appendix C: derivation and configuration

> Runtime check: L5 below and every mention of "Scala 3 reads only the first `@jsonAliases`" are wrong. Two `@jsonAliases` on one field work on Scala 3.9.0. `-Yretain-trees` is confirmed unnecessary on Scala 3.


Source: `series/2.x` at `d59b926ea40`.
Source paths below are abbreviated:

- `S2/macros` = `zio-json/shared/src/main/scala-2.x/zio/json/macros.scala`
- `S3/macros` = `zio-json/shared/src/main/scala-3/zio/json/macros.scala`
- `S2/Config`, `S3/Config` = `.../scala-2.x|scala-3/zio/json/JsonCodecConfiguration.scala`
- `jsonDerive` = `zio-json-macros/shared/src/main/scala/zio/json/jsonDerive.scala`
- `S3/union` = `zio-json/shared/src/main/scala-3/zio/json/union_derivation.scala`

Build facts used throughout:

- `zio-json` cross-builds Scala 2.12.21, 2.13.18 and 3.9.0 (`project/BuildHelper.scala:18-20,208`) on JVM, JS and Native (`build.sbt:121`).
- Scala 2 derivation uses Magnolia 1.1.14 (`build.sbt:151-157`). Scala 3 uses native macros since #1565 (commit 9e16384f, 2026-09-01); Magnolia is gone on Scala 3.
- `zio-json-macros` is JVM/JS/Native, Scala 2 only (`build.sbt:361-368`, `crossScalaVersions -= Scala3`). It needs `-Ymacro-annotations` on 2.13 and the paradise 2.1.1 plugin on 2.12 (`project/BuildHelper.scala:251-264`).
- The docs project is compiled by mdoc with Scala 2.13 only (`build.sbt:466-468`), so every Scala 3 snippet in the docs is unchecked plain `scala`.

### Inventory (task 1)

#### Annotations (all in package `zio.json`; same set on Scala 2 and 3)

| Symbol | Source | Status |
|---|---|---|
| `jsonField(name)` | S2/macros:15, S3/macros:21 | Documented: annotations.md:16, field-names.md §1 |
| `jsonAliases(alias, aliases*)` | S2/macros:20, S3/macros:26 | Documented: annotations.md:17, field-names.md §5 (Scala 3 difference not mentioned, see Partial) |
| `jsonExplicitNull` | S2/macros:25 (`final class`), S3/macros:183 (`case class jsonExplicitNull()`) | Documented: annotations.md:23, optional-and-default-fields.md §2 |
| `jsonExplicitEmptyCollections(encoding, decoding)` | S2/macros:30, S3/macros:188 | Documented: annotations.md:24, optional §3, config page (one wrong statement, see below) |
| `jsonDiscriminator(name)` | S2/macros:42, S3/macros:38 | Documented: annotations.md:21, sealed-traits.md §3 |
| `jsonMemberNames(format)` | S2/macros:88, S3/macros:82 | Documented: annotations.md:18, field-names.md §2 |
| `jsonHint(name)` | S2/macros:174, S3/macros:172 | Documented: annotations.md:19, sealed-traits.md §1 |
| `jsonHintNames(format)` | S2/macros:180, S3/macros:178 | Documented: annotations.md:20, sealed-traits.md §2 |
| `jsonNoExtraFields` | S2/macros:192, S3/macros:200 | Documented: annotations.md:22, optional §4 |
| `jsonExclude` | S2/macros:197, S3/macros:205 | Documented: annotations.md:25, optional §5 (decoder still reads it: correct) |
| `jsonDerive(config)` (zio-json-macros) | jsonDerive:22-26 | Documented: annotations.md §jsonDerive (partial) |

#### JsonMemberFormat

`sealed trait JsonMemberFormat extends (String => String)` (S2/macros:49, S3/macros:45). Cases: `CustomCase(f)`, `SnakeCase`, `CamelCase`, `PascalCase`, `KebabCase`, `IdentityFormat`, plus `ziojson_03.SnakeCase` / `ziojson_03.KebabCase` (S2/macros:51-82). All documented (field-names.md table, annotations.md "Member formats", config page). There is no `ScreamingSnakeCase` or any other format in the source, so none is missing.

Checked the examples: `ziojson_03.SnakeCase("abc123Def") == "abc_123_def"` and the current `SnakeCase("abc123Def") == "abc123_def"`, worked out by hand from S2/macros:122-166. The claim at annotations.md:34 is correct.

#### JsonCodecConfiguration (S2/Config:30-38, S3/Config same file with the defaults changed)

| Field | Default S2 | Default S3 | Doc status |
|---|---|---|---|
| `sumTypeHandling` | `WrapperWithClassNameField` | same | Documented |
| `fieldNameMapping` | `IdentityFormat` | same | Documented |
| `allowExtraFields` | `true` | same | Documented |
| `sumTypeMapping` | `IdentityFormat` | same | Documented |
| `explicitNulls` | `false` | same | Documented |
| `explicitEmptyCollections` | `ExplicitEmptyCollections()` (true,true) | same | Documented |
| `enumValuesAsStrings` | `false` (S2/Config:37) | `true` (S3/Config:37) | Documented, defaults correct |
| `SumTypeHandling.WrapperWithClassNameField` / `DiscriminatorField(name)` | S2/Config:169,184 | same | Documented |
| `SumTypeHandling#discriminatorField: Option[String]` | S2/Config:161 | same | Not documented (Low) |
| Overloaded 5/6-arg `apply`, constructors, `copy` | S2/Config:39-121,125-156 | same | Not documented (binary-compat only, Low; no doc needed) |
| `implicit val default` | S2/Config:158 | same | Documented (config page:35) |

#### Derivation entry points

| Entry point | Scala 2 | Scala 3 | Doc status |
|---|---|---|---|
| `DeriveJsonEncoder.gen[A]` | macro (S2/macros:893) | `inline def gen[A](using config = default)(using Mirror.Of[A])` (S3/macros:1475) | Documented |
| `DeriveJsonDecoder.gen[A]` | S2/macros:722 | S3/macros:1386 | Documented |
| `DeriveJsonCodec.gen[A]` | whitebox macro `JsonCodec(Encoder.gen, Decoder.gen)` (S2/macros:1053-1065) | own implementation that shares one metadata pass (S3/macros:1713-1854) | Documented |
| `JsonCodec.derived`, `JsonEncoder.derived`, `JsonDecoder.derived` (`derives`) | n/a | `JsonCodecVersionSpecific.scala:9`, `JsonEncoderVersionSpecific.scala:23`, `JsonDecoderVersionSpecific.scala:21` | Documented (json-codec.md:37,173; json-encoder.md:214; json-decoder.md:278) but "needs a given" is misleading, see Wrong |
| Scala 3 `enum` derivation (parameterless cases → strings; mixed → wrapper objects) | n/a | S3/macros:367-373,1453 | Partial (see below) |
| String-literal union types `"A" \| "B"` | n/a | `JsonEncoderVersionSpecific.scala:31-34`, `JsonDecoderVersionSpecific.scala:29-36`, S3/union | Partial: decoding.md:112-126, with a broken example |
| Union literal types as Map keys | n/a | `scala-3/JsonFieldDecoder.scala:83` (`stringLike[T <: String]`) | Not documented |
| Opaque types | no special support in source | no special support in source | Not mentioned anywhere in docs |
| `DeriveJsonDecoder.join/split`, `Typeclass` (Magnolia plumbing, public) | S2/macros:219-221,520,734-736,806 | n/a | Not documented (Low; internal) |
| `DeriveJsonEncoder.NestedWriter` (public by accident) | `private[this]` (S2/macros:904) | public `final class` (S3/macros:1563) | Not documented; should be private, not documented |

#### zio-json-macros public API

- `class jsonDerive(val config: JsonDeriveConfig = JsonDeriveConfig.Codec)` (jsonDerive:22-26).
- `sealed abstract class JsonDeriveConfig` with `Codec`, `Encoder`, `Decoder` (jsonDerive:9-20).
- `JsonCodecStyle` (`DefaultJsonCodec`, `SnakeCaseJsonCodec`, `KebabCaseJsonCodec`) and `JsonCodecType` (`Codec`, `DecodeOnly`, `EncodeOnly`) are `private` (jsonDerive:161-173). `snakeCaseJsonDerive` and `kebabCaseJsonDerive` are commented out (jsonDerive:28-39). They are not public API, and the docs correctly leave them out.
- Generated members: `implicit val codecFor<T>`, `encode<T>`, `decode<T>`. For a generic class it generates `implicit def` with an implicit `JsonEncoder`/`JsonDecoder` per type parameter (jsonDerive:115-158).

### Undocumented API

| Symbol / behaviour | Source file:line | Priority | Suggested home |
|---|---|---|---|
| Nested types and subtypes without an instance are derived automatically (recursive derivation), using the outer configuration | S3/macros:645-714 (leaf subtypes), 733-789 (product fields, fallback to `DeriveJsonX.gen` via `Mirror`); Scala 2 via Magnolia `gen` (the sealed-traits.md §1 mdoc example derives `Fruit` with no subtype codecs and compiles on 2.13) | High | tutorial §4/§6 + how-it-works.md "No reflection at runtime" + json-codec-configuration.md "Passing a custom configuration" |
| Scala 3 string-literal union types as Map keys, and single literal types (e.g. `Option["A"]`) | scala-3/JsonFieldDecoder.scala:83; S3/union:30-36; test `DecoderVersionSpecificSpec.scala:193-203`, `CodecVersionSpecificSpec.scala:37-40` | Medium | new "Scala 3 derivation" how-to, or fix decoding.md §String-based union types |
| Scala 3 sealed hierarchies are flattened to leaf case classes/objects: intermediate sealed traits get no hint of their own | S3/macros:351-357, 504-510, 653-657; test `DecoderVersionSpecificSpec.scala:250-257`. Scala 2 behaviour depends on Magnolia, not verified | Medium | sealed-traits.md (new section "Nested sealed traits") |
| Derivation of generic (type-parameterised) case classes and ADTs: `implicit def codec[A: JsonEncoder: JsonDecoder]: JsonCodec[Box[A]] = DeriveJsonCodec.gen[Box[A]]` | S3/macros:629-642 (`resolveLeafType`); test `DecoderVersionSpecificSpec.scala:259-275` | Medium | new how-to "Derive codecs for generic types" or a section of encoding/decoding.md |
| Duplicate case names (after `@jsonHint`/`@jsonHintNames`) throw `AssertionError("Case names in ADT ... must be distinct")` at runtime when the decoder is built | S2/macros:526-530; S3/macros:1020-1026 | Medium | sealed-traits.md (a "Keep in mind" list like field-names.md:157-161) |
| `@jsonDerive` generated member names `codecFor<T>`, `encode<T>`, `decode<T>`, and `implicit def` with implicit params for generic classes | jsonDerive:118-151 | Medium | annotations.md §jsonDerive (or a new zio-json-macros page) |
| `@jsonDerive` with an unrecognised `config` expression emits a compiler warning and falls back to generating a full codec | jsonDerive:107-112 | Low | annotations.md §jsonDerive |
| Scala 3 compile error text when no instance can be found or derived: `Cannot find or derive JsonEncoder for T` | S3/macros:672, 708, 754, 783 | Low | errors.md or a Scala 3 page |
| `SumTypeHandling#discriminatorField` | S2/Config:160-162 | Low | json-codec-configuration.md §sumTypeHandling (one line) |
| A parameterless case class or case object decoder accepts any JSON value (it skips the value) unless `@jsonNoExtraFields` is set | S2/macros:199-216; S3/macros:981-1005; canary test `zio-json-macros/.../DeriveSpec.scala:11-18` | Low | optional-and-default-fields.md §4 note |
| Discriminator decoding: the hint field may appear anywhere in the object; the encoder always writes it first | S2/macros:644-659, 884; S3/macros:1102-1131, 1335 | Low | sealed-traits.md §3 |
| `DeriveJsonEncoder.NestedWriter` public on Scala 3 | S3/macros:1563 | Low | Do not document. Make it `private[json]` (see Library bugs) |

### Partially documented

1. **`@jsonDerive` (annotations.md:36-113).**
   - The "roughly what you would write" examples use the member names `codec`/`encoder`/`decoder`. The real names are `codecForWatermelon`, `encodeMelon`, `decodeGourd` (jsonDerive:118-120). This matters for name clashes and for referring to the instance explicitly.
   - Line 113 says generic classes work but does not say that the generated member is an `implicit def` that needs `JsonEncoder[A]` and `JsonDecoder[A]` for every type parameter (both for `Codec`).
   - There is no sbt line for the 2.12 paradise plugin: `addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)` (BuildHelper.scala:261).
   - No `%%%` note for Scala.js/Native, although the module is published for both.
2. **`enumValuesAsStrings` (json-codec-configuration.md:215-244, sealed-traits.md §5).** What counts as an "enumeration" differs by Scala version, and the docs only describe "all case objects":
   - Scala 2 counts every subtype whose derived instance is the case-object instance. That includes zero-parameter case classes `case class A()` (S2/macros:227, 540-541, 737, 816-817). It excludes case objects that have a hand-written instance in scope.
   - Scala 3 counts only modules and parameterless enum cases (S3/macros:367-373, 1453, 1543). It ignores any hand-written instance of a case object.
   - A Scala 3 enum that mixes parameterless and parameterised cases writes the parameterless ones as `{"Bar":{}}`. The test `DecoderVersionSpecificSpec.scala:~190` shows `{"Qux":{"foo":{"Bar":{}}}}`. sealed-traits.md:190 only implies this.
3. **String-literal unions (decoding.md:112-126).** The section exists but the example is broken (see Wrong). It does not say that only `String` literal unions are supported (S3/union:30-36, `IsUnionOf[String, T]`), that the error is `expected one of: A, B` (only errors.md:92 has that), or that unions work as Map keys.
4. **`@jsonAliases` on Scala 3.** Only the first `@jsonAliases` annotation on a field is read (S3/macros:1410-1412, 1741-1743, `collectFirst`). Scala 2 reads all of them (S2/macros:237-240, `flatMap`). field-names.md §5 is silent on this.
5. **Default values and `null` (optional-and-default-fields.md:32).** The page states the rule but not the consequence: an `Option` field whose default is not `None` never round-trips `None`. See Library bugs L2.
6. **`explicitEmptyCollections.decoding` scope.** The source treats every `CollectionJsonDecoder` as a "collection". That includes derived case-class decoders and parameterless/case-object decoders (S2/macros:199-200, 260, 288-293; S3/macros:846, 853-859, 985). With `decoding = false`, a missing field whose type is a derived case class decodes from that class's own defaults/`None`/empties. With `decoding = true` (the default), it fails with `missing`. Neither optional-and-default-fields.md §1 nor the config page say that nested case classes are affected. `NonEmptyChunk` is not a `CollectionJsonDecoder`, so its missing field is always `missing`.
7. **`@jsonDiscriminator` + strict decoding.** sealed-traits.md:142 and optional:128 warn about `@jsonNoExtraFields`. They do not warn that `JsonCodecConfiguration(allowExtraFields = false)` together with `DiscriminatorField` (or `@jsonDiscriminator`) breaks decoding of every case class in the family in the same way (S3/macros:1118-1121 passes the whole object, hint included, to the case decoder; that decoder rejects the hint at 896-899; Scala 2: S2/macros:652-653 and 337).
8. **Scala 2 vs Scala 3 behaviour differences.** The config page's description (line 4) promises "the Scala 2 and Scala 3 differences", but line 30 says the only difference is the `enumValuesAsStrings` default. Behavioural differences exist: items 2 and 4 above, plus Library bugs L4.
9. **Configuration propagation (json-codec-configuration.md:80).** "Nested types are derived with the configuration in scope at the outer gen call" is correct, but it applies only to nested types that have no instance of their own. The page should say that an existing implicit/given instance is used as-is, with the configuration it was derived with (S3/macros:746-755, 775-784).
10. **Field-level `@jsonExplicitEmptyCollections`.** annotations.md:24 and optional:106 correctly say a field annotation affects encoding only. The config page contradicts them (see Wrong W4).

### Wrong or outdated statements

| # | Page:line | Claim | Truth + source | Corrected statement |
|---|---|---|---|---|
| W1 | tutorial/getting-started.mdx:125 | "Derive the codec on the trait itself, and the subtypes need codecs too." | Subtypes without an instance are derived automatically. Scala 3: S3/macros:703-709 (`summonLeafDecoders` falls back to `DeriveJsonDecoder.gen` via `Mirror`), same for encoders at 667-673. Scala 2: Magnolia derives missing subtype instances; sealed-traits.md:32-40 and json-codec-configuration.md:97-104 derive a family with no subtype codecs and compile under mdoc. | "Derive the codec on the trait itself. Subtypes that have no codec of their own are derived along with it, so you only need a separate subtype codec when you also encode or decode that subtype on its own." (Then make lines 134-135 optional, or drop them.) |
| W2 | tutorial/getting-started.mdx:24 | "ZIO JSON works on Scala 2.13 and Scala 3." | Also Scala 2.12, on JVM, Scala.js and Scala Native (project/BuildHelper.scala:18-20,208; build.sbt:121). | "ZIO JSON works on Scala 2.12, 2.13 and 3, on the JVM, Scala.js and Scala Native." |
| W3 | reference/annotations.md:16-25 (How-to column) | Links written as `[Field names](../how-to/field-names.md)#1-rename-one-field-with-jsonfield` | The fragment sits outside the link parentheses, so it renders as literal text after the link and the link goes to the top of the page. The anchors themselves exist. | Move each fragment inside the parentheses, e.g. `[Field names](../how-to/field-names.md#1-rename-one-field-with-jsonfield)`. Same for all 10 rows. |
| W4 | reference/json-codec-configuration.md:196 | "`@jsonExplicitEmptyCollections(encoding, decoding)` sets it per type or per field." | The decoder reads the annotation from the class only (S2/macros:267-270; S3/macros:849-851). Field annotations affect encoding only (S2/macros:752-754; S3/macros:1188-1189). | "`@jsonExplicitEmptyCollections(encoding, decoding)` sets it per type. On a single field it changes encoding only; the `decoding` flag of a field annotation is ignored." |
| W5 | reference/json-codec-configuration.md:217 | "It applies to a sealed trait whose cases are all case objects." | Scala 2 also treats zero-parameter case classes as enum values and skips the string form if any case object has a hand-written instance (S2/macros:540-541, 816-817). Scala 3 requires case objects or parameterless enum cases (S3/macros:367-373). | "It applies to a sealed trait whose cases all have no fields. On Scala 3 that means case objects and parameterless enum cases. On Scala 2 it also includes case classes with an empty parameter list, and it is skipped if any case has its own hand-written codec." |
| W6 | how-to/optional-and-default-fields.md:32 and how-to/decoding.md:80 | "On Scala 3, defaults need `-Yretain-trees` in `scalacOptions`." | Likely outdated. The requirement came from Magnolia for Scala 3, which was removed in #1565 (commit 9e16384f). The native macro reads defaults by selecting the companion's `$lessinit$greater$default$N` methods (S3/macros:275-296, 434-462) and never calls `.tree`. Needs one confirmation compile of a Scala 3 project without the flag before removal. | After confirming: delete the sentence/note. If it is still needed, keep it and explain why (it is not needed by the macro code shown). |
| W7 | how-to/decoding.md:121, 123, 125 | `"Apple".fromJson["Apple" \| "Banana"]` "succeeds"; "'Pear'" vs `"Peer"` | The Scala string `"Apple"` holds `Apple` without JSON quotes, which is not valid JSON, so decoding fails. The prose says "Pear" while the code says "Peer". | `""""Apple"""".fromJson["Apple" \| "Banana"]  // Right(Apple)` and `""""Pear"""".fromJson["Apple" \| "Banana"]  // Left((expected one of: Apple, Banana))` |
| W8 | reference/json-codec.md:173, reference/json-decoder.md:278, reference/json-encoder.md:215 (outside the slice but about derivation) | "`JsonCodec.derived` ... needs a given `JsonCodecConfiguration`" | The companion always provides `implicit val default` (S2/Config:158, S3/Config:158), so `derives` works with no user-provided given. | "It takes a `JsonCodecConfiguration` as a `using` parameter. Without a given of your own, `JsonCodecConfiguration.default` is used." |
| W9 | how-to/field-names.md:161 | "deriving the decoder fails with an `AssertionError`" | True, but at runtime when the codec value is initialised, not at compile time (S2/macros:246-256 inside `join`; S3/macros:821-830 inside `buildProductDecoder`). | "...the decoder throws an `AssertionError` at runtime when the codec is first initialised (it compiles fine)." |
| W10 | explanation/how-it-works.md:41 | "Missing codecs are compile errors, not runtime surprises." | Nested case classes and subtypes without codecs are derived, not reported (see W1). Name and hint collisions surface as runtime `AssertionError`s (S2/macros:253, 529; S3/macros:829, 1025). | "A type that has no codec and cannot be derived is a compile error. Nested case classes and sealed subtypes without a codec are derived automatically. Duplicate field names, aliases or case names are reported when the codec is first initialised." |
| W11 | reference/json-codec-configuration.md:30 | "The only difference between the two Scala versions is the default of `enumValuesAsStrings`." | True for defaults only. Behaviour differs (Partial items 2, 4; Library bugs L4). | "The only difference in defaults is `enumValuesAsStrings`. A few behaviours also differ; see [Scala 2 vs Scala 3](#...)." (and add that section) |
| W12 | how-to/optional-and-default-fields.md:61 | "Missing nulls are always accepted when decoding." | Wording is unclear; the intended fact is correct (decoders ignore `explicitNulls`). | "When decoding, an `Option` field accepts both `null` and a missing key, whether or not the annotation is present." |
| W13 | how-to/optional-and-default-fields.md:40 | Explains that a companion codec for a class with defaults "fails with a `StackOverflowError`" inside the mdoc wrapper | Not verifiable without running mdoc, and it is build trivia on a user-facing page. | Remove the paragraph. Use `mdoc:reset-object` or a top-level layout so the example can use the normal companion pattern. |
| W14 | index.md:31-33 | "For cross-platform projects with Scala.js and Scala Native need to replace `%%` operator by `%%%`, and optionally ... need to add" | Grammar. | "For Scala.js and Scala Native, use `%%%` instead of `%%`. If you use `java.time.ZoneId` or `java.time.ZonedDateTime`, also add the time-zone database:" |

Checked and found correct (no action): annotations.md:16-26 (semantics), 34, 41, 43; json-codec-configuration.md:20-29 (every default), 67, 86-90, 113, 147, 163, 171, 192-195, 220-221; field-names.md:58, 126, 139 (`jsonAliases` arrived in v0.5.0, commit 172a788d, #866), 159-160; sealed-traits.md:14, 122, 142, 168, 171, 183-190; optional-and-default-fields.md:25-30, 88, 106, 128, 148; how-it-works.md:32-37; getting-started.mdx:60, 115, 152-160.

### Missing examples / cross-links

- **tutorial/getting-started.mdx §8 "Where next"** does not link to [Optional, default and null fields](../how-to/optional-and-default-fields.md) or to [JsonCodecConfiguration](../reference/json-codec-configuration.md), although §5 introduces `Option`/`List` behaviour.
- **sealed-traits.md**:
  - no Scala 3 `enum` example (plain `scala` block): pure enum → strings, mixed enum → objects, `@jsonHintNames` on an enum (pattern in `DecoderVersionSpecificSpec.scala:91-99`);
  - no nested sealed hierarchy example;
  - no hint-collision note;
  - §5 should link to json-codec-configuration.md#enumvaluesasstrings.
- **json-codec-configuration.md** §enumValuesAsStrings should link back to sealed-traits.md#5-write-case-objects-as-strings. Every option section could link to its annotation in annotations.md.
- **optional-and-default-fields.md**:
  - add an example of `Option[A]` with a non-`None` default and what `None` round-trips to;
  - add an example of a nested case-class field with `decoding = false`.
- **annotations.md §jsonDerive**:
  - add a generic-class example and show the real generated names;
  - add the 2.12 paradise sbt line;
  - the `@jsonDerive` row could link to a dedicated module page if one is created.
- **how-it-works.md** "No reflection at runtime" should link to json-codec-configuration.md and annotations.md.
- **decoding.md union section** should link to errors.md:92 (`expected one of`).
- **index.md** has no list of modules (zio-json-macros, zio-json-yaml, zio-json-golden, interop modules) and no supported Scala versions/platforms.
- **reference/index.md** "Data and modules" lists YAML but not zio-json-macros.

### Conceptual gaps

1. **No Scala 3 derivation guide.** Only scattered one-liners cover `derives`, `given` configuration, enums (pure vs mixed), string-literal unions, generic ADTs, leaf flattening of nested sealed traits and the `Cannot find or derive` error. None of these Scala 3 snippets are compiled, because mdoc runs on 2.13 only.
2. **No Scala 2 → Scala 3 migration notes.** Key items:
   - the `enumValuesAsStrings` default flips, which changes the wire format of case-object families;
   - `@jsonDerive` is not available, use `derives`;
   - zero-parameter case classes stop counting as enum values;
   - multiple `@jsonAliases` on one field: only the first is read;
   - Magnolia was replaced by native macros;
   - the status of `-Yretain-trees`.
3. **No page for the zio-json-macros module.** It exists only as a section inside the annotations reference. The module is not mentioned in index.md or reference/index.md.
4. **Automatic recursive derivation is not explained anywhere.** The tutorial says the opposite (W1). Readers need to know that nested types are re-derived at each use site with the outer configuration, and that an existing instance is used as-is.
5. **No wire-format overview.** No single table shows how each value kind is written under each strategy: case class, case object, empty case class, Scala 3 enum case × wrapper / discriminator / `enumValuesAsStrings`.
6. **No guidance on round-trip safety.** Defaults + `None`, `encoding=false` + `decoding=true`, and discriminator + strict decoding can all produce JSON that the same codec will not read back to the same value.
7. **Opaque types / newtypes.** There is no derivation support in source and no mention in docs. A pointer from the derivation pages to manual-instances.md (`map`/`contramap`/`transform`) would close it.
8. **Only one tutorial.** There is no learning path for configuration or for sealed-trait modelling.

### Library bugs noticed

| # | Bug | Source | Minimal repro idea |
|---|---|---|---|
| L1 | Scaladoc of `ExplicitEmptyCollections` has the two directions swapped: "When disabled for decoding, keys with empty collections will be omitted from the JSON. When disabled for encoding, missing keys will default to empty collections." The code does the opposite (encoding=false omits, decoding=false fills in empties). | S2/Config:6-9, S3/Config:6-9; behaviour at S2/macros:742-754, 267-293 | Read the scaladoc, then run `ExplicitEmptyCollections(encoding = false)` on `case class B(xs: List[Int])`: `B(Nil).toJson == "{}"` |
| L2 | `Option` fields with a non-`None` default do not round-trip `None`, even with the default configuration. The encoder omits `None` (FieldEncoder flag 1 skips `isNothing`); the decoder fills a missing key, or an explicit `null`, with the default. With `explicitNulls = true` the encoder writes `null`, and the decoder still replaces it with the default. | FieldEncoder.scala:14-17,36-41; JsonEncoder.scala:306; S2/macros:326-335, 341-348; S3/macros:888-893, 904-911 | `case class A(o: Option[Int] = Some(1))`; `A(None).toJson.fromJson[A]` returns `Right(A(Some(1)))` (expected `A(None)`). This may be intended design; at minimum it needs a doc warning. |
| L3 | Enum-string detection differs between Scala 2 and 3: the same source gives different JSON. | S2/macros:540-541, 816-817 vs S3/macros:367-373, 1453, 1543 | `sealed trait T; case class A() extends T; case class B() extends T` with `enumValuesAsStrings = true`: Scala 2 writes `"A"`, Scala 3 writes `{"A":{}}`. Also: a case object with its own `JsonEncoder` in scope is honoured on Scala 2 but ignored on Scala 3 when the family is string-encoded. |
| L4 | The Scala 3 empty-product encoder does not override `isEmpty = true`, while Scala 2's case-object encoder does. With `explicitEmptyCollections.encoding = false`, a field holding a case object or empty case class is dropped on Scala 2 but written as `{}` on Scala 3. | S2/macros:726-732 vs S3/macros:1242-1249 | `@jsonExplicitEmptyCollections(encoding = false) case class W(e: Empty); case class Empty()`; `W(Empty()).toJson` gives `{}` on 2 and `{"e":{}}` on 3. On Scala 2 the output then fails to decode with `.e(missing)` unless decoding=false too. |
| L5 | Scala 3 reads only the first `@jsonAliases` on a field; Scala 2 merges all of them. | S3/macros:1410-1412, 1741-1743 vs S2/macros:237-240 | `case class C(@jsonAliases("a") @jsonAliases("b") x: Int)`; `{"b":1}` decodes on 2, fails on 3. |
| L6 | `DeriveJsonEncoder.NestedWriter` is public API on Scala 3 (a `private[this]` top-level class on Scala 2). It leaks an internal writer into the public surface and MiMa. | S3/macros:1561-1563 vs S2/macros:904 | Inspect the API: `zio.json.DeriveJsonEncoder.NestedWriter` is accessible from user code on Scala 3. |
| L7 | zio-json-macros dead code: `codecStyle` is computed but never used, and the `snakeCaseJsonDerive`/`kebabCaseJsonDerive` branches match annotations that are commented out. Suspected (not compiled): type parameters are copied verbatim, variance included, into `implicit def`s, which may not compile for `@jsonDerive case class Box[+A](a: A)`. | jsonDerive:28-39, 83-89, 129-150 | Compile `@jsonDerive case class Box[+A](a: A)` on 2.13 with `-Ymacro-annotations`; expect a "variance annotation not allowed" style error if the suspicion holds. |
| L8 | Design limitation, unchecked at derivation time: the discriminator form passes the whole object, hint field included, to the case decoder. `allowExtraFields = false` in the configuration therefore breaks every discriminated family derived under it. Nothing warns at compile time. | S3/macros:1118-1121 + 896-899; S2/macros:652-653 + 337 | `implicit val c = JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("type"), allowExtraFields = false)`; derive `Shape`; `{"type":"Circle","radius":1}` gives `Left((invalid extra field))`. |

### Counts

- Undocumented API rows: 12 (1 High, 5 Medium, 6 Low)
- Partially documented: 10
- Wrong or outdated statements: 14 (W1-W14; W6 needs one confirming Scala 3 compile)
- Library bugs / inconsistencies: 8 (L1-L8; L7 partly suspected)

## Appendix D: AST and modules

> Bug 6 below (`Json.Obj` equality with duplicate keys) is the subject of open PR #1694, so it is pending upstream rather than new.


Source: `series/2.x` at `d59b926ea40`.
All source paths are relative to the repo root. Read-only audit: nothing compiled or run. Every item below was checked in source.
Where a SnakeYAML behavior is cited, it was checked in `snakeyaml-2.0-sources.jar` from the local coursier cache (the module uses 2.7, build.sbt:350).

### Platform and install matrix (from build.sbt + project/BuildHelper.scala)

`stdSettings` sets `crossScalaVersions := Seq(2.12.21, 2.13.18, 3.9.0)` (project/BuildHelper.scala:18-20, 208).

| Module | Artifact | Kind (build.sbt line) | Platforms | Scala | Docs install snippet |
|---|---|---|---|---|---|
| core | `zio-json` | crossProject (121) | JVM, JS, Native | 2.12, 2.13, 3 | index.md:127 `%%` + note on `%%%` — OK |
| YAML | `zio-json-yaml` | project (344) | JVM only | 2.12, 2.13, 3 | yaml.md:17 `%%` — OK, "JVM only" (yaml.md:10) — OK |
| golden | `zio-json-golden` | project (328) | JVM only | 2.12, 2.13, 3 | golden-testing.md:14 `%% ... % Test` — OK |
| http4s | `zio-json-interop-http4s` | project (385), http4s 0.23.38, CE 3.7.0 | JVM only | 2.12, 2.13, 3 | http4s.md:12 `%%` — OK; page never says JVM-only |
| refined | `zio-json-interop-refined` | crossProject (403), refined 0.11.4 | JVM, JS, Native | 2.12, 2.13, 3 | refined.md:9 `%%` only; no `%%%` note |
| scalaz7x | `zio-json-interop-scalaz7x` | crossProject (418), scalaz 7.3.9, `crossScalaVersions -= Scala3` (424) | JVM, JS, Native | 2.12, 2.13 only | scalaz-7x.md:9 `%%`; page never says "no Scala 3" |
| enumeratum | `zio-json-interop-enumeratum` | crossProject (434), enumeratum 1.9.8 | JVM, JS, Native | 2.12, 2.13, 3 (`-Yretain-trees` on Scala 3, line 441) | enumeratum.md:21 `%%`; no `%%%` note, no Scala 3 flag note |
| macros | `zio-json-macros` | crossProject (361), `-= Scala3` | JVM, JS, Native | 2.12, 2.13 | out of slice |

No page (index.md, interop/index.md) shows this matrix. index.md:122-136 only covers core.

### Undocumented API

| Symbol | Source file:line | Priority | Suggested home |
|---|---|---|---|
| `Json.Obj` field operations: `keys`, `values`, `toMap`, `add`, `+:`, `remove`, `mapValues`, `filter`, `filterKeys`, `isEmpty`, `nonEmpty`, typed `merge(Json.Obj): Json.Obj` | zio-json/shared/src/main/scala/zio/json/ast/ast.scala:325-338 | High | json-ast.md, new "Working with Objects" section |
| `Obj#add` / `+:` semantics: implemented as `merge(Json.Obj(key -> value))`, so adding an existing key whose old and new values are both objects deep-merges them instead of replacing | ast.scala:332-333, 338-368 | High (gotcha) | same section, as a note |
| `Json` equality: object field order is ignored; `Num` equality is `java.math.BigDecimal.equals`, so it is scale-sensitive (`Json.Num(1) != Json.Num(1.0)`, `"1".fromJson[Json] != "1.0".fromJson[Json]`) | ast.scala:81-107 (Num at 100), 169-185 | High (gotcha) | json-ast.md "The Json Type" |
| Per-case codecs: `Json.codec/encoder/decoder`, `Json.Obj.codec`, `Json.Arr.codec`, `Json.Str.codec`, `Json.Num.codec`, `Json.Bool.codec`, `Json.Null.codec` — lets you use `Json`/`Json.Obj` etc. as case-class field types; mismatches fail with `Not an object`, `Not an array`, `Not a bool value`, `Not a string value`, `Not a number`, `Not null` | ast.scala:390-409, 443-462, 477-494, 501-518, 545-562, 567-585, 590-624 | High | json-ast.md, new "Embedding Raw JSON in Case Classes" section |
| `JsonCursor` instance methods `field(name)` and `element(index)` (require `To <:< Json.Obj` / `Json.Arr`, so need `.isObject` / `.isArray` first), `filterType`, `andThen`, instance `isBool`, `isNull`, `isNumber` | zio-json/shared/src/main/scala/zio/json/ast/JsonCursor.scala:34-55 | High | json-ast.md "Cursors" |
| Companion `JsonCursor.identity`, `JsonCursor.filter(jsonType)`, `JsonCursor.isBool`, `isNull`, `isNumber` | JsonCursor.scala:62-78 | Medium | json-ast.md "Cursors" |
| Cursor case classes `JsonCursor.Identity`, `DownField`, `DownElement`, `FilterType` | JsonCursor.scala:80-84 | Low | json-ast.md "Cursors" (one sentence) |
| Error strings returned by `get`/`transformAt`/`delete`: `No such field: 'x'`, `The array does not have index n`, `expected object/array/string/number/boolean/null`, `Non compatible types` (intersect) | ast.scala:155, 161, 200, 278, 289; JsonType.scala:24-29 | Medium | json-ast.md, in "Reading Values" |
| `JsonType` sealed trait with `Null`, `Bool`, `Obj`, `Arr`, `Str`, `Num` and `get(json): Either[String, A]` | zio-json/shared/src/main/scala/zio/json/ast/JsonType.scala:18-78 | Medium | json-ast.md "Cursors" (used by `filterType`/`JsonCursor.filter`) |
| `mapBoolean`, `mapNumber`, `mapString`, `mapArray`, `mapObject`, `mapObjectEntries` (page says "mapXxx" and shows only `mapObjectKeys`, `mapObjectValues`, `mapArrayValues`) | ast.scala:48-56 (overrides 371-374, 429-430, 466, 498, 522) | Medium | json-ast.md "Transforming Values" (a short table) |
| `Json.apply(fields: (String, Json)*)` returning `Json` | ast.scala:626-628 | Medium | json-ast.md "The Json Type" |
| `Json.Obj.apply` overloads (`Chunk`, varargs, `(key, value)`), `Json.Obj.empty`, `Json.Arr.empty`, `Json.Arr.apply(Chunk)`, `Json.Bool.True`/`False` | ast.scala:377-388, 433-441, 470-471 | Low | json-ast.md "The Json Type" |
| `Json.Arr#isEmpty`, `nonEmpty`, typed `Arr#merge` | ast.scala:412-426 | Low | json-ast.md |
| `Json#arrayOrObject(or, jsonArray, jsonObject)` | ast.scala:58-66 | Low | json-ast.md "Reading Values" |
| `Json#widen` | ast.scala:318 | Low | json-ast.md (one line: up-cast `Json.Obj` to `Json`) |
| `Json#toString` renders compact JSON | ast.scala:320 | Low | json-ast.md "Rendering and Parsing" |
| Class-level scaladoc warning: converting `Num` (BigDecimal) to other numbers can be a DoS vector (e.g. `.toBigInteger` on `1e214748364`) | ast.scala:32-36 | Medium | json-ast.md tip + link to explanation/security.md |
| `zio.json.codegen.Generator.printCaseClasses(input: String)` — prints Scala case classes + `DeriveJsonCodec` companions inferred from a sample JSON string (ANSI CYAN coloured, to stdout). Only public entry point; `generate` is `private[codegen]`. Infers `Int`/`Long`/`Double`/`BigDecimal`, `LocalDate`/`LocalDateTime`/`UUID` from strings, `Option` for null/missing fields, `List` for arrays. Tests only on Scala 2 (zio-json/shared/src/test/scala-2.12-2.13/zio/json/codegen/GeneratorSpec.scala) | zio-json/shared/src/main/scala/zio/json/codegen/Generator.scala:53-57, 207-233 | Low (dev/REPL utility; decide whether to document or mark internal) | short how-to "Generate case classes from sample JSON" or a tip in tutorial; list limitations (see bugs) |
| Public object `zio.json.codegen.JsonType` (`render`, `unifyTypes`, `flattenCaseClasses`, `renderCaseClass`) — public object over a `private[codegen]` trait, collides by simple name with `zio.json.ast.JsonType` | Generator.scala:151-235 | Low | Not doc-worthy; flag to maintainers (should be `private[codegen]`) |
| YAML: `Json#toYaml` returns `Either[YAMLException, String]` (not `Either[String, String]`) | zio-json-yaml/src/main/scala/zio/json/yaml/package.scala:25 | Medium | yaml.md "Encoding to YAML" |
| YAML: `Json#toYamlAST` returns `Node` directly; `A#toYamlAST` returns `Either[String, Node]` | package.scala:58, 64-65 | Low | yaml.md "Working with the YAML Node Tree" |
| YAML decoding limits: `fromYaml` uses `new Yaml()` with default `LoaderOptions` (single document only, nesting depth 50, 50 aliases, 3 MB code points, duplicate keys allowed — SnakeYAML LoaderOptions.java:24-39) and no way to pass custom options | package.scala:68-76 | Medium | yaml.md new "Decoding limits" subsection |
| YAML number rendering: integral numbers (after `stripTrailingZeros`) are written as `!!int`, so `Json.Num(1.0)` becomes `1`; `dropNulls` drops only object fields, not `null` array elements | package.scala:83-93, 127-133 | Low | yaml.md "Options" |
| `zio.json.golden.GoldenSample(samples: Json)` and its codec | zio-json-golden/src/main/scala/zio/json/golden/GoldenSample.scala:5-9 | Low | golden-testing.md (file format) |
| `zio.json.golden.filehelpers` (public: `moduleRootProperty`, `getRootDir`, `createGoldenDirectory`, `writeSampleToFile`, `readSampleFromFile`) | zio-json-golden/src/main/scala/zio/json/golden/filehelpers.scala:10-56 | Low | Not doc-worthy except `moduleRootProperty` constant (already described by string) |
| `enumeratum.ZioJson.encoder`, `encoderUppercase`, `decoder`, `decoderUppercaseOnly`, `decoderCaseInsensitive`, `keyEncoder`, `keyDecoder` (only `encoderLowercase` + `decoderLowercaseOnly` shown) | zio-json-interop-enumeratum/shared/src/main/scala/enumeratum/ZioJson.scala:7-32 | Medium | enumeratum.md "String Enums" (table of the 9 builders) |
| `enumeratum.values.ZioJson.encoder/decoder/keyEncoder/keyDecoder` (manual value-enum instances) | zio-json-interop-enumeratum/shared/src/main/scala/enumeratum/values/ZioJson.scala:5-26 | Low | enumeratum.md "Value Enums" |
| `enumeratum.values.ZioJsonValueEnum` base trait | zio-json-interop-enumeratum/shared/src/main/scala/enumeratum/values/ZioJsonValueEnum.scala:5-27 | Low | enumeratum.md "Value Enums" (one sentence) |
| http4s `jsonOf[F[_]: Concurrent, A: JsonDecoder]: EntityDecoder[F, A]` — reads body as UTF-8, empty body fails with `MalformedMessageBodyFailure("Invalid JSON: empty body")`, decode errors become `MalformedMessageBodyFailure` | zio-json-interop-http4s/src/main/scala/zio/json/interop/http4s/ZIOJsonInstances.scala:11-20 | High | http4s.md "Decoding request bodies" |
| http4s `ZIOEntityEncoder` / `ZIOEntityDecoder` / `ZIOEntityCodec` — implicit `EntityEncoder`/`EntityDecoder` for every type with a zio-json codec, via `import zio.json.interop.http4s.ZIOEntityCodec._` | ZIOEntityEncoder.scala:6-10, ZIOEntityDecoder.scala:7-12, ZIOEntityCodec.scala:3 | High | http4s.md "Automatic instances" |
| http4s `package object http4s extends ZIOJsonInstances` (why `import zio.json.interop.http4s._` brings `jsonOf`/`jsonEncoderOf`) | zio-json-interop-http4s/src/main/scala/zio/json/interop/http4s/package.scala:3 | Low | http4s.md |
| refined `encodeRefined` (page only derives a decoder) | zio-json-interop-refined/shared/src/main/scala/zio/json/interop/refined/package.scala:8-9 | Medium | refined.md (use `DeriveJsonCodec` in example) |
| refined `encodeFieldRefined` / `decodeFieldRefined` (refined types as `Map` keys) | refined/package.scala:14-18 | Medium | refined.md |
| scalaz7x `ilistJsonDecoder` (page shows only encoding) | zio-json-interop-scalaz7x/shared/src/main/scala/zio/json/interop/scalaz7x/package.scala:10-11 | Low | scalaz-7x.md |

### Partially documented

| Symbol / topic | Page:line | What is there | What is missing (verified in source) |
|---|---|---|---|
| `Json#get` | json-ast.md:64 | Sentence only; examples use `Obj#get` | A direct `json.get(JsonCursor.field("a").isString)` example; return type `Either[String, A]` with refined `A` (ast.scala:147) |
| `as*` accessors | json-ast.md:76 | Names listed | Exact return types: `asNumber: Option[Json.Num]`, `asArray: Option[Chunk[Json]]`, `asObject: Option[Json.Obj]`, `asNull: Option[Unit]` (ast.scala:41-46) |
| `foldDown` / `foldUp` | json-ast.md:148 | "visit every node" | `foldDown` is pre-order (parent before children, ast.scala:109-117); `foldUp` is post-order (children first, ast.scala:126-139); object fields visited in insertion order |
| `merge` on arrays | json-ast.md:128 | "their elements are merged" | Merged pairwise by index; extra elements of the longer array are appended (ast.scala:415-426) |
| `intersect` | json-ast.md:137 | "fields or elements present in both" | Compares whole key-value pairs / elements with `Chunk#intersect` (multiset, order of left side; ast.scala:194-201) |
| `relocate` | json-ast.md:114 | "moves a node" | Target cursor must already exist (see Wrong #1) |
| `delete` | json-ast.md:114-118 | Basic example | Deleting `JsonCursor.identity` returns `Right(Json.Null)` (ast.scala:75-79, 271) |
| `>>>` | json-ast.md:191-196 | Used in example | Never explained; `andThen` alias not mentioned (JsonCursor.scala:19-34) |
| YAML `newDumperOptions` | yaml.md:91 | "supply your own DumperOptions" | `toYaml` overwrites indent, indicatorIndent, width/splitLines, lineBreak, indentWithIndicator afterwards (package.scala:29-40), so only other DumperOptions settings take effect |
| `ZioJsonKeyEnum` | enumeratum.md:36 | One sentence | No `Map[ShirtSize, Int]` example; key decoder is case-sensitive only (ZioJson.scala:31-32) |
| `goldenTest` naming | golden-testing.md:44 | "short name of the type" | Name is `Tag[A].tag.shortName` (golden/package.scala:192-193); generic types such as `List[Banana]` and `Option[Banana]` both use the type-constructor short name, so they collide; use `relativePath` |
| `_changed.json` lifecycle | golden-testing.md:54 | Created on mismatch | Never deleted by the library after it passes again; stale `_changed`/`_new` files must be removed by hand; an existing `_new.json` is overwritten (package.scala:155-175) |
| golden `DeriveGen` dependency | golden-testing.md:20 | Uses `zio.test.magnolia.DeriveGen` | zio-json-golden already depends on `zio-test-magnolia` at compile scope (build.sbt:336), so no extra dependency is needed — worth one sentence |
| http4s module | http4s.md:16-34 | `jsonEncoderOf` only | `jsonOf`, `ZIOEntityCodec`, JVM-only, CE3 requirement |

### Wrong or outdated statements

| # | Page:line | Claim | Truth + source | Corrected statement |
|---|---|---|---|---|
| 1 | docs/reference/json-ast.md:114 | "The `relocate` method moves a node from one cursor to another, removing it from its old location" | `relocate` = `get(from)` then `transformAt(to)` then `delete(from)` (ast.scala:228-232). `transformAt` on a `DownField` returns `Left("No such field: '...'")` when the key is absent (ast.scala:277-278), so `relocate` can only overwrite an existing node; it cannot move a value to a new key | "The `relocate` method copies the node at the `from` cursor over the existing node at the `to` cursor, then deletes the `from` node. The `to` location must already exist; to move a value to a new key, read it with `get`, add it with `Json.Obj#add`, and remove the old key with `delete`." |
| 2 | docs/reference/json-ast.md:128 | "When both values are arrays, their elements are merged." | Arrays are zipped by index, each pair merged, leftover tail of the longer array appended (ast.scala:415-426) | "When both values are arrays, elements at the same index are merged with each other, and the extra elements of the longer array are kept." |
| 3 | docs/reference/json-ast.md:137 | "keeps only the fields or elements that are present in both values" | `Chunk#intersect` on `(key, value)` tuples / elements (ast.scala:196-199): a key with different values on each side is dropped | "The `intersect` method keeps only the key-value pairs (for objects) or elements (for arrays) that are equal on both sides. It returns `Left("Non compatible types")` unless both values are objects or both are arrays." |
| 4 | docs/reference/json-ast.md:76 | "Each case has an `as*` method that returns an `Option` of the matching type" | The six `as*` methods are defined on `Json` and return `None`; each case overrides only its own (ast.scala:41-46, 370, 428, 465, 497, 521, 587). `asNumber` returns `Json.Num`, not a number | "`Json` has six `as*` methods (`asString`, `asNumber`, `asArray`, `asObject`, `asBoolean`, `asNull`). Each returns `Some` only when the value is that case: `asString` gives a `String`, `asNumber` a `Json.Num`, `asArray` a `Chunk[Json]`, `asObject` a `Json.Obj`, `asBoolean` a `Boolean`, and `asNull` a `Unit`." |
| 5 | docs/reference/json-ast.md:84 | "`mapObjectKeys` renames every key in an object" | Maps only the top-level keys of an `Obj`; no recursion; identity on non-objects (ast.scala:54, 372) | "For example, `mapObjectKeys` renames the top-level keys of an object; nested objects are not changed (use `transformDownSome` for that)." |
| 6 | docs/reference/yaml.md:84 | "`indentation` ... and `sequenceIndentation` (default `2`): the number of spaces used for nested mappings and sequences." | `sequenceIndentation` is passed to `DumperOptions.setIndicatorIndent` (package.scala:31), which SnakeYAML defines as "number of white spaces to use for the sequence indicator '-'" (DumperOptions.java:372-377) | "`indentation` (default `2`): spaces per nesting level. `sequenceIndentation` (default `2`): spaces placed before the `-` indicator of sequence entries (SnakeYAML's indicator indent)." |
| 7 | docs/reference/yaml.md:85 | "`indentWithIndicator` (default `true`): indent the sequence entries relative to the `-` indicator." | Passed to `setIndentWithIndicator` (package.scala:40); SnakeYAML: "Set to true to add the indent for sequences to the general indent" (DumperOptions.java:396-401) | "`indentWithIndicator` (default `true`): adds the `-` indicator indent to the general indentation, so sequence item content is indented past the `-` instead of sharing its column." |
| 8 | docs/reference/yaml.md:93 | "Strings that contain line breaks are written as literal block scalars (`\|`) automatically, even when the `scalarStyle` is `PLAIN`." | The switch happens only when the style is `PLAIN` (package.scala:122-124); other styles are kept as chosen | "When `scalarStyle` returns `PLAIN` for a string that contains a line break, the string is written as a literal block scalar (`\|`) instead." |
| 9 | docs/reference/yaml.md:37 + :43 | "Calling `toYaml` returns an `Either[String, String]`" then "`toYaml` also accepts a `Json` value directly" | On a `Json` value, `toYaml` is `JsonOps.toYaml: Either[YAMLException, String]` (package.scala:25); `Either[String, String]` is only for values with an encoder (package.scala:62) | Add after line 43: "On a `Json` value, `toYaml` returns `Either[YAMLException, String]`, and `toYamlAST` returns the SnakeYAML `Node` directly." |
| 10 | docs/reference/yaml.md:91 | "use `newDumperOptions` to supply your own SnakeYAML `DumperOptions`" | `toYaml` overwrites indent, indicator indent, width/split lines, line break and indentWithIndicator after creating the options (package.scala:29-40) | "...use `newDumperOptions` to supply your own SnakeYAML `DumperOptions`. The settings that `YamlOptions` exposes (indentation, sequence indentation, width, line break, indentWithIndicator) are always overwritten from `YamlOptions`, so set those through `YamlOptions` instead." |
| 11 | docs/interop/enumeratum.md:54 | `ShirtSize.Small.toJson // "Small"` | `JsonEncoder[A]` is invariant (zio-json/shared/src/main/scala/zio/json/JsonEncoder.scala:27); `ShirtSize.Small` has type `ShirtSize.Small.type`, and only `JsonEncoder[ShirtSize]` exists, so the call does not compile. The source scaladoc avoids this with `val size: ShirtSize = ShirtSize.Small` (ZioJsonEnum.scala:21-24); tests use `values` (typed as the base type) | `(ShirtSize.Small: ShirtSize).toJson` — and convert the block to `mdoc` (docs project already depends on the enumeratum module, build.sbt:463) |
| 12 | docs/interop/enumeratum.md:103 | `LibraryItem.Book.toJson // 1` | Same invariance issue | `(LibraryItem.Book: LibraryItem).toJson` |
| 13 | docs/interop/enumeratum.md:106, :109 | `3.fromJson[LibraryItem]`, `999.fromJson[LibraryItem]` | `fromJson` is an extension on `CharSequence` only (zio-json/shared/src/main/scala/zio/json/package.scala:30, 41); `Int` has no `fromJson`; does not compile. Tests use `"999".fromJson[...]` (ZioJsonValueEnumSpec.scala:25) | `"3".fromJson[LibraryItem]` and `"999".fromJson[LibraryItem]` |
| 14 | docs/interop/http4s.md:38 | Example "can be found here" → `https://github.com/zio/zio-json/tree/develop/examples/interop-http4s` | Upstream has no `develop` branch; `git ls-remote origin` shows HEAD → `refs/heads/series/2.x` | Link to `https://github.com/zio/zio-json/tree/series/2.x/examples/interop-http4s` |
| 15 | docs/interop/http4s.md:6 + :38 | Module is for "HTTP4s v0.23"; example is "fully working" | The example uses http4s **0.21.21**, blaze server, CE2 `ConcurrentEffect`/`Timer`, zio-json `0.1.3+8-...-SNAPSHOT`, sbt 1.5.7 (examples/interop-http4s/build.sbt:1-16, Server.scala:3-21, project/build.properties:1, Readme.md:3 links http4s v0.21). The module is built against http4s 0.23.38 + cats-effect 3.7.0 (build.sbt:391-393). The example cannot be built against the documented module | Either update the example to http4s 0.23 / CE3 / current zio-json, or change the sentence to: "An older example project for http4s 0.21 is in `examples/interop-http4s`; it does not build against the current module." |
| 16 | docs/interop/scalaz-7x.md:1-23 | (no platform statement; implies all Scala versions) | `crossScalaVersions -= Scala3` (build.sbt:424) | Add: "This module is published for Scala 2.12 and 2.13 only (JVM, Scala.js and Scala Native); there is no Scala 3 artifact." |
| 17 | docs/interop/enumeratum.md:19-23, refined.md:9, scalaz-7x.md:9 | `%%` only | These three are `crossProject(JSPlatform, JVMPlatform, NativePlatform)` (build.sbt:403, 418, 434) | Add: "For Scala.js or Scala Native, use `%%%` instead of `%%`." |
| 18 | docs/interop/http4s.md:31 | `implicit def entityEncoder[F[_]: Applicative]` | `jsonEncoderOf[F[_], A: JsonEncoder]` needs no type class on `F` (ZIOJsonInstances.scala:22) — not wrong, but the `Applicative` bound and `cats.Applicative` import are unnecessary (copied from the old CE2 example) | `implicit def entityEncoder[F[_]]: EntityEncoder[F, Greeting] = jsonEncoderOf[F, Greeting]` (low priority) |
| 19 | docs/index.md:135 | `"scala-java-time-tzdb" % "latest.integration"` | zio-json JS/Native depend on `scala-java-time` 2.10.0 (project/BuildHelper.scala:279; build.sbt:296, 312), tzdb only in Test scope. `latest.integration` is non-reproducible and can pull a tzdb incompatible with the scala-java-time version | `libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.10.0"` with a note "use the same version as `scala-java-time`" (low priority) |

Verified as correct (no action): json-ast.md:12-13, :30 (all nine `Num.apply` overloads exist, ast.scala:525-543), :34 (`toJsonAST` / `fromJsonAST`, package.scala:27, JsonDecoder.scala:256), :90, :99-100 (transformDown applies `f` then recurses; transformUp recurses then applies, ast.scala:248-316), :128 object merge, :159; yaml.md:10, :17, :22, :62 (NonFatal → Left, package.scala:73-75), :83, :86-89 (defaults match YamlOptions.scala:28-39); golden-testing.md:11, :14, :44, :48, :52-54, :60, :88, :100-105 (property name matches filehelpers.scala:16); interop/index.md lists all four interop modules; enumeratum.md:27, :61 (message format `'X' is not a member of enum Y`, ZioJson.scala:34-35, rendered in parentheses by JsonError.scala:32), :73-84 table (all six value-enum traits exist, ZioJsonValueEnum.scala:56-120, each providing the four instances); http4s.md:6 (0.23.38).

### Missing examples / cross-links

1. json-ast.md has no example of `Json#get` with a cursor on `user` (only `Obj#get`), no example of `JsonCursor` instance chaining (`JsonCursor.field("a").isObject.field("b")`), and no example of `filterType`/`JsonType`.
2. json-ast.md has no "use `Json` as a field type" example (e.g. `case class Event(kind: String, payload: Json)` or `Json.Obj`) although this is the main reason the type exists (scaladoc ast.scala:27-30).
3. json-ast.md has no example of building / editing objects with `add`, `remove`, `filterKeys`, `+:`.
4. json-ast.md "Cursors" (line 173) should link to the transforms above (`transformAt`, `delete`, `relocate` all take cursors) and vice versa; the intro (line 8) links to Cursors, but the cursor-taking methods at lines 106-124 are used before cursors are explained. Recommend moving "Cursors" before "Transforming Values".
5. json-ast.md should link to explanation/security.md for the `Num` DoS caveat, and to how-to/decoding.md (which pattern-matches on `Json.Obj` at lines 226 and 375).
6. yaml.md has no example of `YamlOptions` `flowStyle`/`scalarStyle`/`keyStyle` functions (line 88 promises "force flow style for small objects" but shows nothing), nor `NonPrintableStyle.BINARY`, nor `maxScalarWidth = None`.
7. yaml.md has no round-trip example (`toYaml` then `fromYaml`) and no example of `fromYaml[Json]` on multi-type scalars (dates become strings via `case _ => Json.Str(scalar.getValue)`, package.scala:154).
8. golden-testing.md: no link to the `examples/zio-json-golden` project (which pins zio-json-golden 0.7.8 — also stale: examples/zio-json-golden/build.sbt:2) and no sample of the produced golden file content (`{"samples": [...]}`).
9. enumeratum.md: both code blocks are plain ```scala, not mdoc, which is why the non-compiling lines (Wrong #11-13) survived. Convert to `mdoc`. Add a `Map[ShirtSize, Int]` key example for `ZioJsonKeyEnum` and a value-enum key example.
10. http4s.md: no decoding example (`req.as[Greeting]` with `jsonOf` / `ZIOEntityDecoder`), no route example, plain ```scala (not mdoc; the docs project depends on the http4s module, build.sbt:460, so mdoc is possible).
11. refined.md: shows only a decoder; no encoder or `Map` key example; no link to how-to/validate-on-decode.md (which covers refinement-style validation).
12. scalaz-7x.md: one line; no decoding example (`"[1,2,3]".fromJson[IList[Int]]`).
13. interop/index.md: list of links only — no one-line description per module, no install/platform matrix, no mention of the other add-on modules (`zio-json-yaml`, `zio-json-golden`, `zio-json-macros`) or link to them.
14. docs/index.md "Installation": lists only core; no table of add-on artifacts with platform/Scala support (see matrix above). README.md mirrors this (README.md:26-40).

### Conceptual gaps

1. No how-to on navigating and editing a JSON document whose shape is unknown (read nested values with cursors, update one field, delete/rename keys, merge config defaults). The material exists as one-liners in json-ast.md but there is no task-oriented guide, and the cursor variance rules (`field`/`element` need an `isObject`/`isArray` step; `JsonCursor.field(...)` has `From = Json.Obj` but `Json#get` accepts it on any `Json`) are not explained.
2. No explanation of `Json` equality semantics (order-insensitive objects, scale-sensitive numbers) — this matters for tests, `intersect`, and golden testing (golden compares with `==`, golden/package.scala:144).
3. No YAML how-to (e.g. "load a YAML config file into a case class", "convert JSON to YAML"); yaml.md is reference only, and nothing documents the decoding limits inherited from SnakeYAML defaults (single document, depth 50, 3 MB).
4. Interop pages are thin: http4s covers only half the API (encoding), with a stale example; refined covers only decoding; scalaz covers only encoding; none state platforms/Scala versions.
5. No statement anywhere of which modules support Scala 3 / Scala.js / Native (scalaz7x and macros are Scala-2 only; yaml, golden, http4s are JVM only).
6. Codegen (`Generator.printCaseClasses`) is neither documented nor marked internal; maintainers should decide. If documented, it needs its limitations (see bugs 3-5).
7. enumeratum on Scala 3: enumeratum's `findValues` requires `-Yretain-trees` (the module's own build sets it, build.sbt:440-442); users get no hint.

### Library bugs noticed

1. **YAML: integers beyond `Long` silently corrupted.** `jsonToYaml` writes integral numbers with `stripped.longValue.toString` (zio-json-yaml/src/main/scala/zio/json/yaml/package.scala:128-130). `java.math.BigDecimal.longValue` keeps only the low 64 bits. Repro: `Json.Num(BigInt("123456789012345678901234567890")).toYaml()` produces a wrong number. Fix idea: `stripped.toBigInteger.toString` (or `toPlainString`).
2. **`Json#delete` ignores type filters.** For `delete`, `transformOrDelete` passes `delete = true` through `FilterType` to the parent `DownField`/`DownElement`, which deletes without ever calling the filter function (ast.scala:279-280, 290-291, 296-297). Repro: `Json.Obj("a" -> Json.Num(1)).delete(JsonCursor.field("a").isString)` returns `Right({})` instead of `Left("expected string")`.
3. **Codegen crashes on empty arrays.** `unifyTypes` does `elements.map(...).reduce(_ unify _)` (Generator.scala:211); an empty array throws `UnsupportedOperationException`. Repro: `Generator.printCaseClasses("""{"a": []}""")`. No test covers it (GeneratorSpec.scala).
4. **Codegen throws on heterogeneous arrays / mixed field types.** `unify` throws `Exception("Cannot combine ...")` for e.g. `[1, "x"]` (Generator.scala:96-101); the `Alternatives` case is never produced. Field names are emitted unescaped (Generator.scala:192-193), so keys like `"first-name"` or `"type"` produce invalid Scala. Scaladoc example says `JsonCoder[...]` but output uses `JsonCodec` (Generator.scala:41, 49 — doc typo).
5. **`fromYaml` on empty input fails with an NPE message.** SnakeYAML `compose` returns `null` for an empty stream (Composer.getSingleNode, Composer.java:151-158); `yamlToJson(null)` hits `case _ => throw new YAMLException(s"... ${node.getClass...}")` and throws `NullPointerException` (package.scala:181-182), caught by `NonFatal` → `Left(<NPE message or null>)`. Repro: `"".fromYaml[Json]`. Expected: a clear "empty document" error (or `Json.Null`).
6. **`Json.equals`/`hashCode` inconsistent with duplicate keys.** `objEqual` compares `left.toMap` against `right` chunk (ast.scala:82-95), so equality is not symmetric: `Obj("a"->1,"a"->2) == Obj("a"->2,"a"->2)` is `true` but the reverse is `false`; their `hashCode`s (XOR of tuples, ast.scala:174-177) also differ. Low priority (duplicate keys are rare but the decoder accepts them).
7. **golden: missing `target` ancestor causes a defect, not a failure.** `getRootDir` recurses to `getParentFile` until a dir named `target`; at the filesystem root `getParentFile` is `null` and `file.getName` throws an NPE outside `ZIO.attempt` (filehelpers.scala:18-20), so the test dies with an NPE instead of a message pointing at `zio.json.golden.moduleRoot`. Repro: run a golden test from a build whose classes dir has no `target` ancestor and no system property set.
