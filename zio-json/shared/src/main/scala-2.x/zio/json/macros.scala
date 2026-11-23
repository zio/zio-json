package zio.json

import magnolia1._
import zio.Chunk
import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.{ FieldEncoder, Lexer, RecordingReader, RetractReader, StringMatrix, Write }
import scala.annotation._
import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros

/**
 * Sets the JSON name of a case class field, for both encoding and decoding. Without it, the JSON name is the field name
 * after the [[jsonMemberNames]] format of the class or the `fieldNameMapping` of the [[JsonCodecConfiguration]]. The
 * name given here is used unchanged and wins over both. The decoder accepts only this name and the [[jsonAliases]] of
 * the field.
 *
 * {{{
 * final case class Book(@jsonField("book_title") title: String, pages: Int)
 *
 * object Book {
 *   implicit val codec: JsonCodec[Book] = DeriveJsonCodec.gen[Book]
 * }
 *
 * Book("Dune", 412).toJson // {"book_title":"Dune","pages":412}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonField(name: String) extends Annotation

/**
 * Adds names that the derived decoder also accepts for a case class field, next to its JSON name. The encoder writes
 * only the JSON name. An object that holds a field under two of its names fails to decode with `(duplicate)`. Creating
 * the decoder throws a `java.lang.AssertionError` when an alias equals a JSON field name or another alias of the class.
 *
 * On Scala 3, only one `jsonAliases` annotation per field is read; put all the aliases in one annotation.
 *
 * {{{
 * final case class Customer(@jsonAliases("full_name", "fullName") name: String)
 *
 * object Customer {
 *   implicit val codec: JsonCodec[Customer] = DeriveJsonCodec.gen[Customer]
 * }
 *
 * """{"full_name":"Ada"}""".fromJson[Customer] // Right(Customer(Ada))
 * Customer("Ada").toJson                       // {"name":"Ada"}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonAliases(alias: String, aliases: String*) extends Annotation

/**
 * Makes the derived encoder write an `Option` field that is `None` as `null` instead of leaving the key out. On a case
 * class it applies to every field, on a field only to that field. `JsonCodecConfiguration(explicitNulls = true)` does
 * the same for every class derived with that configuration in scope. The decoder is not affected: it reads a missing
 * key and `null` as `None`.
 *
 * {{{
 * final case class Sensor(id: String, @jsonExplicitNull reading: Option[Double], unit: Option[String])
 *
 * object Sensor {
 *   implicit val codec: JsonCodec[Sensor] = DeriveJsonCodec.gen[Sensor]
 * }
 *
 * Sensor("t1", None, None).toJson // {"id":"t1","reading":null}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final class jsonExplicitNull extends Annotation

/**
 * Sets, for a case class or one of its fields, whether fields that hold an empty collection are written and required.
 * It overrides the `explicitEmptyCollections` option of the [[JsonCodecConfiguration]].
 *
 * With `encoding = false`, the derived encoder leaves out a field whose collection is empty, such as an empty `List` or
 * `Map`. On Scala 2 it also leaves out a field whose type is a case class without fields; Scala 3 writes it as `{}`.
 * With `decoding = false` on the class, the derived decoder reads a missing collection field as an empty collection;
 * with the default `true`, it fails with `.tracks(missing)`, where `tracks` is the field name. On a field, only
 * `encoding` has an effect.
 *
 * {{{
 * @jsonExplicitEmptyCollections(encoding = false, decoding = false)
 * final case class Playlist(name: String, tracks: List[String])
 *
 * object Playlist {
 *   implicit val codec: JsonCodec[Playlist] = DeriveJsonCodec.gen[Playlist]
 * }
 *
 * Playlist("chill", Nil).toJson              // {"name":"chill"}
 * """{"name":"chill"}""".fromJson[Playlist] // Right(Playlist(chill,List()))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonExplicitEmptyCollections(encoding: Boolean = true, decoding: Boolean = true) extends Annotation

/**
 * Writes the type hint of a sealed trait or class as a field named `name` inside the object of each case, instead of
 * wrapping the case in an object whose only key is the type hint. The encoder writes the hint field first, and the
 * decoder accepts it at any position. A case object is written as an object with only the hint field. The annotation
 * overrides the `sumTypeHandling` option of the [[JsonCodecConfiguration]].
 *
 * Decoding fails with `(missing hint 'type')` when the field is absent (here `type` is the `name`), and with
 * `(invalid disambiguator)` when its value is a string that is not the hint of a case. The decoder of each case sees
 * the hint field, so a case that rejects unknown fields ([[jsonNoExtraFields]] or `allowExtraFields = false`) fails
 * with `(Circle)(invalid extra field)`, where `Circle` is the hint.
 *
 * Note that using a discriminator is less performant, uses more memory, and may be prone to DOS attacks that are
 * impossible with the default encoding. A hand-written encoder of a case must write a JSON object: for any other JSON,
 * `toJson` writes invalid JSON and `toJsonAST` returns a `Left`. Use a discriminator when an external schema requires
 * one.
 *
 * {{{
 * @jsonDiscriminator("type")
 * sealed trait Shape
 * final case class Circle(radius: Double) extends Shape
 * final case class Square(side: Double)   extends Shape
 *
 * object Shape {
 *   implicit val codec: JsonCodec[Shape] = DeriveJsonCodec.gen[Shape]
 * }
 *
 * (Circle(1.5): Shape).toJson                            // {"type":"Circle","radius":1.5}
 * """{"side":2.0,"type":"Square"}""".fromJson[Shape] // Right(Square(2.0))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonDiscriminator(name: String) extends Annotation
// TODO a strategy where the constructor is inferred from the field names, only
// valid if there is no ambiguity in the types of fields for all case classes.
// Such a strategy cannot be implemented with Magnolia because the SealedTrait
// does not provide a mechanism for obtaining the CaseClass associated to the
// Subtype.

/**
 * A function that turns a Scala name into a JSON name. Derivation applies it to the field names of a case class
 * ([[jsonMemberNames]], or the `fieldNameMapping` of the [[JsonCodecConfiguration]]) and to the type hints of a sealed
 * trait ([[jsonHintNames]], or `sumTypeMapping`), for both encoding and decoding. The formats are [[SnakeCase]],
 * [[KebabCase]], [[CamelCase]], [[PascalCase]], [[IdentityFormat]] and [[CustomCase]], plus the zio-json 0.3.0 variants
 * `ziojson_03.SnakeCase` and `ziojson_03.KebabCase`.
 *
 * {{{
 * SnakeCase("firstName")            // first_name
 * KebabCase("firstName")            // first-name
 * PascalCase("first_name")          // FirstName
 * CustomCase(_.toUpperCase)("name") // NAME
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
sealed trait JsonMemberFormat extends (String => String)

/**
 * A [[JsonMemberFormat]] that applies `f` to each name, for cases that the built-in formats do not cover.
 *
 * {{{
 * @jsonMemberNames(CustomCase(name => name.toUpperCase))
 * final case class Reading(value: Double)
 *
 * object Reading {
 *   implicit val codec: JsonCodec[Reading] = DeriveJsonCodec.gen[Reading]
 * }
 *
 * Reading(21.5).toJson // {"VALUE":21.5}
 * }}}
 */
case class CustomCase(f: String => String) extends JsonMemberFormat {
  override def apply(memberName: String): String = f(memberName)
}

/**
 * Writes names in snake case: `firstName` becomes `first_name`, `HTTPServer` becomes `http_server`, and a `-` becomes
 * `_`. A digit does not start a new word, so `sensor2Value` becomes `sensor2_value`; `ziojson_03.SnakeCase` gives
 * `sensor_2_value`.
 */
case object SnakeCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '_')
}

/**
 * Writes names in camel case: `first_name` and `first-name` become `firstName`. In a name with `_` or `-`, the
 * separators are dropped, the letter after each one is upper-cased, and the other letters are lower-cased. A name
 * without them only gets a lower-case first letter, so `HTTPServer` becomes `hTTPServer`.
 */
case object CamelCase extends JsonMemberFormat {
  override def apply(memberName: String): String =
    jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = false)
}

/**
 * Writes names in Pascal case: `firstName`, `first_name` and `first-name` become `FirstName`. In a name with `_` or
 * `-`, the separators are dropped, the first letter and the letter after each separator are upper-cased, and the other
 * letters are lower-cased. A name without them only gets an upper-case first letter.
 */
case object PascalCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = true)
}

/**
 * Writes names in kebab case: `firstName` becomes `first-name`, `HTTPServer` becomes `http-server`, and a `_` becomes
 * `-`. A digit does not start a new word, so `sensor2Value` becomes `sensor2-value`; `ziojson_03.KebabCase` gives
 * `sensor-2-value`.
 */
case object KebabCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '-')
}

/**
 * Leaves names unchanged. It is the default `fieldNameMapping` and `sumTypeMapping` of [[JsonCodecConfiguration]].
 */
case object IdentityFormat extends JsonMemberFormat {
  override def apply(memberName: String): String = memberName
}

/**
 * The snake case and kebab case formats of zio-json 0.3.0. They start a new word at a digit that follows a lower-case
 * letter, so `abc123Def` becomes `abc_123_def`, where the current `zio.json.SnakeCase` gives `abc123_def`. Use them to
 * keep JSON names that were produced with that version.
 */
object ziojson_03 {

  /** Snake case as in zio-json 0.3.0: `abc123Def` becomes `abc_123_def`. */
  case object SnakeCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '_')
  }

  /** Kebab case as in zio-json 0.3.0: `abc123Def` becomes `abc-123-def`. */
  case object KebabCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '-')
  }
}

/**
 * Sets the [[JsonMemberFormat]] that turns the field names of a case class into JSON names, for both encoding and
 * decoding. The decoder accepts the converted names and the [[jsonAliases]] of each field, not the original field
 * names. It overrides the `fieldNameMapping` of the [[JsonCodecConfiguration]] for this class, and [[jsonField]]
 * overrides it for one field.
 *
 * {{{
 * @jsonMemberNames(SnakeCase)
 * final case class Order(orderId: Int, totalAmount: Double)
 *
 * object Order {
 *   implicit val codec: JsonCodec[Order] = DeriveJsonCodec.gen[Order]
 * }
 *
 * Order(1, 9.5).toJson // {"order_id":1,"total_amount":9.5}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonMemberNames(format: JsonMemberFormat) extends Annotation
private[json] object jsonMemberNames {

  import java.lang.Character._

  def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String =
    if (s.indexOf('_') == -1 && s.indexOf('-') == -1) {
      if (s.isEmpty) s
      else {
        val ch      = s.charAt(0)
        val fixedCh =
          if (toPascal) toUpperCase(ch)
          else toLowerCase(ch)
        s"$fixedCh${s.substring(1)}"
      }
    } else {
      val len             = s.length
      val sb              = new StringBuilder(len)
      var i               = 0
      var isPrecedingDash = toPascal
      while (i < len) isPrecedingDash = {
        val ch = s.charAt(i)
        i += 1
        (ch == '_' || ch == '-') || {
          val fixedCh =
            if (isPrecedingDash) toUpperCase(ch)
            else toLowerCase(ch)
          sb.append(fixedCh)
          false
        }
      }
      sb.toString
    }

  def enforceSnakeOrKebabCase(s: String, separator: Char): String = {
    val len                      = s.length
    val sb                       = new StringBuilder(len << 1)
    var i                        = 0
    var isPrecedingNotUpperCased = false
    while (i < len) isPrecedingNotUpperCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (!isUpperCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingNotUpperCased || i > 1 && i < len && !isUpperCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

  def enforceSnakeOrKebabCaseSeparateNumbers(s: String, separator: Char): String = {
    val len                   = s.length
    val sb                    = new StringBuilder(len << 1)
    var i                     = 0
    var isPrecedingLowerCased = false
    while (i < len) isPrecedingLowerCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (isLowerCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingLowerCased || i > 1 && i < len && isLowerCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

}

/**
 * Sets the type hint of a case class or case object in a sealed family: the key of the wrapper object, the value of the
 * discriminator field, or the string written when `enumValuesAsStrings` applies. Without it, the hint is the short type
 * name after the [[jsonHintNames]] format of the sealed type or the `sumTypeMapping` of the [[JsonCodecConfiguration]].
 * The name given here is used unchanged. Decoding a hint that names no case fails with `(invalid disambiguator)`, or
 * with `(invalid enumeration value)` for the string form.
 *
 * {{{
 * sealed trait Payment
 * @jsonHint("card") final case class CreditCard(number: String) extends Payment
 * case object Cash extends Payment
 *
 * object Payment {
 *   implicit val codec: JsonCodec[Payment] = DeriveJsonCodec.gen[Payment]
 * }
 *
 * (CreditCard("4111"): Payment).toJson // {"card":{"number":"4111"}}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonHint(name: String) extends Annotation

/**
 * Sets the [[JsonMemberFormat]] that turns the short type names of the cases of a sealed trait or class into type
 * hints, for both encoding and decoding. It overrides the `sumTypeMapping` of the [[JsonCodecConfiguration]] for this
 * type, and a case with [[jsonHint]] keeps the hint given there.
 *
 * {{{
 * @jsonHintNames(KebabCase)
 * sealed trait Transfer
 * final case class BankTransfer(iban: String) extends Transfer
 * final case class WireTransfer(swift: String) extends Transfer
 *
 * object Transfer {
 *   implicit val codec: JsonCodec[Transfer] = DeriveJsonCodec.gen[Transfer]
 * }
 *
 * (BankTransfer("DE89"): Transfer).toJson // {"bank-transfer":{"iban":"DE89"}}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final case class jsonHintNames(format: JsonMemberFormat) extends Annotation

/**
 * Makes the derived decoder of a case class fail with `(invalid extra field)` at the first key that is neither a JSON
 * field name nor an alias of the class. Without it, the decoder skips unknown keys.
 * `JsonCodecConfiguration(allowExtraFields = false)` does the same for every class derived with that configuration in
 * scope. On a case class without fields, decoding JSON text accepts only an empty object.
 *
 * This adds extra protections against a DOS attacks but means that changes in the schema will result in a hard error
 * rather than silently ignoring those fields.
 *
 * It cannot be combined with [[jsonDiscriminator]]: the decoder of the case sees the discriminator field as an extra
 * field, so decoding fails with `(Circle)(invalid extra field)`, where `Circle` is the type hint.
 *
 * {{{
 * @jsonNoExtraFields
 * final case class Point(x: Int, y: Int)
 *
 * object Point {
 *   implicit val codec: JsonCodec[Point] = DeriveJsonCodec.gen[Point]
 * }
 *
 * """{"x":1,"y":2,"z":3}""".fromJson[Point] // Left((invalid extra field))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final class jsonNoExtraFields extends Annotation

/**
 * Leaves a case class field out of the JSON that the derived encoder writes. The derived decoder still reads the field
 * when its key is present. When the key is missing, the decoder uses the default value of the field, `None` for an
 * `Option`, and otherwise fails with `.cacheKey(missing)`, where `cacheKey` is the field name. Give an excluded field a
 * default value or an `Option` type so that the encoder's output can be decoded.
 *
 * {{{
 * final case class Account(name: String, @jsonExclude cacheKey: Int = 0)
 *
 * object Account {
 *   implicit val codec: JsonCodec[Account] = DeriveJsonCodec.gen[Account]
 * }
 *
 * Account("ann", 42).toJson               // {"name":"ann"}
 * """{"name":"ann"}""".fromJson[Account] // Right(Account(ann,0))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/configuration/ Configuration]]
 */
final class jsonExclude extends Annotation

private class CaseObjectDecoder[Typeclass[_], A](val ctx: CaseClass[Typeclass, A], no_extra: Boolean)
    extends CollectionJsonDecoder[A] {
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
    if (no_extra) {
      Lexer.char(trace, in, '{')
      Lexer.char(trace, in, '}')
    } else Lexer.skipValue(trace, in)
    ctx.rawConstruct(Nil)
  }

  override def unsafeDecodeMissing(trace: List[JsonError]): A = ctx.rawConstruct(Nil)

  override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
    json match {
      case _: Json.Obj | Json.Null => ctx.rawConstruct(Nil)
      case _                       => Lexer.error("expected object", trace)
    }
}

/**
 * Derives a [[JsonDecoder]] for a case class or a sealed trait at compile time. A case class is read from a JSON object
 * with one key per field. A case of a sealed trait is read from an object whose only key is the type hint (the short
 * type name by default) and whose value is the case. On Scala 3, where `enumValuesAsStrings` is `true` by default, a
 * sealed trait whose cases are all case objects is read from a JSON string such as `"Red"`. The annotations of the
 * type, such as [[jsonField]] or [[jsonDiscriminator]], and the [[JsonCodecConfiguration]] in implicit scope change
 * these defaults.
 *
 * On Scala 2, the derivation uses Magnolia, and every field type needs a decoder in implicit scope. On Scala 3, it uses
 * the `Mirror` of the type, and derives a decoder for a field whose type is a case class or a sealed trait without one.
 * A known bug on Scala 3: a field of type `Option[Inner]`, where `Inner` has no decoder, is derived as a sealed trait
 * and read from `{"Some":{"value":...}}`, and a `List[Inner]` field fails to compile; give `Inner` its own decoder.
 *
 * {{{
 * final case class Sensor(id: String, celsius: Double, label: Option[String])
 *
 * object Sensor {
 *   implicit val decoder: JsonDecoder[Sensor] = DeriveJsonDecoder.gen[Sensor]
 * }
 *
 * """{"id":"s1","celsius":21.5}""".fromJson[Sensor] // Right(Sensor(s1,21.5,None))
 * """{"id":"s1"}""".fromJson[Sensor]                // Left(.celsius(missing))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/getting-started/ Getting Started]]
 */
object DeriveJsonDecoder {

  /** The type class that Magnolia derives, [[JsonDecoder]]. Used by the derivation; call `gen` instead. */
  type Typeclass[A] = JsonDecoder[A]

  /**
   * Builds the decoder of a case class from Magnolia's description of it. Called by the code that `gen` generates; call
   * `gen` instead.
   */
  def join[A](ctx: CaseClass[JsonDecoder, A])(implicit config: JsonCodecConfiguration): JsonDecoder[A] = {
    val nameTransform =
      ctx.annotations.collectFirst { case jsonMemberNames(format) => format }.getOrElse(config.fieldNameMapping)
    val no_extra = ctx.annotations.collectFirst { case _: jsonNoExtraFields =>
      ()
    }.isDefined || !config.allowExtraFields
    if (ctx.parameters.isEmpty) new CaseObjectDecoder(ctx, no_extra)
    else {
      var splitIndex                                              = -1
      val (names, aliases): (Array[String], Array[(String, Int)]) = {
        val names          = new Array[String](ctx.parameters.size)
        val aliasesBuilder = new ArrayBuffer[(String, Int)]
        ctx.parameters.foreach {
          var idx = 0
          p =>
            names(idx) = p.annotations.collectFirst { case jsonField(name) => name }.getOrElse(nameTransform(p.label))
            aliasesBuilder ++= p.annotations.flatMap {
              case jsonAliases(alias, aliases @ _*) => (alias +: aliases).map(_ -> idx)
              case _                                => Seq.empty
            }
            idx += 1
            if (splitIndex < 0 && idx + aliasesBuilder.length > 64) splitIndex = idx - 1
        }
        val aliases       = aliasesBuilder.toArray
        val allFieldNames = names ++ aliases.map(_._1)
        if (allFieldNames.length != allFieldNames.distinct.length) {
          val typeName   = ctx.typeName.full
          val collisions = aliases
            .map(_._1)
            .distinct
            .filter(alias => names.contains(alias) || aliases.count(_._1 == alias) > 1)
            .mkString(",")
          throw new AssertionError(
            s"Field names and aliases in case class $typeName must be distinct, alias(es) $collisions collide with a field or another alias"
          )
        }
        (names, aliases)
      }
      if (splitIndex < 0) {
        new CollectionJsonDecoder[A] {
          private[this] val len                      = names.length
          private[this] val matrix                   = new StringMatrix(names, aliases)
          private[this] val spans                    = names.map(JsonError.ObjectAccess)
          private[this] val defaults                 = ctx.parameters.map(_.evaluateDefault.orNull).toArray
          private[this] lazy val tcs                 = ctx.parameters.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
          private[this] lazy val namesMap            = (names.zipWithIndex ++ aliases).toMap
          private[this] val explicitEmptyCollections =
            ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
              a.decoding
            }.getOrElse(config.explicitEmptyCollections.decoding)
          private[this] val missingValueDecoder =
            if (explicitEmptyCollections) {
              lazy val missingValueDecoders = tcs.map { d =>
                if (allowMissingValueDecoder(d)) d
                else null
              }
              (idx: Int, trace: List[JsonError]) => {
                val trace_  = spans(idx) :: trace
                val decoder = missingValueDecoders(idx)
                if (decoder eq null) Lexer.error("missing", trace_)
                decoder.unsafeDecodeMissing(trace_)
              }
            } else { (idx: Int, trace: List[JsonError]) =>
              tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
            }

          @tailrec
          private[this] def allowMissingValueDecoder(d: JsonDecoder[_]): Boolean = d match {
            case _: OptionJsonDecoder[_]     => true
            case _: CollectionJsonDecoder[_] => !explicitEmptyCollections
            case d: MappedJsonDecoder[_]     => allowMissingValueDecoder(d.underlying)
            case _                           => true
          }

          override def unsafeDecodeMissing(trace: List[JsonError]): A = {
            val ps  = new Array[Any](len)
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(new ArraySeq(ps))
          }

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')

            // TODO it would be more efficient to have a solution that didn't box
            // primitives, but Magnolia does not ealiasesxpose an API for that. Adding
            // such a feature to Magnolia is the only way to avoid this, e.g. a
            // ctx.createMutableCons that specialises on the types (with some way
            // of noting that things have been initialised), which can be called
            // to instantiate the case class. Would also require JsonDecoder to be
            // specialised.
            val ps = new Array[Any](len)
            if (Lexer.firstField(in)) {
              do {
                val idx = Lexer.field(trace, in, matrix)
                if (idx >= 0) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) =
                      if (
                        (default eq null) || in.nextNonWhitespace() != 'n' && {
                          in.retract()
                          true
                        }
                      ) tcs(idx).unsafeDecode(spans(idx) :: trace, in)
                      else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') default()
                      else Lexer.error("expected 'null'", spans(idx) :: trace)
                  } else Lexer.error("duplicate", trace)
                } else if (no_extra) Lexer.error("invalid extra field", trace)
                else Lexer.skipValue(trace, in)
              } while (Lexer.nextField(trace, in))
            }
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(new ArraySeq(ps))
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                val ps = new Array[Any](len)
                o.fields.foreach { kv =>
                  namesMap.get(kv._1) match {
                    case Some(idx) =>
                      if (ps(idx) != null) Lexer.error("duplicate", trace)
                      val default = defaults(idx)
                      ps(idx) =
                        if ((default ne null) && (kv._2 eq Json.Null)) default()
                        else tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, kv._2)
                    case _ =>
                      if (no_extra) Lexer.error("invalid extra field", trace)
                  }
                }
                var idx = 0
                while (idx < len) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) =
                      if (default ne null) default()
                      else missingValueDecoder(idx, trace)
                  }
                  idx += 1
                }
                ctx.rawConstruct(new ArraySeq(ps))
              case _ => Lexer.error("expected object", trace)
            }
        }
      } else {
        val (names1, names2) = names.splitAt(splitIndex)
        val aliases1         = aliases.filter(kv => kv._2 <= splitIndex)
        val aliases2         = aliases.collect {
          case (k, v) if v > splitIndex =>
            (k, v - splitIndex)
        }
        new CollectionJsonDecoder[A] {
          private[this] val len                      = names.length
          private[this] val matrix1                  = new StringMatrix(names1, aliases1)
          private[this] val matrix2                  = new StringMatrix(names2, aliases2)
          private[this] val spans                    = names.map(JsonError.ObjectAccess)
          private[this] val defaults                 = ctx.parameters.map(_.evaluateDefault.orNull).toArray
          private[this] lazy val tcs                 = ctx.parameters.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
          private[this] lazy val namesMap            = (names.zipWithIndex ++ aliases).toMap
          private[this] val explicitEmptyCollections =
            ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
              a.decoding
            }.getOrElse(config.explicitEmptyCollections.decoding)
          private[this] val missingValueDecoder =
            if (explicitEmptyCollections) {
              lazy val missingValueDecoders = tcs.map { d =>
                if (allowMissingValueDecoder(d)) d
                else null
              }
              (idx: Int, trace: List[JsonError]) => {
                val trace_  = spans(idx) :: trace
                val decoder = missingValueDecoders(idx)
                if (decoder eq null) Lexer.error("missing", trace_)
                decoder.unsafeDecodeMissing(trace_)
              }
            } else { (idx: Int, trace: List[JsonError]) =>
              tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
            }

          @tailrec
          private[this] def allowMissingValueDecoder(d: JsonDecoder[_]): Boolean = d match {
            case _: OptionJsonDecoder[_]     => true
            case _: CollectionJsonDecoder[_] => !explicitEmptyCollections
            case d: MappedJsonDecoder[_]     => allowMissingValueDecoder(d.underlying)
            case _                           => true
          }

          override def unsafeDecodeMissing(trace: List[JsonError]): A = {
            val ps  = new Array[Any](len)
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(new ArraySeq(ps))
          }

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')

            // TODO it would be more efficient to have a solution that didn't box
            // primitives, but Magnolia does not ealiasesxpose an API for that. Adding
            // such a feature to Magnolia is the only way to avoid this, e.g. a
            // ctx.createMutableCons that specialises on the types (with some way
            // of noting that things have been initialised), which can be called
            // to instantiate the case class. Would also require JsonDecoder to be
            // specialised.
            val ps = new Array[Any](len)
            if (Lexer.firstField(in)) {
              do {
                val idx = Lexer.field128(trace, in, matrix1, matrix2)
                if (idx >= 0) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) =
                      if (
                        (default eq null) || in.nextNonWhitespace() != 'n' && {
                          in.retract()
                          true
                        }
                      ) tcs(idx).unsafeDecode(spans(idx) :: trace, in)
                      else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') default()
                      else Lexer.error("expected 'null'", spans(idx) :: trace)
                  } else Lexer.error("duplicate", trace)
                } else if (no_extra) Lexer.error("invalid extra field", trace)
                else Lexer.skipValue(trace, in)
              } while (Lexer.nextField(trace, in))
            }
            var idx = 0
            while (idx < len) {
              if (ps(idx) == null) {
                val default = defaults(idx)
                ps(idx) =
                  if (default ne null) default()
                  else missingValueDecoder(idx, trace)
              }
              idx += 1
            }
            ctx.rawConstruct(new ArraySeq(ps))
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                val ps = new Array[Any](len)
                o.fields.foreach { kv =>
                  namesMap.get(kv._1) match {
                    case Some(idx) =>
                      if (ps(idx) != null) Lexer.error("duplicate", trace)
                      val default = defaults(idx)
                      ps(idx) =
                        if ((default ne null) && (kv._2 eq Json.Null)) default()
                        else tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, kv._2)
                    case _ =>
                      if (no_extra) Lexer.error("invalid extra field", trace)
                  }
                }
                var idx = 0
                while (idx < len) {
                  if (ps(idx) == null) {
                    val default = defaults(idx)
                    ps(idx) =
                      if (default ne null) default()
                      else missingValueDecoder(idx, trace)
                  }
                  idx += 1
                }
                ctx.rawConstruct(new ArraySeq(ps))
              case _ => Lexer.error("expected object", trace)
            }
        }
      }
    }
  }

  /**
   * Builds the decoder of a sealed trait from the decoders of its cases. Called by the code that `gen` generates; call
   * `gen` instead.
   */
  def split[A](ctx: SealedTrait[JsonDecoder, A])(implicit config: JsonCodecConfiguration): JsonDecoder[A] = {
    val jsonHintFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names = ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeName.short))
    }.toArray
    if (names.distinct.length != names.length) {
      val typeName   = ctx.typeName.full
      val collisions = names.groupBy(identity).collect { case (n, ns) if ns.lengthCompare(1) > 0 => n }.mkString(",")
      throw new AssertionError(s"Case names in ADT $typeName must be distinct, name(s) $collisions are duplicated")
    }
    val (names1, names2) = names.splitAt(64)
    val matrix1          = new StringMatrix(names1)
    val matrix2          =
      if (names2.isEmpty) null
      else new StringMatrix(names2)
    lazy val tcs      = ctx.subtypes.map(_.typeclass).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    lazy val namesMap = names.zipWithIndex.toMap
    val discrim       =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)
    lazy val isEnumeration = config.enumValuesAsStrings &&
      ctx.subtypes.forall(_.typeclass.isInstanceOf[CaseObjectDecoder[JsonDecoder, _]])
    if (discrim.isEmpty && isEnumeration) {
      if (names.length <= 64) {
        new JsonDecoder.AbstractJsonDecoder[A] {
          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val idx = Lexer.enumeration(trace, in, matrix1)
            if (idx >= 0) tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
            else Lexer.error("invalid enumeration value", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case s: Json.Str =>
                namesMap.get(s.value) match {
                  case Some(idx) => tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
                  case _         => Lexer.error("invalid enumeration value", trace)
                }
              case _ => Lexer.error("expected string", trace)
            }
        }
      } else {
        new JsonDecoder.AbstractJsonDecoder[A] {
          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val idx = Lexer.enumeration128(trace, in, matrix1, matrix2)
            if (idx >= 0) tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
            else Lexer.error("invalid enumeration value", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case s: Json.Str =>
                namesMap.get(s.value) match {
                  case Some(idx) => tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
                  case _         => Lexer.error("invalid enumeration value", trace)
                }
              case _ => Lexer.error("expected string", trace)
            }
        }
      }
    } else if (discrim.isEmpty) {
      // We're not allowing extra fields in this encoding
      if (names.length <= 64) {
        new JsonDecoder.AbstractJsonDecoder[A] {
          private[this] val spans = names.map(JsonError.ObjectAccess)

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            if (Lexer.firstField(in)) {
              val idx = Lexer.field(trace, in, matrix1)
              if (idx >= 0) {
                val a = tcs(idx).unsafeDecode(spans(idx) :: trace, in).asInstanceOf[A]
                Lexer.char(trace, in, '}')
                a
              } else Lexer.error("invalid disambiguator", trace)
            } else Lexer.error("expected non-empty object", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj if o.fields.length == 1 =>
                val kv = o.fields(0)
                namesMap.get(kv._1) match {
                  case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, kv._2).asInstanceOf[A]
                  case _         => Lexer.error("invalid disambiguator", trace)
                }
              case _ => Lexer.error("expected single field object", trace)
            }
        }
      } else {
        new JsonDecoder.AbstractJsonDecoder[A] {
          private[this] val spans = names.map(JsonError.ObjectAccess)

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            if (Lexer.firstField(in)) {
              val idx = Lexer.field128(trace, in, matrix1, matrix2)
              if (idx >= 0) {
                val a = tcs(idx).unsafeDecode(spans(idx) :: trace, in).asInstanceOf[A]
                Lexer.char(trace, in, '}')
                a
              } else Lexer.error("invalid disambiguator", trace)
            } else Lexer.error("expected non-empty object", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj if o.fields.length == 1 =>
                val kv = o.fields(0)
                namesMap.get(kv._1) match {
                  case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, kv._2).asInstanceOf[A]
                  case _         => Lexer.error("invalid disambiguator", trace)
                }
              case _ => Lexer.error("expected single field object", trace)
            }
        }
      }
    } else {
      if (names.length <= 64) {
        new JsonDecoder.AbstractJsonDecoder[A] {
          private[this] val hintfield  = discrim.get
          private[this] val hintmatrix = new StringMatrix(Array(hintfield))
          private[this] val spans      = names.map(JsonError.Message)

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val in_ = RecordingReader(in)
            Lexer.char(trace, in_, '{')
            if (Lexer.firstField(in_)) {
              do {
                if (Lexer.field(trace, in_, hintmatrix) >= 0) {
                  val idx = Lexer.enumeration(trace, in_, matrix1)
                  if (idx >= 0) {
                    in_.rewind()
                    return tcs(idx).unsafeDecode(spans(idx) :: trace, in_).asInstanceOf[A]
                  } else Lexer.error("invalid disambiguator", trace)
                } else Lexer.skipValue(trace, in_)
              } while (Lexer.nextField(trace, in_))
            }
            Lexer.error(s"missing hint '$hintfield'", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                o.fields.collectFirst {
                  case kv if kv._1 == hintfield && kv._2.isInstanceOf[Json.Str] =>
                    kv._2.asInstanceOf[Json.Str].value
                } match {
                  case Some(name) =>
                    namesMap.get(name) match {
                      case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, json).asInstanceOf[A]
                      case _         => Lexer.error("invalid disambiguator", trace)
                    }
                  case _ => Lexer.error(s"missing hint '$hintfield'", trace)
                }
              case _ => Lexer.error("expected object", trace)
            }
        }
      } else {
        new JsonDecoder.AbstractJsonDecoder[A] {
          private[this] val hintfield  = discrim.get
          private[this] val hintmatrix = new StringMatrix(Array(hintfield))
          private[this] val spans      = names.map(JsonError.Message)

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val in_ = RecordingReader(in)
            Lexer.char(trace, in_, '{')
            if (Lexer.firstField(in_)) {
              do {
                if (Lexer.field(trace, in_, hintmatrix) >= 0) {
                  val idx = Lexer.enumeration128(trace, in_, matrix1, matrix2)
                  if (idx >= 0) {
                    in_.rewind()
                    return tcs(idx).unsafeDecode(spans(idx) :: trace, in_).asInstanceOf[A]
                  } else Lexer.error("invalid disambiguator", trace)
                } else Lexer.skipValue(trace, in_)
              } while (Lexer.nextField(trace, in_))
            }
            Lexer.error(s"missing hint '$hintfield'", trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case o: Json.Obj =>
                o.fields.collectFirst {
                  case kv if kv._1 == hintfield && kv._2.isInstanceOf[Json.Str] =>
                    kv._2.asInstanceOf[Json.Str].value
                } match {
                  case Some(name) =>
                    namesMap.get(name) match {
                      case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, json).asInstanceOf[A]
                      case _         => Lexer.error("invalid disambiguator", trace)
                    }
                  case _ => Lexer.error(s"missing hint '$hintfield'", trace)
                }
              case _ => Lexer.error("expected object", trace)
            }
        }
      }
    }
  }

  /**
   * Derives a `JsonDecoder[A]` for the case class or sealed trait `A`, with the [[JsonCodecConfiguration]] in implicit
   * scope or else `JsonCodecConfiguration.default`. A missing field gets its default value, `None` for an `Option`, an
   * empty collection when `explicitEmptyCollections.decoding` is `false`, and otherwise fails with `.name(missing)`,
   * where `name` is the JSON name of the field. A `null` for a field with a default value also gives the default.
   * Unknown keys are skipped unless [[jsonNoExtraFields]] or `allowExtraFields = false` applies. A case class without
   * fields is read from any JSON value in text (only from an empty object with [[jsonNoExtraFields]]), and from an
   * object or `null` in a JSON AST.
   *
   * Throws a `java.lang.AssertionError` when the decoder is created, if two fields or aliases of a case class have the
   * same JSON name, or two cases of a sealed trait have the same type hint.
   */
  def gen[A]: JsonDecoder[A] = macro Magnolia.gen[A]
}

/**
 * Derives a [[JsonEncoder]] for a case class or a sealed trait at compile time. A case class is written as a JSON
 * object with one key per field, in declaration order. A case of a sealed trait is wrapped in an object whose only key
 * is the type hint (the short type name by default). On Scala 3, where `enumValuesAsStrings` is `true` by default, a
 * sealed trait whose cases are all case objects is written as a JSON string such as `"Red"`. The annotations of the
 * type, such as [[jsonField]] or [[jsonDiscriminator]], and the [[JsonCodecConfiguration]] in implicit scope change
 * these defaults.
 *
 * On Scala 2, the derivation uses Magnolia, and every field type needs an encoder in implicit scope. On Scala 3, it
 * uses the `Mirror` of the type, and derives an encoder for a field whose type is a case class or a sealed trait
 * without one. A known bug on Scala 3: a field of type `Option[Inner]`, where `Inner` has no encoder, is derived as a
 * sealed trait and written as `{"Some":{"value":...}}` or `{"None":{}}`, and a `List[Inner]` field fails to compile;
 * give `Inner` its own encoder.
 *
 * {{{
 * final case class Parcel(id: String, weightKg: Double, note: Option[String])
 *
 * object Parcel {
 *   implicit val encoder: JsonEncoder[Parcel] = DeriveJsonEncoder.gen[Parcel]
 * }
 *
 * Parcel("p1", 2.5, None).toJson // {"id":"p1","weightKg":2.5}
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/getting-started/ Getting Started]]
 */
object DeriveJsonEncoder {
  private lazy val caseObjectEncoder: JsonEncoder[Any] = new JsonEncoder.AbstractJsonEncoder[Any] {
    override def isEmpty(a: Any): Boolean = true

    def unsafeEncode(a: Any, indent: Option[Int], out: Write): Unit = out.write("{}")

    override final def toJsonAST(a: Any): Either[String, Json] = new Right(Json.Obj.empty)
  }

  /** The type class that Magnolia derives, [[JsonEncoder]]. Used by the derivation; call `gen` instead. */
  type Typeclass[A] = JsonEncoder[A]

  /**
   * Builds the encoder of a case class from Magnolia's description of it. Called by the code that `gen` generates; call
   * `gen` instead.
   */
  def join[A](ctx: CaseClass[JsonEncoder, A])(implicit config: JsonCodecConfiguration): JsonEncoder[A] =
    if (ctx.parameters.isEmpty) caseObjectEncoder.narrow[A]
    else {
      val nameTransform =
        ctx.annotations.collectFirst { case jsonMemberNames(format) => format }.getOrElse(config.fieldNameMapping)
      val explicitNulls            = config.explicitNulls || ctx.annotations.exists(_.isInstanceOf[jsonExplicitNull])
      val explicitEmptyCollections = ctx.annotations.collectFirst { case a: jsonExplicitEmptyCollections => a.encoding }
        .getOrElse(config.explicitEmptyCollections.encoding)
      val params = ctx.parameters.filter(p => p.annotations.collectFirst { case _: jsonExclude => () }.isEmpty).toArray
      new JsonEncoder.AbstractJsonEncoder[A] {
        private[this] lazy val fields = params.map { p =>
          FieldEncoder(
            p = p,
            name = p.annotations.collectFirst { case jsonField(name) => name }.getOrElse(nameTransform(p.label)),
            encoder = p.typeclass.asInstanceOf[JsonEncoder[Any]],
            withExplicitNulls = explicitNulls || p.annotations.exists(_.isInstanceOf[jsonExplicitNull]),
            withExplicitEmptyCollections = p.annotations.collectFirst { case a: jsonExplicitEmptyCollections =>
              a.encoding
            }.getOrElse(explicitEmptyCollections)
          )
        }

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          val fields  = this.fields
          var idx     = 0
          var comma   = false
          while (idx < fields.length) {
            val field = fields(idx)
            idx += 1
            val p = field.p.dereference(a)
            if (field.skip(p)) ()
            else {
              if (comma) out.write(',')
              else comma = true
              JsonEncoder.pad(indent_, out)
              out.write(if (indent eq None) field.encodedName else field.prettyEncodedName)
              field.encoder.unsafeEncode(p, indent_, out)
            }
          }
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          val fields = this.fields
          var buf    = new Array[(String, Json)](fields.length)
          var i, idx = 0
          while (idx < fields.length) {
            val field = fields(idx)
            idx += 1
            val p = field.p.dereference(a)
            if (field.skip(p)) ()
            else {
              field.encoder.toJsonAST(p) match {
                case Right(value) =>
                  buf(i) = (field.name, value)
                  i += 1
                case left =>
                  return left
              }
            }
          }
          if (i != buf.length) buf = java.util.Arrays.copyOf(buf, i)
          new Right(Json.Obj(Chunk.fromArray(buf)))
        }
      }
    }

  /**
   * Builds the encoder of a sealed trait from the encoders of its cases. Called by the code that `gen` generates; call
   * `gen` instead.
   */
  def split[A](ctx: SealedTrait[JsonEncoder, A])(implicit config: JsonCodecConfiguration): JsonEncoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names: Array[String] = ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeName.short))
    }.toArray
    val encodedNames: Array[String] = names.map(name => JsonEncoder.string.encodeJson(name, None).toString)
    lazy val tcs                    = ctx.subtypes.map(_.typeclass).toArray.asInstanceOf[Array[JsonEncoder[Any]]]
    val discrim                     =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)
    lazy val isEnumeration = config.enumValuesAsStrings &&
      ctx.subtypes.forall(_.typeclass == caseObjectEncoder)
    if (discrim.isEmpty && isEnumeration) {
      new JsonEncoder.AbstractJsonEncoder[A] {
        private[this] val casts = ctx.subtypes.map(_.cast).toArray

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          out.write(encodedNames(idx))
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          new Right(new Json.Str(names(idx)))
        }
      }
    } else if (discrim.isEmpty) {
      new JsonEncoder.AbstractJsonEncoder[A] {
        private[this] val casts = ctx.subtypes.map(_.cast).toArray

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          out.write(encodedNames(idx))
          if (indent eq None) out.write(':')
          else out.write(" : ")
          tcs(idx).unsafeEncode(a, indent_, out)
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override def toJsonAST(a: A): Either[String, Json] = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          tcs(idx).toJsonAST(a).map(inner => new Json.Obj(Chunk(names(idx) -> inner)))
        }
      }
    } else {
      new JsonEncoder.AbstractJsonEncoder[A] {
        private[this] val casts                = ctx.subtypes.map(_.cast).toArray
        private[this] val hintFieldName        = discrim.get
        private[this] val encodedHintFieldName = JsonEncoder.string.encodeJson(hintFieldName, None).toString

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          out.write('{')
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          out.write(encodedHintFieldName)
          if (indent eq None) out.write(':')
          else out.write(" : ")
          out.write(encodedNames(idx))
          // pretty printing: a case without fields gets its closing `}` right after the hint, on the same line
          tcs(idx).unsafeEncode(a, indent, new NestedWriter(out, indent_))
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          var idx = 0
          while (!casts(idx).isDefinedAt(a)) idx += 1
          tcs(idx).toJsonAST(a).flatMap {
            case o: Json.Obj =>
              val hintField = hintFieldName -> new Json.Str(names(idx))
              new Right(new Json.Obj(hintField +: o.fields)) // hint field is always first
            case _ =>
              new Left("expected object")
          }
        }
      }
    }
  }

  /**
   * Derives a `JsonEncoder[A]` for the case class or sealed trait `A`, with the [[JsonCodecConfiguration]] in implicit
   * scope or else `JsonCodecConfiguration.default`. By default, a field that is `None` is left out and an empty
   * collection is written; [[jsonExplicitNull]] and [[jsonExplicitEmptyCollections]] change this. Fields with
   * [[jsonExclude]] are left out.
   */
  def gen[A]: JsonEncoder[A] = macro Magnolia.gen[A]
}

// backcompat for 2.12, otherwise we'd use ArraySeq.unsafeWrapArray
private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
  @inline def apply(i: Int): Any = p(i)
  @inline def length: Int        = p.length
}

// intercepts the first `{` of a nested writer and discards it. We also need to
// inject a `,` unless an empty object `{}` has been written.
private[this] final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
  private[this] var state = 2

  @inline def write(c: Char): Unit =
    if (state == 0) out.write(c)
    else nonZeroStateWrite(c)

  @noinline private[this] def nonZeroStateWrite(c: Char): Unit =
    if (c != ' ' && c != '\n') {
      if (state == 2) {
        if (c == '{') state = 1
      } else {
        state = 0
        if (c != '}') {
          out.write(',')
          JsonEncoder.pad(indent, out)
        }
        out.write(c)
      }
    }

  @inline def write(s: String): Unit =
    if (state == 0) out.write(s)
    else nonZeroStateWrite(s)

  @noinline private[this] def nonZeroStateWrite(s: String): Unit = {
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c != ' ' && c != '\n') {
        if (state == 2) {
          if (c == '{') state = 1
        } else {
          state = 0
          if (c != '}') {
            out.write(',')
            JsonEncoder.pad(indent, out)
          }
          while (i < s.length) {
            out.write(s.charAt(i))
            i += 1
          }
          return
        }
      }
      i += 1
    }
  }

  @inline override def write(cs: Array[Char], from: Int, to: Int): Unit =
    if (state == 0) out.write(cs, from, to)
    else nonZeroStateWrite(cs, from, to)

  @noinline def nonZeroStateWrite(cs: Array[Char], from: Int, to: Int): Unit = {
    var i = from
    while (i < to) {
      val c = cs(i)
      if (c != ' ' && c != '\n') {
        if (state == 2) {
          if (c == '{') state = 1
        } else {
          state = 0
          if (c != '}') {
            out.write(',')
            JsonEncoder.pad(indent, out)
          }
          out.write(cs, i, to)
          return
        }
      }
      i += 1
    }
  }

  @inline override def write(c1: Char, c2: Char): Unit =
    if (state == 0) out.write(c1, c2)
    else {
      nonZeroStateWrite(c1)
      nonZeroStateWrite(c2)
    }

  @inline override def write(c1: Char, c2: Char, c3: Char): Unit =
    if (state == 0) out.write(c1, c2, c3)
    else {
      nonZeroStateWrite(c1)
      nonZeroStateWrite(c2)
      nonZeroStateWrite(c3)
    }

  @inline override def write(c1: Char, c2: Char, c3: Char, c4: Char): Unit =
    if (state == 0) out.write(c1, c2, c3, c4)
    else {
      nonZeroStateWrite(c1)
      nonZeroStateWrite(c2)
      nonZeroStateWrite(c3)
      nonZeroStateWrite(c4)
    }

  @inline override def write(c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): Unit =
    if (state == 0) out.write(c1, c2, c3, c4, c5)
    else {
      nonZeroStateWrite(c1)
      nonZeroStateWrite(c2)
      nonZeroStateWrite(c3)
      nonZeroStateWrite(c4)
      nonZeroStateWrite(c5)
    }

  @inline override def write(s: Short): Unit =
    if (state == 0) out.write(s)
    else {
      nonZeroStateWrite((s & 0xff).toChar)
      nonZeroStateWrite((s >> 8).toChar)
    }

  @inline override def write(s1: Short, s2: Short): Unit =
    if (state == 0) out.write(s1, s2)
    else {
      nonZeroStateWrite((s1 & 0xff).toChar)
      nonZeroStateWrite((s1 >> 8).toChar)
      nonZeroStateWrite((s2 & 0xff).toChar)
      nonZeroStateWrite((s2 >> 8).toChar)
    }

  @inline override def write(s1: Short, s2: Short, s3: Short): Unit =
    if (state == 0) out.write(s1, s2, s3)
    else {
      nonZeroStateWrite((s1 & 0xff).toChar)
      nonZeroStateWrite((s1 >> 8).toChar)
      nonZeroStateWrite((s2 & 0xff).toChar)
      nonZeroStateWrite((s2 >> 8).toChar)
      nonZeroStateWrite((s3 & 0xff).toChar)
      nonZeroStateWrite((s3 >> 8).toChar)
    }

  @inline override def write(s1: Short, s2: Short, s3: Short, s4: Short): Unit =
    if (state == 0) out.write(s1, s2, s3, s4)
    else {
      nonZeroStateWrite((s1 & 0xff).toChar)
      nonZeroStateWrite((s1 >> 8).toChar)
      nonZeroStateWrite((s2 & 0xff).toChar)
      nonZeroStateWrite((s2 >> 8).toChar)
      nonZeroStateWrite((s3 & 0xff).toChar)
      nonZeroStateWrite((s3 >> 8).toChar)
      nonZeroStateWrite((s4 & 0xff).toChar)
      nonZeroStateWrite((s4 >> 8).toChar)
    }
}

/**
 * Derives a [[JsonCodec]] for a case class or a sealed trait at compile time: an encoder and a decoder that read and
 * write the same JSON. It reads the same annotations and [[JsonCodecConfiguration]] as [[DeriveJsonEncoder]] and
 * [[DeriveJsonDecoder]]. A case class is written as an object with one key per field; a case of a sealed trait is
 * wrapped in an object whose only key is the type hint. On Scala 3, where `enumValuesAsStrings` is `true` by default, a
 * sealed trait whose cases are all case objects is written as a JSON string such as `"Red"`.
 *
 * On Scala 2, put the codec of a recursive type in an `implicit lazy val`.
 *
 * {{{
 * sealed trait Shape
 * final case class Circle(radius: Double)                 extends Shape
 * final case class Rectangle(width: Double, height: Double) extends Shape
 *
 * object Shape {
 *   implicit val codec: JsonCodec[Shape] = DeriveJsonCodec.gen[Shape]
 * }
 *
 * (Circle(1.5): Shape).toJson // {"Circle":{"radius":1.5}}
 * """{"Rectangle":{"width":2.0,"height":3.0}}""".fromJson[Shape] // Right(Rectangle(2.0,3.0))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-json/getting-started/ Getting Started]]
 */
object DeriveJsonCodec {

  /**
   * Derives a `JsonCodec[A]` for the case class or sealed trait `A`, with the [[JsonCodecConfiguration]] in implicit
   * scope or else `JsonCodecConfiguration.default`. The result behaves as
   * `JsonCodec(DeriveJsonEncoder.gen[A], DeriveJsonDecoder.gen[A])`.
   *
   * Throws a `java.lang.AssertionError` when the codec is created, if two fields or aliases of a case class have the
   * same JSON name, or two cases of a sealed trait have the same type hint.
   */
  def gen[A]: JsonCodec[A] = macro genBoth[A]

  import scala.reflect.macros._

  /**
   * The macro implementation of `gen` on Scala 2: it expands to
   * `JsonCodec(DeriveJsonEncoder.gen[T], DeriveJsonDecoder.gen[T])`. Call `gen` instead.
   */
  def genBoth[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val encoder = q"_root_.zio.json.DeriveJsonEncoder.gen[${c.weakTypeOf[T]}]"
    val decoder = q"_root_.zio.json.DeriveJsonDecoder.gen[${c.weakTypeOf[T]}]"

    q"_root_.zio.json.JsonCodec($encoder, $decoder)"
  }
}
