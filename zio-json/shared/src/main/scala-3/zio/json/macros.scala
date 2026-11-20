package zio.json

import scala.annotation.*
import scala.compiletime.*
import zio.Chunk
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.ast.Json
import zio.json.internal.*

import scala.annotation.*
import scala.collection.mutable
import scala.compiletime.*
import scala.deriving.Mirror
import scala.language.experimental.macros
import scala.quoted.*
import scala.reflect.*

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
sealed trait JsonMemberFormat              extends (String => String)
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

  /**
   * ~~Stolen~~ Borrowed from jsoniter-scala by Andriy Plokhotnyuk (he even granted permission for this, imagine that!)
   */

  import java.lang.Character.*

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
final case class jsonExplicitNull() extends Annotation

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

// =======================================================================================
// Consolidated metadata case classes for single-pass extraction
// =======================================================================================
private[json] final case class ProductMeta(
  fieldAnnotations: Map[String, List[Any]],
  typeAnnotations: List[Any],
  defaults: List[Option[() => Any]],
  excludedFields: Set[String],
  hasCaseFields: Boolean,
  isModule: Boolean,
  isStaticEnum: Boolean,
  typeName: String
)

private[json] final case class SumMeta(
  typeAnnotations: List[Any],
  subtypeInfo: Array[(String, Boolean, Boolean, Boolean)],
  typeName: String
)

// =======================================================================================
// Macro helpers for annotation and metadata extraction.
// Most of these macros only extract data; the summon* ones also emit
// DeriveJsonEncoder.gen / DeriveJsonDecoder.gen calls for types without an instance.
// =======================================================================================
private[json] object MacroHelpers {

  private def extractJsonHintName(using Quotes)(annotation: quotes.reflect.Term): Option[Expr[String]] = {
    annotation.asExpr match {
      case '{ new zio.json.jsonHint($name: String) } => Some(name)
      case _                                         => None
    }
  }

  // =====================================================================================
  // Consolidated single-pass metadata extraction macros
  // =====================================================================================

  inline def extractProductMeta[T]: ProductMeta = ${ extractProductMetaImpl[T] }

  private def extractProductMetaImpl[T: Type](using Quotes): Expr[ProductMeta] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    // Type annotations
    val typeAnns = Expr.ofList(
      sym.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
      }.map(_.asExpr)
    )

    // Field annotations
    val paramAnns = sym.primaryConstructor.paramSymss.flatMap {
      _.filter(_.isValDef).map { s =>
        val name = Expr(s.name)
        val anns = Expr.ofList(
          s.annotations.filter { a =>
            a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }.map(_.asExpr)
        )
        '{ ($name, $anns) }
      }
    }
    val fieldAnnsExpr = '{ ${ Expr.ofList(paramAnns) }.toMap }

    // Default values
    val numFields = sym.caseFields.length
    val typeArgs = tpe.dealias match {
      case AppliedType(_, args) => args
      case _                   => Nil
    }
    val defaults = (1 to numFields).toList.map { i =>
      val defaultMethodName = s"$$lessinit$$greater$$default$$$i"
      sym.companionClass
        .declaredMethod(defaultMethodName)
        .headOption
        .map { defaultMethod =>
          val select = Select(Ref(sym.companionModule), defaultMethod)
          val default = if (typeArgs.nonEmpty && defaultMethod.paramSymss.exists(_.exists(_.isTypeParam)))
            TypeApply(select, typeArgs.map(Inferred(_))).asExpr
          else
            select.asExpr
          '{ Some(() => $default) }
        }
        .getOrElse('{ None })
    }
    val defaultsExpr = Expr.ofList(defaults)

    // Excluded fields
    val fieldSyms = sym.primaryConstructor.paramSymss.flatMap {
      _.filter(_.isValDef).map { s =>
        s.name -> s.annotations.map(_.asExpr)
      }
    }
    val excluded = fieldSyms.collect {
      case (name, anns) if anns.exists {
            case '{ new zio.json.jsonExclude() } => true
            case _                               => false
          } =>
        Expr(name)
    }
    val excludedExpr = '{ Set(${ Expr.ofSeq(excluded) }*) }

    // Boolean flags
    val hasCaseFieldsExpr = Expr(sym.caseFields.nonEmpty)
    val isModuleExpr = Expr(sym.flags.is(Flags.Module))
    val isStaticEnumExpr = Expr(sym.flags.is(Flags.Enum) && sym.flags.is(Flags.JavaStatic))

    // Type name
    val typeNameExpr = Expr(sym.fullName)

    '{
      ProductMeta(
        fieldAnnotations = $fieldAnnsExpr,
        typeAnnotations = $typeAnns,
        defaults = $defaultsExpr,
        excludedFields = $excludedExpr,
        hasCaseFields = $hasCaseFieldsExpr,
        isModule = $isModuleExpr,
        isStaticEnum = $isStaticEnumExpr,
        typeName = $typeNameExpr
      )
    }
  }

  inline def extractSumMeta[T]: SumMeta = ${ extractSumMetaImpl[T] }

  private def extractSumMetaImpl[T: Type](using Quotes): Expr[SumMeta] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    // Type annotations
    val typeAnns = Expr.ofList(
      sym.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
      }.map(_.asExpr)
    )

    // Subtype info
    def leafTypes(of: quotes.reflect.Symbol): List[quotes.reflect.Symbol] =
      of.children.flatMap { child =>
        if (child.flags.is(Flags.Case)) List(child)
        else leafTypes(child)
      }

    val subTypes = leafTypes(sym)
    val infos = subTypes.map { subSym =>
      val hintAnnotation = subSym.annotations.collectFirst(Function.unlift(extractJsonHintName))
      val hasHint = hintAnnotation.isDefined
      val hintName = hintAnnotation.getOrElse {
        Expr(
          if (subSym.flags.is(Flags.Module)) subSym.companionModule.name
          else subSym.name
        )
      }
      val isObj = subSym.flags.is(Flags.Module) ||
        (subSym.flags.is(Flags.Enum) && subSym.flags.is(Flags.JavaStatic)) ||
        (subSym.flags.is(Flags.Enum) && subSym.flags.is(Flags.Case) && subSym.caseFields.isEmpty && !subSym.flags.is(
          Flags.Trait
        ))
      val hasFields = subSym.caseFields.nonEmpty
      (hintName, Expr(isObj), Expr(hasFields), Expr(hasHint))
    }
    val subtypeInfoExpr = '{
      Array(${
        Expr.ofSeq(infos.map { case (h, o, f, hh) => '{ ($h, $o, $f, $hh) } })
      }*)
    }

    // Type name
    val typeNameExpr = Expr(sym.fullName)

    '{
      SumMeta(
        typeAnnotations = $typeAnns,
        subtypeInfo = $subtypeInfoExpr,
        typeName = $typeNameExpr
      )
    }
  }

  // =====================================================================================
  // Individual macro helpers (kept for backward compatibility and standalone use)
  // =====================================================================================

  // Type annotations (class-level)
  inline def typeAnnotations[T]: List[Any] = ${ typeAnnotationsImpl[T] }

  private def typeAnnotationsImpl[T: Type](using Quotes): Expr[List[Any]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    Expr.ofList(
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
      }
        .map(_.asExpr)
    )
  }

  // Field annotations: List[(fieldName, List[annotation])]
  inline def fieldAnnotations[T]: List[(String, List[Any])] = ${ fieldAnnotationsImpl[T] }

  private def fieldAnnotationsImpl[T: Type](using Quotes): Expr[List[(String, List[Any])]] = {
    import quotes.reflect.*
    val tpe       = TypeRepr.of[T]
    val paramAnns = tpe.typeSymbol.primaryConstructor.paramSymss.flatMap {
      _.filter(_.isValDef).map { s =>
        val name = Expr(s.name)
        val anns = Expr.ofList(
          s.annotations.filter { a =>
            a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
          }
            .map(_.asExpr)
        )
        '{ ($name, $anns) }
      }
    }
    Expr.ofList(paramAnns)
  }

  // Default values: List[Option[() => Any]]
  inline def defaultValues[T]: List[Option[() => Any]] = ${ defaultValuesImpl[T] }

  private def defaultValuesImpl[T: Type](using Quotes): Expr[List[Option[() => Any]]] = {
    import quotes.reflect.*
    val tpe       = TypeRepr.of[T]
    val sym       = tpe.typeSymbol
    val numFields = sym.caseFields.length
    val typeArgs  = tpe.dealias match {
      case AppliedType(_, args) => args
      case _                   => Nil
    }
    val defaults  = (1 to numFields).toList.map { i =>
      val defaultMethodName = s"$$lessinit$$greater$$default$$$i"
      sym.companionClass
        .declaredMethod(defaultMethodName)
        .headOption
        .map { defaultMethod =>
          val select = Select(Ref(sym.companionModule), defaultMethod)
          val default = if (typeArgs.nonEmpty && defaultMethod.paramSymss.exists(_.exists(_.isTypeParam)))
            TypeApply(select, typeArgs.map(Inferred(_))).asExpr
          else
            select.asExpr
          '{ Some(() => $default) }
        }
        .getOrElse('{ None })
    }
    Expr.ofList(defaults)
  }

  // Is this type a case object / module?
  inline def isModule[T]: Boolean = ${ isModuleImpl[T] }

  private def isModuleImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))
  }

  // Is this type a static enum case (like an enum case with no params)?
  inline def isStaticEnum[T]: Boolean = ${ isStaticEnumImpl[T] }

  private def isStaticEnumImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    Expr(sym.flags.is(Flags.Enum) && sym.flags.is(Flags.JavaStatic))
  }

  // Has case fields?
  inline def hasCaseFields[T]: Boolean = ${ hasCaseFieldsImpl[T] }

  private def hasCaseFieldsImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.caseFields.nonEmpty)
  }

  // Get the full type name
  inline def fullTypeName[T]: String = ${ fullTypeNameImpl[T] }

  private def fullTypeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.fullName)
  }

  // Get the leaf subtype names (for sum types): Array[(hintName, isObject, hasCaseFields, hasJsonHintAnnotation)]
  inline def subtypeInfo[T]: Array[(String, Boolean, Boolean, Boolean)] = ${ subtypeInfoImpl[T] }

  private def subtypeInfoImpl[T: Type](using Quotes): Expr[Array[(String, Boolean, Boolean, Boolean)]] = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol

    def leafTypes(of: quotes.reflect.Symbol): List[quotes.reflect.Symbol] =
      of.children.flatMap { child =>
        if (child.flags.is(Flags.Case)) List(child)
        else leafTypes(child)
      }

    val subTypes = leafTypes(sym)
    val infos    = subTypes.map { subSym =>
      val hintAnnotation = subSym.annotations.collectFirst(Function.unlift(extractJsonHintName))
      val hasHint  = hintAnnotation.isDefined
      val hintName = hintAnnotation.getOrElse {
        Expr(
          if (subSym.flags.is(Flags.Module)) subSym.companionModule.name
          else subSym.name
        )
      }
      val isObj = subSym.flags.is(Flags.Module) ||
        (subSym.flags.is(Flags.Enum) && subSym.flags.is(Flags.JavaStatic)) ||
        (subSym.flags.is(Flags.Enum) && subSym.flags.is(Flags.Case) && subSym.caseFields.isEmpty && !subSym.flags.is(
          Flags.Trait
        ))
      val hasFields = subSym.caseFields.nonEmpty
      (hintName, Expr(isObj), Expr(hasFields), Expr(hasHint))
    }
    '{
      Array(${
        Expr.ofSeq(infos.map { case (h, o, f, hh) => '{ ($h, $o, $f, $hh) } })
      }*)
    }
  }

  // Check if excluded field
  inline def excludedFields[T]: Set[String] = ${ excludedFieldsImpl[T] }

  private def excludedFieldsImpl[T: Type](using Quotes): Expr[Set[String]] = {
    import quotes.reflect.*
    val tpe       = TypeRepr.of[T]
    val sym       = tpe.typeSymbol
    val fieldAnns = sym.primaryConstructor.paramSymss.flatMap {
      _.filter(_.isValDef).map { s =>
        s.name -> s.annotations.map(_.asExpr)
      }
    }
    val excluded = fieldAnns.collect {
      case (name, anns) if anns.exists {
            case '{ new zio.json.jsonExclude() } => true
            case _                               => false
          } =>
        Expr(name)
    }
    '{ Set(${ Expr.ofSeq(excluded) }*) }
  }

  // Construct a product from array of Any values
  inline def constructProduct[T](using mirror: Mirror.ProductOf[T]): Array[Any] => T =
    (ps: Array[Any]) =>
      mirror.fromProduct(new scala.Product {
        def canEqual(that: Any): Boolean = true
        def productArity: Int            = ps.length
        def productElement(i: Int): Any  = ps(i)
      })

  // Build field accessors for encoding: List[A => Any]
  inline def fieldAccessors[T]: List[T => Any] = ${ fieldAccessorsImpl[T] }

  private def fieldAccessorsImpl[T: Type](using Quotes): Expr[List[T => Any]] = {
    import quotes.reflect.*
    val tpe       = TypeRepr.of[T]
    val sym       = tpe.typeSymbol
    val fieldAnns = sym.primaryConstructor.paramSymss.flatMap {
      _.filter(_.isValDef).map { s =>
        s.name -> s.annotations.map(_.asExpr)
      }
    }.toMap
    val caseFields = sym.caseFields.filterNot { f =>
      fieldAnns
        .get(f.name)
        .exists(_.exists {
          case '{ new zio.json.jsonExclude() } => true
          case _                               => false
        })
    }
    val accessors = caseFields.map { field =>
      '{ (a: T) => ${ Select.unique('{ a }.asTerm, field.name).asExprOf[Any] } }
    }
    Expr.ofList(accessors)
  }

  // Build the match expression for sum type encoding/decoding ordinals
  inline def sumTypeOrdinal[T](value: T): Int = ${ sumTypeOrdinalImpl[T]('value) }

  private def sumTypeOrdinalImpl[T: Type](value: Expr[T])(using Quotes): Expr[Int] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    def leafTypes(of: quotes.reflect.Symbol): List[quotes.reflect.Symbol] =
      of.children.flatMap { child =>
        if (child.flags.is(Flags.Case)) List(child)
        else leafTypes(child)
      }

    // Build if-else chain using isInstanceOf to avoid outer accessor issues with Typed patterns
    val subTypes = leafTypes(sym)
    val valueTerm = value.asTerm
    val fallback: Term = '{ throw new MatchError($value) }.asTerm

    val ifElseChain = subTypes.zipWithIndex.foldRight(fallback) { case ((subSym, idx), elseBody) =>
      val subType = if (subSym.isClassDef && !subSym.flags.is(Flags.Module)) resolveLeafType(tpe, subSym) else subSym.typeRef
      val cond =
        if (subSym.flags.is(Flags.Enum) && subSym.primaryConstructor.paramSymss.isEmpty)
          // Parameterless enum case (singleton): value eq EnumCase
          Apply(Select.unique(valueTerm, "eq"), List(Ident(subSym.termRef)))
        else if (subSym.flags.is(Flags.Module))
          // Case object: value eq Module
          Apply(Select.unique(valueTerm, "eq"), List(Ident(subSym.termRef)))
        else
          // Case class: value.isInstanceOf[SubType]
          TypeApply(Select.unique(valueTerm, "isInstanceOf"), List(Inferred(subType)))
      If(cond, Literal(IntConstant(idx)), elseBody)
    }

    ifElseChain.asExprOf[Int]
  }

  // Resolve the applied type for a leaf subtype of a parameterized parent type.
  // e.g., for Either[String, Unit], leaf Left gives Left[String, Unit]
  private def resolveLeafType(using Quotes)(parentTpe: quotes.reflect.TypeRepr, leaf: quotes.reflect.Symbol): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    val leafRef = leaf.typeRef
    parentTpe match {
      case AppliedType(_, parentArgs) if parentArgs.nonEmpty =>
        // Parent has type args (e.g., Either[String, Unit]).
        // The leaf (e.g., Left) extends the parent with the same type params.
        // Apply the parent's type args to the leaf's type reference.
        leafRef.appliedTo(parentArgs)
      case _ => leafRef
    }
  }

  // Summon encoders for leaf types (flattened through intermediate sealed traits)
  inline def summonLeafEncoders[T](using config: JsonCodecConfiguration): Array[JsonEncoder[?]] =
    ${ summonLeafEncodersImpl[T] }

  private def summonLeafEncodersImpl[T: Type](using Quotes): Expr[Array[JsonEncoder[?]]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    def leafTypes(of: quotes.reflect.Symbol): List[quotes.reflect.Symbol] =
      of.children.flatMap { child =>
        if (child.flags.is(Flags.Case)) List(child)
        else leafTypes(child)
      }

    val leaves = leafTypes(sym)
    val encoderExprs: List[Expr[JsonEncoder[?]]] = leaves.map { leaf =>
      val leafTpe =
        if (leaf.flags.is(Flags.Module)) Ref(leaf.companionModule).tpe
        else if (leaf.isClassDef) resolveLeafType(tpe, leaf)
        else Ref(leaf).tpe
      leafTpe.asType match {
        case '[t] =>
          Expr.summon[JsonEncoder[t]].getOrElse {
            Expr.summon[scala.deriving.Mirror.Of[t]] match {
              case Some(mirror) =>
                '{ DeriveJsonEncoder.gen[t](using summonInline[JsonCodecConfiguration])(using $mirror) }
              case _ =>
                report.errorAndAbort(s"Cannot find or derive JsonEncoder for ${Type.show[t]}")
            }
          }
      }
    }
    '{ Array[JsonEncoder[?]](${ Expr.ofSeq(encoderExprs) }*) }
  }

  // Summon decoders for leaf types (flattened through intermediate sealed traits)
  inline def summonLeafDecoders[T](using config: JsonCodecConfiguration): Array[JsonDecoder[?]] =
    ${ summonLeafDecodersImpl[T] }

  private def summonLeafDecodersImpl[T: Type](using Quotes): Expr[Array[JsonDecoder[?]]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol

    def leafTypes(of: quotes.reflect.Symbol): List[quotes.reflect.Symbol] =
      of.children.flatMap { child =>
        if (child.flags.is(Flags.Case)) List(child)
        else leafTypes(child)
      }

    val leaves = leafTypes(sym)
    val decoderExprs: List[Expr[JsonDecoder[?]]] = leaves.map { leaf =>
      val leafTpe =
        if (leaf.flags.is(Flags.Module)) Ref(leaf.companionModule).tpe
        else if (leaf.isClassDef) resolveLeafType(tpe, leaf)
        else Ref(leaf).tpe
      leafTpe.asType match {
        case '[t] =>
          Expr.summon[JsonDecoder[t]].getOrElse {
            Expr.summon[scala.deriving.Mirror.Of[t]] match {
              case Some(mirror) =>
                '{ DeriveJsonDecoder.gen[t](using summonInline[JsonCodecConfiguration])(using $mirror) }
              case _ =>
                report.errorAndAbort(s"Cannot find or derive JsonDecoder for ${Type.show[t]}")
            }
          }
      }
    }
    '{ Array[JsonDecoder[?]](${ Expr.ofSeq(decoderExprs) }*) }
  }

  // =====================================================================================
  // Macro-based tuple list generation (replaces inline tuple recursion)
  // =====================================================================================

  inline def labelsToList[T <: Tuple]: List[String] = ${ labelsToListImpl[T] }

  private def labelsToListImpl[T: Type](using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    def extractLabels(tpe: TypeRepr): List[String] = tpe.dealias match {
      case AppliedType(_, List(ConstantType(StringConstant(label)), tail)) =>
        label :: extractLabels(tail)
      case _ => Nil
    }
    val labels = extractLabels(TypeRepr.of[T])
    Expr(labels)
  }

  inline def summonDecoders[T <: Tuple](using config: JsonCodecConfiguration): List[JsonDecoder[?]] =
    ${ summonDecodersImpl[T] }

  private def summonDecodersImpl[T: Type](using Quotes): Expr[List[JsonDecoder[?]]] = {
    import quotes.reflect.*
    def extractTypes(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
      case AppliedType(_, List(head, tail)) => head :: extractTypes(tail)
      case _                               => Nil
    }
    val elemTypes = extractTypes(TypeRepr.of[T])
    val decoderExprs = elemTypes.map { elemTpe =>
      elemTpe.asType match {
        case '[t] =>
          Expr.summon[JsonDecoder[t]] match {
            case Some(d) => d.asExprOf[JsonDecoder[?]]
            case _ =>
              Expr.summon[scala.deriving.Mirror.Of[t]] match {
                case Some(mirror) =>
                  '{ DeriveJsonDecoder.gen[t](using summonInline[JsonCodecConfiguration])(using $mirror) }
                    .asExprOf[JsonDecoder[?]]
                case _ =>
                  report.errorAndAbort(s"Cannot find or derive JsonDecoder for ${Type.show[t]}")
              }
          }
      }
    }
    Expr.ofList(decoderExprs)
  }

  inline def summonEncoders[T <: Tuple](using config: JsonCodecConfiguration): List[JsonEncoder[?]] =
    ${ summonEncodersImpl[T] }

  private def summonEncodersImpl[T: Type](using Quotes): Expr[List[JsonEncoder[?]]] = {
    import quotes.reflect.*
    def extractTypes(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
      case AppliedType(_, List(head, tail)) => head :: extractTypes(tail)
      case _                               => Nil
    }
    val elemTypes = extractTypes(TypeRepr.of[T])
    val encoderExprs = elemTypes.map { elemTpe =>
      elemTpe.asType match {
        case '[t] =>
          Expr.summon[JsonEncoder[t]] match {
            case Some(e) => e.asExprOf[JsonEncoder[?]]
            case _ =>
              Expr.summon[scala.deriving.Mirror.Of[t]] match {
                case Some(mirror) =>
                  '{ DeriveJsonEncoder.gen[t](using summonInline[JsonCodecConfiguration])(using $mirror) }
                    .asExprOf[JsonEncoder[?]]
                case _ =>
                  report.errorAndAbort(s"Cannot find or derive JsonEncoder for ${Type.show[t]}")
              }
          }
      }
    }
    Expr.ofList(encoderExprs)
  }
}

// =======================================================================================
// Runtime helpers for building decoders/encoders from compile-time extracted metadata
// =======================================================================================
private[json] object DeriveHelpers {

  def buildProductDecoder[A](
    fieldNamesWithAliases: List[((Either[String, String], List[String]), Int)],
    decoders: => Array[JsonDecoder[?]],
    defaults: Array[Option[() => Any]],
    noExtra: Boolean,
    typeAnnotations: List[Any],
    config: JsonCodecConfiguration,
    typeName: String,
    construct: Array[Any] => A
  ): JsonDecoder[A] = {
    val nameTransformer: String => String =
      typeAnnotations.collectFirst { case jsonMemberNames(format) => format }.getOrElse(config.fieldNameMapping)

    var splitIndex                                              = -1
    val (names, aliases): (Array[String], Array[(String, Int)]) = {
      val namesArr       = Array.ofDim[String](fieldNamesWithAliases.size)
      val aliasesBuilder = Array.newBuilder[(String, Int)]
      fieldNamesWithAliases.foreach { case ((name, aliasList), i) =>
        namesArr(i) = name.fold(identity, nameTransformer)
        aliasesBuilder ++= aliasList.map((_, i))
        if (splitIndex < 0 && i + 1 + aliasesBuilder.length > 64) splitIndex = i
      }
      val aliasArr = aliasesBuilder.result()

      val allFieldNames = namesArr ++ aliasArr.map(_._1)
      if (allFieldNames.length != allFieldNames.distinct.length) {
        val aliasNames = aliasArr.map(_._1)
        val collisions = aliasNames
          .filter(alias => namesArr.contains(alias) || aliasArr.count { case (a, _) => a == alias } > 1)
          .distinct
        val msg = s"Field names and aliases in case class $typeName must be distinct, " +
          s"alias(es) ${collisions.mkString(",")} collide with a field or another alias"
        throw new AssertionError(msg)
      }
      (namesArr, aliasArr)
    }

    val len                                                   = names.length
    val spans: Array[JsonError]                               = names.map(JsonError.ObjectAccess.apply)
    lazy val (matrix1, matrix2): (StringMatrix, StringMatrix) =
      if (splitIndex < 0) (new StringMatrix(names, aliases), null)
      else {
        val (names1, names2) = names.splitAt(splitIndex)
        val aliases1         = aliases.filter(kv => kv._2 < splitIndex)
        val aliases2         = aliases.collect { case (k, v) if v >= splitIndex => (k, v - splitIndex) }
        (new StringMatrix(names1, aliases1), new StringMatrix(names2, aliases2))
      }
    lazy val namesMap: Map[String, Int] = (names.zipWithIndex ++ aliases).toMap

    new CollectionJsonDecoder[A] {
      lazy val tcs: Array[JsonDecoder[?]] = decoders

      private val explicitEmptyCollections: Boolean =
        typeAnnotations.collectFirst { case a: jsonExplicitEmptyCollections => a.decoding }
          .getOrElse(config.explicitEmptyCollections.decoding)

      @tailrec
      private def allowMissingValueDecoder(d: JsonDecoder[_]): Boolean = d match {
        case _: OptionJsonDecoder[_]     => true
        case _: CollectionJsonDecoder[_] => !explicitEmptyCollections
        case d: MappedJsonDecoder[_]     => allowMissingValueDecoder(d.underlying)
        case _                           => true
      }

      private val missingValueDecoder: (Int, List[JsonError]) => Any =
        if (explicitEmptyCollections) {
          lazy val missingValueDecoders = tcs.map { d =>
            if (allowMissingValueDecoder(d)) d else null
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

      def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
        Lexer.char(trace, in, '{')
        val ps: Array[Any] = Array.ofDim(len)
        if (Lexer.firstField(in))
          while ({
            var trace_ = trace
            val field  =
              if (matrix2 eq null) Lexer.field(trace, in, matrix1) else Lexer.field128(trace, in, matrix1, matrix2)
            if (field != -1) {
              trace_ = spans(field) :: trace
              if (ps(field) != null)
                throw UnsafeJson(JsonError.Message("duplicate") :: trace)
              val default = defaults(field)
              ps(field) =
                if (!default.isDefined || in.nextNonWhitespace() != 'n' && { in.retract(); true })
                  tcs(field).unsafeDecode(trace_, in)
                else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l')
                  default.get.apply()
                else
                  Lexer.error("expected 'null'", trace_)
            } else if (noExtra) {
              throw UnsafeJson(
                JsonError.Message("invalid extra field") :: trace
              )
            } else
              Lexer.skipValue(trace_, in)
            Lexer.nextField(trace, in)
          }) ()
        var i = 0
        while (i < len) {
          if (ps(i) == null) {
            if (defaults(i).isDefined) {
              ps(i) = defaults(i).get.apply()
            } else {
              ps(i) = missingValueDecoder(i, trace)
            }
          }
          i += 1
        }
        construct(ps)
      }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
        val no_extra: Boolean = noExtra
        json match {
          case Json.Obj(fields) =>
            val ps: Array[Any] = Array.ofDim(len)

            if (aliases.nonEmpty) {
              val present = fields.map { case (key, _) => namesMap.get(key) }.collect { case Some(idx) => idx }
              if (present.distinct.size != present.size) {
                throw UnsafeJson(JsonError.Message("duplicate") :: trace)
              }
            }

            for ((key, value) <- fields) {
              namesMap.get(key) match {
                case Some(field) =>
                  if (ps(field) != null) throw UnsafeJson(JsonError.Message("duplicate") :: trace)
                  val trace_ = JsonError.ObjectAccess(key) :: trace
                  if (defaults(field).isDefined && value == Json.Null) {
                    ps(field) = defaults(field).get.apply()
                  } else {
                    ps(field) = tcs(field).unsafeFromJsonAST(trace_, value)
                  }
                case _ =>
                  if (no_extra)
                    throw UnsafeJson(JsonError.Message("invalid extra field") :: trace)
              }
            }

            var i = 0
            while (i < len) {
              if (ps(i) == null) {
                if (defaults(i).isDefined) {
                  ps(i) = defaults(i).get.apply()
                } else {
                  ps(i) = missingValueDecoder(i, trace)
                }
              }
              i += 1
            }

            construct(ps)

          case _ => throw UnsafeJson(JsonError.Message("expected object") :: trace)
        }
      }

      override def unsafeDecodeMissing(trace: List[JsonError]): A = {
        val ps: Array[Any] = Array.ofDim(len)
        var i              = 0
        while (i < len) {
          if (defaults(i).isDefined) {
            ps(i) = defaults(i).get.apply()
          } else {
            ps(i) = missingValueDecoder(i, trace)
          }
          i += 1
        }
        construct(ps)
      }
    }
  }

  def buildEmptyProductDecoder[A](
    noExtra: Boolean,
    construct: Array[Any] => A
  ): JsonDecoder[A] =
    new CollectionJsonDecoder[A] {
      def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
        if (noExtra) {
          Lexer.char(trace, in, '{')
          Lexer.char(trace, in, '}')
        } else {
          Lexer.skipValue(trace, in)
        }
        construct(Array.empty)
      }

      override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
        json match {
          case Json.Obj(_) => construct(Array.empty)
          case Json.Null   => construct(Array.empty)
          case _           => throw UnsafeJson(JsonError.Message("expected object") :: trace)
        }

      override def unsafeDecodeMissing(trace: List[JsonError]): A =
        construct(Array.empty)
    }

  def buildSumDecoder[A](
    names: Array[String],
    hasJsonHint: Array[Boolean],
    decoders: => Array[JsonDecoder[?]],
    discrim: Option[String],
    hintFormat: JsonMemberFormat,
    isEnumeration: Boolean,
    config: JsonCodecConfiguration,
    typeName: String
  ): JsonDecoder[A] = {
    val transformedNames = names.zipWithIndex.map { case (name, i) =>
      if (hasJsonHint(i)) name else hintFormat(name)
    }
    if (transformedNames.distinct.length != transformedNames.length) {
      val collisions = transformedNames
        .groupBy(identity)
        .collect { case (n, ns) if ns.length > 1 => n }
        .mkString(",")
      throw new AssertionError(s"Case names in ADT $typeName must be distinct, name(s) $collisions are duplicated")
    }
    val namesMap: Map[String, Int] = transformedNames.zipWithIndex.toMap
    val (names1, names2)           = transformedNames.splitAt(64)
    lazy val matrix1: StringMatrix = new StringMatrix(names1)
    lazy val matrix2: StringMatrix = if (names2.isEmpty) null else new StringMatrix(names2)

    if (discrim.isEmpty && config.enumValuesAsStrings && isEnumeration) {
      new JsonDecoder[A] {
        lazy val tcs: Array[JsonDecoder[?]] = decoders

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val field =
            if (matrix2 eq null) Lexer.enumeration(trace, in, matrix1)
            else Lexer.enumeration128(trace, in, matrix1, matrix2)
          if (field >= 0) {
            // Lexer.enumeration already consumed the string token, so construct the case without reading more input
            // For enum values (case objects), we decode from AST to avoid re-reading consumed input
            tcs(field).unsafeFromJsonAST(trace, Json.Obj(Chunk.empty)).asInstanceOf[A]
          } else {
            throw UnsafeJson(JsonError.Message("invalid enumeration value") :: trace)
          }
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Str(value) =>
              namesMap.get(value) match {
                case Some(idx) => tcs(idx).unsafeFromJsonAST(trace, Json.Obj(Chunk.empty)).asInstanceOf[A]
                case _      => throw UnsafeJson(JsonError.Message("invalid enumeration value") :: trace)
              }
            case _ => throw UnsafeJson(JsonError.Message("expected string") :: trace)
          }
      }
    } else if (discrim.isEmpty) {
      val spans: Array[JsonError] = transformedNames.map(JsonError.ObjectAccess.apply)

      new JsonDecoder[A] {
        lazy val tcs: Array[JsonDecoder[?]] = decoders

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(in)) {
            val field =
              if (matrix2 eq null) Lexer.field(trace, in, matrix1) else Lexer.field128(trace, in, matrix1, matrix2)
            if (field != -1) {
              val trace_ = spans(field) :: trace
              val a      = tcs(field).unsafeDecode(trace_, in)
              Lexer.char(trace, in, '}')
              a.asInstanceOf[A]
            } else
              throw UnsafeJson(JsonError.Message("invalid disambiguator") :: trace)
          } else
            throw UnsafeJson(JsonError.Message("expected non-empty object") :: trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(chunk) if chunk.size == 1 =>
              val (key, inner) = chunk.head
              namesMap.get(key) match {
                case Some(idx) =>
                  tcs(idx).unsafeFromJsonAST(JsonError.ObjectAccess(key) :: trace, inner).asInstanceOf[A]
                case _ => throw UnsafeJson(JsonError.Message("invalid disambiguator") :: trace)
              }
            case Json.Obj(_) => throw UnsafeJson(JsonError.Message("expected object with a single field") :: trace)
            case _           => throw UnsafeJson(JsonError.Message("expected object") :: trace)
          }
      }
    } else {
      val hintfield               = discrim.get
      val hintmatrix              = new StringMatrix(Array(hintfield))
      val spans: Array[JsonError] = transformedNames.map(JsonError.Message(_))

      new JsonDecoder[A] {
        lazy val tcs: Array[JsonDecoder[?]] = decoders

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val in_ : RecordingReader = RecordingReader(in)

          Lexer.char(trace, in_, '{')

          if (Lexer.firstField(in_)) {
            while ({
              if (Lexer.field(trace, in_, hintmatrix) != -1) {
                val field =
                  if (matrix2 eq null) Lexer.enumeration(trace, in_, matrix1)
                  else Lexer.enumeration128(trace, in_, matrix1, matrix2)

                if (field == -1) {
                  throw UnsafeJson(JsonError.Message("invalid disambiguator") :: trace)
                }

                in_.rewind()
                val trace_ = spans(field) :: trace

                return tcs(field).unsafeDecode(trace_, in_).asInstanceOf[A]
              } else {
                Lexer.skipValue(trace, in_)
              }

              Lexer.nextField(trace, in_)
            }) ()
          }

          throw UnsafeJson(JsonError.Message(s"missing hint '$hintfield'") :: trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(fields) =>
              fields.find { case (k, _) => k == hintfield } match {
                case Some((_, Json.Str(name))) =>
                  namesMap.get(name) match {
                    case Some(idx) =>
                      tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, json).asInstanceOf[A]
                    case _ => throw UnsafeJson(JsonError.Message("invalid disambiguator") :: trace)
                  }
                case Some(_) =>
                  throw UnsafeJson(JsonError.Message(s"Non-string hint '$hintfield'") :: trace)
                case _ =>
                  throw UnsafeJson(JsonError.Message(s"missing hint '$hintfield'") :: trace)
              }
            case _ => throw UnsafeJson(JsonError.Message("expected object") :: trace)
          }
      }
    }
  }

  def buildProductEncoder[A](
    fieldNamesEither: List[Either[String, String]],
    encoders: => List[JsonEncoder[?]],
    fieldAccessors: List[A => Any],
    fieldLabels: List[String],
    fieldAnnotations: Map[String, List[Any]],
    typeAnnotations: List[Any],
    config: JsonCodecConfiguration
  ): JsonEncoder[A] =
    new JsonEncoder[A] {
      private lazy val fieldValues: Array[A => Any] = fieldAccessors.toArray

      private lazy val fields: Array[FieldEncoder[Any, Int]] = {
        val nameTransformer: String => String =
          typeAnnotations.collectFirst { case jsonMemberNames(format) => format }.getOrElse(config.fieldNameMapping)
        val explicitNulls            = config.explicitNulls || typeAnnotations.exists(_.isInstanceOf[jsonExplicitNull])
        val explicitEmptyCollections = typeAnnotations.collectFirst { case a: jsonExplicitEmptyCollections =>
          a.encoding
        }
          .getOrElse(config.explicitEmptyCollections.encoding)

        val encs = encoders
        fieldNamesEither
          .zip(encs)
          .zipWithIndex
          .map { case ((nameEither, encoder), idx) =>
            val resolvedName = nameEither.fold(identity, nameTransformer)
            val label        = fieldLabels(idx)
            val anns         = fieldAnnotations.getOrElse(label, Nil)
            FieldEncoder[Any, Int](
              p = idx,
              name = resolvedName,
              encoder = encoder.asInstanceOf[JsonEncoder[Any]],
              withExplicitNulls = explicitNulls || anns.exists(_.isInstanceOf[jsonExplicitNull]),
              withExplicitEmptyCollections = anns.collectFirst { case a: jsonExplicitEmptyCollections => a.encoding }
                .getOrElse(explicitEmptyCollections)
            )
          }
          .toArray
      }

      def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
        out.write('{')
        val indent_ = JsonEncoder.bump(indent)
        val fs      = this.fields
        var idx     = 0
        var comma   = false
        while (idx < fs.length) {
          val field = fs(idx)
          val p     = fieldValues(field.p)(a)
          idx += 1
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
        val fs     = this.fields
        var buf    = new Array[(String, Json)](fs.length)
        var i, idx = 0
        while (idx < fs.length) {
          val field = fs(idx)
          val p     = fieldValues(field.p)(a)
          idx += 1
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

  def buildEmptyProductEncoder[A]: JsonEncoder[A] =
    new JsonEncoder[A] {
      def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
        out.write("{}")

      override final def toJsonAST(a: A): Either[String, Json] =
        Right(Json.Obj(Chunk.empty))
    }

  def buildSumEncoder[A](
    names: Array[String],
    hasJsonHint: Array[Boolean],
    encoders: => Array[JsonEncoder[?]],
    discrim: Option[String],
    hintFormat: JsonMemberFormat,
    isEnumeration: Boolean,
    config: JsonCodecConfiguration,
    ordinalOf: A => Int
  ): JsonEncoder[A] = {
    val transformedNames = names.zipWithIndex.map { case (name, i) =>
      if (hasJsonHint(i)) name else hintFormat(name)
    }

    if (discrim.isEmpty && config.enumValuesAsStrings && isEnumeration) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          val idx = ordinalOf(a)
          JsonEncoder.string.unsafeEncode(transformedNames(idx), indent, out)
        }

        final override def toJsonAST(a: A): Either[String, Json] = {
          val idx = ordinalOf(a)
          Right(Json.Str(transformedNames(idx)))
        }
      }
    } else if (discrim.isEmpty) {
      new JsonEncoder[A] {
        lazy val tcs: Array[JsonEncoder[?]] = encoders

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          val idx     = ordinalOf(a)
          val encoder = tcs(idx)
          val name    = transformedNames(idx)

          out.write("{")
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          JsonEncoder.string.unsafeEncode(name, indent_, out)

          if (indent.isEmpty) out.write(":")
          else out.write(" : ")

          encoder.unsafeEncode(a.asInstanceOf, indent_, out)
          JsonEncoder.pad(indent, out)

          out.write("}")
        }

        final override def toJsonAST(a: A): Either[String, Json] = {
          val idx     = ordinalOf(a)
          val encoder = tcs(idx)
          val name    = transformedNames(idx)
          encoder.toJsonAST(a.asInstanceOf).map(inner => Json.Obj(Chunk(name -> inner)))
        }
      }
    } else {
      val hintFieldName = discrim.get

      new JsonEncoder[A] {
        lazy val tcs: Array[JsonEncoder[?]] = encoders

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          val idx     = ordinalOf(a)
          val encoder = tcs(idx)
          val name    = transformedNames(idx)
          out.write("{")
          val indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          JsonEncoder.string.unsafeEncode(hintFieldName, indent_, out)
          if (indent.isEmpty) out.write(":")
          else out.write(" : ")
          JsonEncoder.string.unsafeEncode(name, indent_, out)

          // pretty printing: a case without fields gets its closing `}` right after the hint, on the same line
          val intermediate = new DeriveJsonEncoder.NestedWriter(out, indent_)
          encoder.unsafeEncode(a.asInstanceOf, indent, intermediate)
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          val idx     = ordinalOf(a)
          val encoder = tcs(idx)
          val name    = transformedNames(idx)
          encoder.toJsonAST(a.asInstanceOf).flatMap {
            case Json.Obj(fields) => Right(Json.Obj(Chunk((hintFieldName, Json.Str(name))) ++ fields))
            case _                => Left("Subtype is not encoded as an object")
          }
        }
      }
    }
  }
}

// =======================================================================================
// Inline derivation helpers for recursively summoning decoders/encoders from tuples
// (kept for backward compatibility — new code uses MacroHelpers.labelsToList etc.)
// =======================================================================================
private[json] object InlineHelpers {

  inline def summonDecoders[T <: Tuple](using config: JsonCodecConfiguration): List[JsonDecoder[?]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonDecoder[t] :: summonDecoders[ts]
    }

  private inline def summonDecoder[T](using config: JsonCodecConfiguration): JsonDecoder[T] =
    summonFrom {
      case d: JsonDecoder[T] => d
      case m: Mirror.Of[T]   => DeriveJsonDecoder.gen[T](using config)(using m)
    }

  inline def summonEncoders[T <: Tuple](using config: JsonCodecConfiguration): List[JsonEncoder[?]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonEncoder[t] :: summonEncoders[ts]
    }

  private inline def summonEncoder[T](using config: JsonCodecConfiguration): JsonEncoder[T] =
    summonFrom {
      case e: JsonEncoder[T] => e
      case m: Mirror.Of[T]   => DeriveJsonEncoder.gen[T](using config)(using m)
    }

  inline def labelsToList[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: labelsToList[ts]
    }
}

// =======================================================================================
// DeriveJsonDecoder — uses inline match on Mirror, not ${ ... } macro splice
// =======================================================================================
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

  /**
   * Derives a `JsonDecoder[A]` for the case class or sealed trait `A`, with the [[JsonCodecConfiguration]] in implicit
   * scope or else `JsonCodecConfiguration.default`. A missing field gets its default value, `None` for an `Option`, an
   * empty collection when `explicitEmptyCollections.decoding` is `false`, and otherwise fails with `.name(missing)`,
   * where `name` is the JSON name of the field. A `null` for a field with a default value also gives the default.
   * Unknown keys are skipped unless [[jsonNoExtraFields]] or `allowExtraFields = false` applies. A case class without
   * fields is read from any JSON value in text (only from an empty object with [[jsonNoExtraFields]]), and from an
   * object or `null` in a JSON AST.
   *
   * @throws java.lang.AssertionError
   *   when the decoder is created, if two fields or aliases of a case class have the same JSON name, or two cases of a
   *   sealed trait have the same type hint
   */
  inline def gen[A](using
    config: JsonCodecConfiguration = JsonCodecConfiguration.default
  )(using mirror: Mirror.Of[A]): JsonDecoder[A] =
    inline mirror match {
      case s: Mirror.SumOf[A]     => deriveSumDecoder[A](using config, s)
      case p: Mirror.ProductOf[A] => deriveProductDecoder[A](using config, p)
    }

  private inline def deriveProductDecoder[A](using
    config: JsonCodecConfiguration,
    mirror: Mirror.ProductOf[A]
  ): JsonDecoder[A] = {
    val meta: ProductMeta                   = MacroHelpers.extractProductMeta[A]
    val fieldLabels: List[String]            = MacroHelpers.labelsToList[mirror.MirroredElemLabels]
    val construct: Array[Any] => A           = MacroHelpers.constructProduct[A]

    val noExtra: Boolean = meta.typeAnnotations.exists(_.isInstanceOf[jsonNoExtraFields]) || !config.allowExtraFields

    val fieldNamesWithAliases: List[((Either[String, String], List[String]), Int)] =
      fieldLabels.zipWithIndex.map { (label, i) =>
        val anns                         = meta.fieldAnnotations.getOrElse(label, Nil)
        val name: Either[String, String] = anns.collectFirst { case jsonField(n) =>
          Left(n)
        }.getOrElse(Right(label))
        val aliases: List[String] = anns.collectFirst { case jsonAliases(alias, more*) =>
          (alias +: more).toList
        }.getOrElse(Nil)
        ((name, aliases), i)
      }

    val numFields = fieldLabels.length
    if (numFields == 0) {
      DeriveHelpers.buildEmptyProductDecoder[A](noExtra, construct)
    } else {
      lazy val decoders: Array[JsonDecoder[?]] = MacroHelpers.summonDecoders[mirror.MirroredElemTypes].toArray
      val defaults: Array[Option[() => Any]]   = meta.defaults.toArray

      DeriveHelpers.buildProductDecoder[A](
        fieldNamesWithAliases = fieldNamesWithAliases,
        decoders = decoders,
        defaults = defaults,
        noExtra = noExtra,
        typeAnnotations = meta.typeAnnotations,
        config = config,
        typeName = meta.typeName,
        construct = construct
      )
    }
  }

  private inline def deriveSumDecoder[A](using
    config: JsonCodecConfiguration,
    mirror: Mirror.SumOf[A]
  ): JsonDecoder[A] = {
    val meta: SumMeta = MacroHelpers.extractSumMeta[A]

    val discrim: Option[String] = meta.typeAnnotations.collectFirst { case jsonDiscriminator(name) =>
      name
    }.orElse(config.sumTypeHandling.discriminatorField)

    val hintFormat: JsonMemberFormat = meta.typeAnnotations.collectFirst { case jsonHintNames(format) =>
      format
    }.getOrElse(config.sumTypeMapping)

    val subtypeInfoArr: Array[(String, Boolean, Boolean, Boolean)] = meta.subtypeInfo
    val names: Array[String]                                       = subtypeInfoArr.map(_._1)
    val hasJsonHintArr: Array[Boolean]                             = subtypeInfoArr.map(_._4)
    val isEnumeration: Boolean                                     = subtypeInfoArr.forall(info => info._2 && !info._3)

    lazy val decoders: Array[JsonDecoder[?]] = MacroHelpers.summonLeafDecoders[A]

    DeriveHelpers.buildSumDecoder[A](
      names = names,
      hasJsonHint = hasJsonHintArr,
      decoders = decoders,
      discrim = discrim,
      hintFormat = hintFormat,
      isEnumeration = isEnumeration,
      config = config,
      typeName = meta.typeName
    )
  }
}

// =======================================================================================
// DeriveJsonEncoder — uses inline match on Mirror, not ${ ... } macro splice
// =======================================================================================
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
object DeriveJsonEncoder { self =>

  /**
   * Derives a `JsonEncoder[A]` for the case class or sealed trait `A`, with the [[JsonCodecConfiguration]] in implicit
   * scope or else `JsonCodecConfiguration.default`. By default, a field that is `None` is left out and an empty
   * collection is written; [[jsonExplicitNull]] and [[jsonExplicitEmptyCollections]] change this. Fields with
   * [[jsonExclude]] are left out.
   */
  inline def gen[A](using
    config: JsonCodecConfiguration = JsonCodecConfiguration.default
  )(using mirror: Mirror.Of[A]): JsonEncoder[A] =
    inline mirror match {
      case s: Mirror.SumOf[A]     => deriveSumEncoder[A](using config, s)
      case p: Mirror.ProductOf[A] => deriveProductEncoder[A](using config, p)
    }

  private inline def deriveProductEncoder[A](using
    config: JsonCodecConfiguration,
    mirror: Mirror.ProductOf[A]
  ): JsonEncoder[A] = {
    val meta: ProductMeta            = MacroHelpers.extractProductMeta[A]
    val fieldLabels: List[String]    = MacroHelpers.labelsToList[mirror.MirroredElemLabels]

    val isEmptyProduct: Boolean = fieldLabels.isEmpty || !meta.hasCaseFields ||
      meta.isModule || meta.isStaticEnum

    if (isEmptyProduct) {
      DeriveHelpers.buildEmptyProductEncoder[A]
    } else {
      val filteredLabels: List[String] = fieldLabels.filterNot(meta.excludedFields.contains)

      val fieldNamesEither: List[Either[String, String]] = filteredLabels.map { label =>
        val anns = meta.fieldAnnotations.getOrElse(label, Nil)
        anns.collectFirst { case jsonField(n) =>
          Left(n): Either[String, String]
        }.getOrElse(Right(label): Either[String, String])
      }

      lazy val encoders: List[JsonEncoder[?]] = {
        val allEncoders = MacroHelpers.summonEncoders[mirror.MirroredElemTypes]
        // Filter out excluded fields by index
        val labels = fieldLabels
        labels.zip(allEncoders).filterNot { case (label, _) => meta.excludedFields.contains(label) }.map(_._2)
      }

      val accessors: List[A => Any] = MacroHelpers.fieldAccessors[A]

      DeriveHelpers.buildProductEncoder[A](
        fieldNamesEither = fieldNamesEither,
        encoders = encoders,
        fieldAccessors = accessors,
        fieldLabels = filteredLabels,
        fieldAnnotations = meta.fieldAnnotations,
        typeAnnotations = meta.typeAnnotations,
        config = config
      )
    }
  }

  private inline def deriveSumEncoder[A](using
    config: JsonCodecConfiguration,
    mirror: Mirror.SumOf[A]
  ): JsonEncoder[A] = {
    val meta: SumMeta = MacroHelpers.extractSumMeta[A]

    val discrim: Option[String] = meta.typeAnnotations.collectFirst { case jsonDiscriminator(name) =>
      name
    }.orElse(config.sumTypeHandling.discriminatorField)

    val hintFormat: JsonMemberFormat = meta.typeAnnotations.collectFirst { case jsonHintNames(format) =>
      format
    }.getOrElse(config.sumTypeMapping)

    val subtypeInfoArr: Array[(String, Boolean, Boolean, Boolean)] = meta.subtypeInfo
    val names: Array[String]                                       = subtypeInfoArr.map(_._1)
    val hasJsonHintArr: Array[Boolean]                             = subtypeInfoArr.map(_._4)
    val isEnumeration: Boolean                                     = subtypeInfoArr.forall(info => info._2 && !info._3)

    lazy val encoders: Array[JsonEncoder[?]] = MacroHelpers.summonLeafEncoders[A]

    val ordinalOf: A => Int = (a: A) => MacroHelpers.sumTypeOrdinal[A](a)

    DeriveHelpers.buildSumEncoder[A](
      names = names,
      hasJsonHint = hasJsonHintArr,
      encoders = encoders,
      discrim = discrim,
      hintFormat = hintFormat,
      isEnumeration = isEnumeration,
      config = config,
      ordinalOf = ordinalOf
    )
  }

  /**
   * A writer that the derived encoder of a sealed trait with a discriminator uses to put the fields of a case into the
   * object that holds the hint field. It drops what the case writes up to and including its opening `{`, then writes a
   * `,` before the first field unless the case is an empty object. Used by the derivation; call `gen` instead.
   */
  final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
    private[this] var state = 2

    /**
     * Writes `c`. Before the first field of the case, it drops whitespace and everything up to and including the
     * opening `{`, and writes a `,` before the first field.
     */
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

    /**
     * Writes `s`. Before the first field of the case, it drops whitespace and everything up to and including the
     * opening `{`, and writes a `,` before the first field.
     */
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

    /**
     * Writes the characters of `cs` from `from` until `to` before the first field of the case has been written: drops
     * them up to and including the opening `{`, then writes a `,` (unless the case is an empty object) and the rest.
     * Used by `write`.
     */
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
   * @throws java.lang.AssertionError
   *   when the codec is created, if two fields or aliases of a case class have the same JSON name, or two cases of a
   *   sealed trait have the same type hint
   */
  inline def gen[A](using
    config: JsonCodecConfiguration = JsonCodecConfiguration.default
  )(using mirror: Mirror.Of[A]): JsonCodec[A] =
    inline mirror match {
      case s: Mirror.SumOf[A]     => deriveSumCodec[A](using config, s)
      case p: Mirror.ProductOf[A] => deriveProductCodec[A](using config, p)
    }

  private inline def deriveProductCodec[A](using
    config: JsonCodecConfiguration,
    mirror: Mirror.ProductOf[A]
  ): JsonCodec[A] = {
    // Extract metadata ONCE, shared between encoder and decoder
    val meta: ProductMeta            = MacroHelpers.extractProductMeta[A]
    val fieldLabels: List[String]    = MacroHelpers.labelsToList[mirror.MirroredElemLabels]
    val construct: Array[Any] => A   = MacroHelpers.constructProduct[A]
    val accessors: List[A => Any]    = MacroHelpers.fieldAccessors[A]

    // === Decoder ===
    val noExtra: Boolean = meta.typeAnnotations.exists(_.isInstanceOf[jsonNoExtraFields]) || !config.allowExtraFields

    val fieldNamesWithAliases: List[((Either[String, String], List[String]), Int)] =
      fieldLabels.zipWithIndex.map { (label, i) =>
        val anns                         = meta.fieldAnnotations.getOrElse(label, Nil)
        val name: Either[String, String] = anns.collectFirst { case jsonField(n) =>
          Left(n)
        }.getOrElse(Right(label))
        val aliases: List[String] = anns.collectFirst { case jsonAliases(alias, more*) =>
          (alias +: more).toList
        }.getOrElse(Nil)
        ((name, aliases), i)
      }

    val numFields = fieldLabels.length

    val decoder: JsonDecoder[A] =
      if (numFields == 0) {
        DeriveHelpers.buildEmptyProductDecoder[A](noExtra, construct)
      } else {
        lazy val decoders: Array[JsonDecoder[?]] = MacroHelpers.summonDecoders[mirror.MirroredElemTypes].toArray
        val defaults: Array[Option[() => Any]]   = meta.defaults.toArray

        DeriveHelpers.buildProductDecoder[A](
          fieldNamesWithAliases = fieldNamesWithAliases,
          decoders = decoders,
          defaults = defaults,
          noExtra = noExtra,
          typeAnnotations = meta.typeAnnotations,
          config = config,
          typeName = meta.typeName,
          construct = construct
        )
      }

    // === Encoder ===
    val isEmptyProduct: Boolean = fieldLabels.isEmpty || !meta.hasCaseFields ||
      meta.isModule || meta.isStaticEnum

    val encoder: JsonEncoder[A] =
      if (isEmptyProduct) {
        DeriveHelpers.buildEmptyProductEncoder[A]
      } else {
        val filteredLabels: List[String] = fieldLabels.filterNot(meta.excludedFields.contains)

        val fieldNamesEither: List[Either[String, String]] = filteredLabels.map { label =>
          val anns = meta.fieldAnnotations.getOrElse(label, Nil)
          anns.collectFirst { case jsonField(n) =>
            Left(n): Either[String, String]
          }.getOrElse(Right(label): Either[String, String])
        }

        lazy val encoders: List[JsonEncoder[?]] = {
          val allEncoders = MacroHelpers.summonEncoders[mirror.MirroredElemTypes]
          val labels = fieldLabels
          labels.zip(allEncoders).filterNot { case (label, _) => meta.excludedFields.contains(label) }.map(_._2)
        }

        DeriveHelpers.buildProductEncoder[A](
          fieldNamesEither = fieldNamesEither,
          encoders = encoders,
          fieldAccessors = accessors,
          fieldLabels = filteredLabels,
          fieldAnnotations = meta.fieldAnnotations,
          typeAnnotations = meta.typeAnnotations,
          config = config
        )
      }

    JsonCodec(encoder, decoder)
  }

  private inline def deriveSumCodec[A](using
    config: JsonCodecConfiguration,
    mirror: Mirror.SumOf[A]
  ): JsonCodec[A] = {
    // Extract metadata ONCE, shared between encoder and decoder
    val meta: SumMeta = MacroHelpers.extractSumMeta[A]

    val discrim: Option[String] = meta.typeAnnotations.collectFirst { case jsonDiscriminator(name) =>
      name
    }.orElse(config.sumTypeHandling.discriminatorField)

    val hintFormat: JsonMemberFormat = meta.typeAnnotations.collectFirst { case jsonHintNames(format) =>
      format
    }.getOrElse(config.sumTypeMapping)

    val subtypeInfoArr: Array[(String, Boolean, Boolean, Boolean)] = meta.subtypeInfo
    val names: Array[String]                                       = subtypeInfoArr.map(_._1)
    val hasJsonHintArr: Array[Boolean]                             = subtypeInfoArr.map(_._4)
    val isEnumeration: Boolean                                     = subtypeInfoArr.forall(info => info._2 && !info._3)

    lazy val decoders: Array[JsonDecoder[?]] = MacroHelpers.summonLeafDecoders[A]
    lazy val encoders: Array[JsonEncoder[?]] = MacroHelpers.summonLeafEncoders[A]

    val ordinalOf: A => Int = (a: A) => MacroHelpers.sumTypeOrdinal[A](a)

    val decoder = DeriveHelpers.buildSumDecoder[A](
      names = names,
      hasJsonHint = hasJsonHintArr,
      decoders = decoders,
      discrim = discrim,
      hintFormat = hintFormat,
      isEnumeration = isEnumeration,
      config = config,
      typeName = meta.typeName
    )

    val encoder = DeriveHelpers.buildSumEncoder[A](
      names = names,
      hasJsonHint = hasJsonHintArr,
      encoders = encoders,
      discrim = discrim,
      hintFormat = hintFormat,
      isEnumeration = isEnumeration,
      config = config,
      ordinalOf = ordinalOf
    )

    JsonCodec(encoder, decoder)
  }
}
