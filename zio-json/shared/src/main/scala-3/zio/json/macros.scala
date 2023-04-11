package zio.json

import scala.annotation.*
import scala.compiletime.*
import zio.Chunk
import zio.json.JsonDecoder.{JsonError, UnsafeJson}
import zio.json.ast.Json
import zio.json.internal.*

import scala.annotation.*
import scala.collection.mutable
import scala.compiletime.*
import scala.language.experimental.macros
import scala.quoted.*
import scala.reflect.*

/**
 * If used on a case class field, determines the name of the JSON field.
 * Defaults to the case class field name.
 */
final case class jsonField(name: String) extends Annotation

/**
 * If used on a case class field, determines the alternative names of the JSON field.
 */
final case class jsonAliases(alias: String, aliases: String*) extends Annotation

/**
 * If used on a sealed class, will determine the name of the field for
 * disambiguating classes.
 *
 * The default is to not use a typehint field and instead
 * have an object with a single key that is the class name.
 *
 * Note that using a discriminator is less performant, uses more memory, and may
 * be prone to DOS attacks that are impossible with the default encoding. In
 * addition, there is slightly less type safety when using custom product
 * encoders (which must write an unenforced object type). Only use this option
 * if you must model an externally defined schema.
 */
final case class jsonDiscriminator(name: String) extends Annotation
// TODO a strategy where the constructor is inferred from the field names, only
// valid if there is no ambiguity in the types of fields for all case classes.
// Such a strategy cannot be implemented with Magnolia because the SealedTrait
// does not provide a mechanism for obtaining the CaseClass associated to the
// Subtype.

sealed trait JsonMemberFormat extends (String => String)
case class CustomCase(f: String => String) extends JsonMemberFormat {
  override def apply(memberName: String): String = f(memberName)
}
case object SnakeCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '_')
}
case object CamelCase extends JsonMemberFormat {
  override def apply(memberName: String): String =
    jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = false)
}
case object PascalCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = true)
}
case object KebabCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '-')
}

/** zio-json version 0.3.0 formats. abc123Def -> abc_123_def */
object ziojson_03 {
  case object SnakeCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '_')
  }
  case object KebabCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '-')
  }
}

/**
 * If used on a case class, determines the strategy of member names
 * transformation during serialization and deserialization. Four common
 * strategies are provided above and a custom one to support specific use cases.
 */
final case class jsonMemberNames(format: JsonMemberFormat) extends Annotation
private[json] object jsonMemberNames {

  /**
   * ~~Stolen~~ Borrowed from jsoniter-scala by Andriy Plokhotnyuk
   * (he even granted permission for this, imagine that!)
   */

  import java.lang.Character.*

  def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String =
    if (s.indexOf('_') == -1 && s.indexOf('-') == -1) {
      if (s.isEmpty) s
      else {
        val ch = s.charAt(0)
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
    val len                   = s.length
    val sb                    = new StringBuilder(len << 1)
    var i                     = 0
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
    val len = s.length
    val sb = new StringBuilder(len << 1)
    var i = 0
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
 * If used on a case class will determine the type hint value for disambiguating
 * sealed traits. Defaults to the short type name.
 */
final case class jsonHint(name: String) extends Annotation

/**
 * If used on a case class, will exit early if any fields are in the JSON that
 * do not correspond to field names in the case class.
 *
 * This adds extra protections against a DOS attacks but means that changes in
 * the schema will result in a hard error rather than silently ignoring those
 * fields.
 *
 * Cannot be combined with `@jsonDiscriminator` since it is considered an extra
 * field from the perspective of the case class.
 */
final class jsonNoExtraFields extends Annotation

/**
 * If used on a case class field, will exclude it from the resulting JSON.
 */
final class jsonExclude extends Annotation

// TODO: implement same configuration as for Scala 2
object DeriveJsonDecoder { self =>

  def deriveDecoder[A: Type](using Quotes): Expr[JsonDecoder[A]] = {
    import quotes.reflect.*, quotes.*
    val symbol = TypeRepr.of[A].typeSymbol

    def decoder(tpe: Type[?]): Expr[JsonDecoder[?]] =
      tpe match { case '[t] => Expr.summon[JsonDecoder[t]].getOrElse(deriveDecoder[t]) }

    def leafTypes(of: Symbol): List[Symbol] =
      of.children.flatMap { child =>
        if child.flags.is(Flags.Case)
        then List(child)
        else leafTypes(child)
      }


    def deriveSumDecoder = {
      val discrim = symbol.annotations.map(_.asExpr).collectFirst {
        case '{ new zio.json.jsonDiscriminator(${Expr(name)}: String) } => name
      }
      val subTypes = leafTypes(symbol).map(_.typeRef)

      val names: Array[String] =
          subTypes.map { subclassType =>
            subclassType.typeSymbol.annotations.map(_.asExpr).collectFirst {
              case '{ new zio.json.jsonHint(${ Expr(name) }: String) } => name
            }.getOrElse {
                if subclassType.typeSymbol.flags.is(Flags.Module) // does not match on enums with intention
                then subclassType.typeSymbol.companionModule.name
                else subclassType.typeSymbol.name
              }
          }.toArray


      val decodersExpr = Expr.ofList(subTypes.map(_.asType).map(decoder)).asExprOf[List[JsonDecoder[?]]]

      val namesExpr = Expr(names)
      val matrixExpr = '{new StringMatrix(${namesExpr})}
      val namesMapExpr = Expr(names.zipWithIndex.toMap)
      if (discrim.isEmpty) {
        // We're not allowing extra fields in this encoding
        '{ new JsonDecoder[A] {
          val namesMap: Map[String, Int]      = ${ namesMapExpr }
          val names: Array[String]            = ${ namesExpr }
          val spans: Array[JsonError]         = names.map(JsonError.ObjectAccess.apply)
          lazy val decoders: Array[JsonDecoder[?]] = ${ decodersExpr }.toArray
          lazy val matrix: StringMatrix       = ${ matrixExpr }

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')

            if (Lexer.firstField(trace, in)) {
              val field = Lexer.field(trace, in, matrix)

              if (field != -1) {
                val trace_ = spans(field) :: trace
                val a = decoders(field).unsafeDecode(trace_, in)
                Lexer.char(trace, in, '}')
                a.asInstanceOf[A]
              } else
                throw UnsafeJson(
                  JsonError.Message(s"invalid disambiguator") :: trace
                  )
            } else
              throw UnsafeJson(
                JsonError.Message("expected non-empty object") :: trace
                )
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
            json match {
              case Json.Obj(chunk) if chunk.size == 1 =>
                val (key, inner) = chunk.head
                namesMap.get(key) match {
                  case Some(idx) => decoders(idx).unsafeFromJsonAST(JsonError.ObjectAccess(key) :: trace, inner).asInstanceOf[A]
                  case None => throw UnsafeJson(JsonError.Message(s"Invalid disambiguator") :: trace)
                }
              case Json.Obj(_) => throw UnsafeJson(JsonError.Message("Not an object with a single field") :: trace)
              case _ => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
            }
          }
        }
        }
      } else {
        val hintfieldExpr = Expr(discrim.get)
        '{ new JsonDecoder[A] {
          val hintfield                       = ${hintfieldExpr}
          val hintmatrix                      = new StringMatrix(Array(hintfield))
          val namesMap: Map[String, Int]      = ${ namesMapExpr }
          val names: Array[String]            = ${ namesExpr }
          val spans: Array[JsonError]         = names.map(JsonError.Message(_))
          lazy val decoders: Array[JsonDecoder[?]] = ${ decodersExpr }.toArray
          lazy val matrix: StringMatrix       = ${ matrixExpr }

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            val in_ : RecordingReader = RecordingReader(in)

            Lexer.char(trace, in_, '{')

            if (Lexer.firstField(trace, in_)) {
              while ( {
                if (Lexer.field(trace, in_, hintmatrix) != -1) {
                  val field = Lexer.enumeration(trace, in_, matrix)

                  if (field == -1) {
                    throw UnsafeJson(JsonError.Message(s"invalid disambiguator") :: trace)
                  }

                  in_.rewind()
                  val trace_ = spans(field) :: trace

                  return decoders(field).unsafeDecode(trace_, in_).asInstanceOf[A]
                } else {
                  Lexer.skipValue(trace, in_)
                }

                Lexer.nextField(trace, in_)
              }) ()
            }

            throw UnsafeJson(JsonError.Message(s"missing hint '$hintfield'") :: trace)
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
            json match {
              case Json.Obj(fields) =>
                fields.find { case (k, _) => k == hintfield } match {
                  case Some((_, Json.Str(name))) =>
                    namesMap.get(name) match {
                      case Some(idx) => decoders(idx).unsafeFromJsonAST(JsonError.ObjectAccess(name) :: trace, json).asInstanceOf[A]
                      case None => throw UnsafeJson(JsonError.Message(s"Invalid disambiguator") :: trace)
                    }
                  case Some(_) =>
                    throw UnsafeJson(JsonError.Message(s"Non-string hint '$hintfield'") :: trace)
                  case None =>
                    throw UnsafeJson(JsonError.Message(s"Missing hint '$hintfield'") :: trace)
                }
              case _ => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
            }
          }
        }
        }
      }

    }

    def deriveProductDecoder = {
      val caseFields = symbol.caseFields
      val clsSymbol = TypeRepr.of[A].classSymbol.get

      def caseClassApply(input: List[Term]) =
        Apply(Select.unique(Ref(clsSymbol.companionModule), "apply"), input)

      val annotations: Expr[List[Any]] =
        Expr.ofList(symbol.annotations.map(_.asExpr))

      val noExtra: Expr[Boolean] =
        symbol
          .annotations
          .map(_.asExpr)
          .collectFirst { case '{ new zio.json.jsonNoExtraFields() } => '{ true } }
          .getOrElse('{ false })

      val isModule = symbol.flags.is(Flags.Module)
      val isStaticEnum = symbol.flags.is(Flags.Enum) && symbol.flags.is(Flags.JavaStatic)

      if (caseFields.isEmpty) {
        '{ new JsonDecoder[A] {
          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            if (${noExtra}) {
              Lexer.char(trace, in, '{')
              Lexer.char(trace, in, '}')
            } else {
              Lexer.skipValue(trace, in)
            }
            ${
              if isModule
              then Ident(symbol.companionModule.termRef).asExprOf[A]
              else if isStaticEnum
              then Ident(symbol.termRef).asExprOf[A]
              else caseClassApply(Nil).asExprOf[A]
            }
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
            json match {
              case Json.Obj(_) =>
                ${
                  if isModule
                  then Ident(symbol.companionModule.termRef).asExprOf[A]
                  else if isStaticEnum
                  then Ident(symbol.termRef).asExprOf[A]
                  else caseClassApply(Nil).asExprOf[A]
                }
              case Json.Null =>
                ${
                  if isModule
                  then Ident(symbol.companionModule.termRef).asExprOf[A]
                  else if isStaticEnum
                  then Ident(symbol.termRef).asExprOf[A]
                  else caseClassApply(Nil).asExprOf[A]
                }
              case _ => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
            }
        }
        }
      } else {
        val fieldAnnotations =
          symbol.primaryConstructor.paramSymss.flatMap {
            _.map(s => s.name -> s.annotations.map(_.asExpr))
          }.toMap

        val fieldTypes = caseFields.map(_.tree).map { case ValDef(_, tpe, _) => tpe.tpe.asType }
        val fieldDecoders: Expr[List[JsonDecoder[?]]] = Expr.ofList(fieldTypes.map(decoder))
        val fieldNames: Expr[List[((Either[String, String], List[String]), Int)]] = Expr {
          caseFields.zipWithIndex.map { (p, i) =>
                val name =
                  fieldAnnotations(p.name)
                    .collectFirst { case '{ new zio.json.jsonField(${ Expr(n) }) } => Left(n) }
                    .getOrElse(Right(p.name))


                val aliases =
                  fieldAnnotations(p.name)
                    .collectFirst {
                      case '{
                        new zio.json.jsonAliases(${ Expr(n) }, ${ Varargs(Exprs(aliases)) }: _*)
                      } =>
                        (n +: aliases).toList
                    }
                    .getOrElse(Nil)
                ((name, aliases), i)
            }
        }

        val defaultValues =
          Expr.ofList(
            (1 to caseFields.size).toList.map(
              i =>
                Expr.ofSeq(
                  TypeRepr.of[A].typeSymbol
                    .companionClass
                    .declaredMethod(s"$$lessinit$$greater$$default$$$i")
                    .headOption
                    .toSeq
                    .map(Select(Ref(TypeRepr.of[A].typeSymbol.companionModule), _).asExpr))
              ))


        '{ new JsonDecoder[A] {
          val nameTransformer: String => String = ${annotations}.collectFirst {
            case jsonMemberNames(transformNames) => transformNames
          }.getOrElse(identity[String] _)

          val (names, aliases): (Array[String], Array[(String, Int)]) = {
            val names = Array.ofDim[String](${fieldNames}.size)
            val aliasesBuilder = Array.newBuilder[(String, Int)]
            ${fieldNames}.map { case ((name, aliases), i) =>
              names(i) = name.fold(identity, nameTransformer)
              aliasesBuilder ++= aliases.map((_, i))
            }
            val aliases = aliasesBuilder.result()

            val allFieldNames = names ++ aliases.map(_._1)
            if (allFieldNames.length != allFieldNames.distinct.length) {
              val aliasNames = aliases.map(_._1)
              val collisions = aliasNames
                .filter(alias => names.contains(alias) || aliases.count { case (a, _) => a == alias } > 1)
                .distinct
              val msg = s"Field names and aliases in case class ${${Expr(symbol.fullName)}} must be distinct, " +
                s"alias(es) ${collisions.mkString(",")} collide with a field or another alias"
              throw new AssertionError(msg)
            }
            (names, aliases)
          }

          val len: Int                        = names.length
          val spans: Array[JsonError]         = names.map(JsonError.ObjectAccess.apply)
          val defaults: Array[Option[?]]      = ${ defaultValues }.map(_.headOption).toArray
          lazy val decoders: Array[JsonDecoder[?]] = ${ fieldDecoders }.toArray
          lazy val matrix: StringMatrix       = new StringMatrix(names, aliases)
          lazy val namesMap: Map[String, Int] = (names.zipWithIndex ++ aliases).toMap

          def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
            Lexer.char(trace, in, '{')
            val ps: Array[Any] = Array.ofDim(len)
            if (Lexer.firstField(trace, in))
              while ( {
                var trace_ = trace
                val field = Lexer.field(trace, in, matrix)
                if (field != -1) {
                  trace_ = spans(field) :: trace
                  if (ps(field) != null)
                    throw UnsafeJson(JsonError.Message("duplicate") :: trace)
                  if (defaults(field).isDefined) {
                    val opt = JsonDecoder.option(decoders(field)).unsafeDecode(trace_, in)
                    ps(field) = opt.getOrElse(defaults(field).get)
                  } else
                    ps(field) = decoders(field).unsafeDecode(trace_, in)
                } else if (${noExtra}) {
                  throw UnsafeJson(
                    JsonError.Message(s"invalid extra field") :: trace
                    )
                } else
                  Lexer.skipValue(trace_, in)
                Lexer.nextField(trace, in)
              }) ()
            var i = 0
            while (i < len) {
              if (ps(i) == null) {
                if (defaults(i).isDefined) {
                  ps(i) = defaults(i).get
                } else {
                  ps(i) = decoders(i).unsafeDecodeMissing(spans(i) :: trace)
                }
              }
              i += 1
            }
            ${ ValDef.let(
              Symbol.spliceOwner,
              fieldTypes.zipWithIndex.toList.map{case ('[t],i) => '{ps(${Expr(i)}).asInstanceOf[t]}.asTerm},
              )(caseClassApply).asExprOf[A]
            }
          }

          override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          {
            val no_extra: Boolean = ${noExtra}
            json match {
              case Json.Obj(fields) =>
                val ps: Array[Any] = Array.ofDim(len)

                if (aliases.nonEmpty) {
                  val present = fields.map { case (key, _) => namesMap(key) }
                  if (present.distinct.size != present.size) {
                    throw UnsafeJson(JsonError.Message("duplicate") :: trace)
                  }
                }

                for ((key, value) <- fields) {
                  namesMap.get(key) match {
                    case Some(field) =>
                      val trace_ = JsonError.ObjectAccess(key) :: trace
                      if (defaults(field).isDefined) {
                        val opt = JsonDecoder.option(decoders(field)).unsafeFromJsonAST(trace_, value)
                        ps(field) = opt.getOrElse(defaults(field).get)
                      } else {
                        ps(field) = decoders(field).unsafeFromJsonAST(trace_, value)
                      }
                    case None =>
                      if   no_extra
                      then throw UnsafeJson(JsonError.Message("invalid extra field") :: trace)
                  }
                }

                var i = 0
                while (i < len) {
                  if (ps(i) == null) {
                    if (defaults(i).isDefined) {
                      ps(i) = defaults(i).get
                    } else {
                      ps(i) = decoders(i).unsafeDecodeMissing(JsonError.ObjectAccess(names(i)) :: trace)
                    }
                  }
                  i += 1
                }

                ${ ValDef.let(
                  Symbol.spliceOwner,
                  fieldTypes.zipWithIndex.toList.map { case ('[t], i) => '{ ps(${ Expr(i) }).asInstanceOf[t] }.asTerm },
                  )(caseClassApply).asExprOf[A]
                }

              case _ => throw UnsafeJson(JsonError.Message("Not an object") :: trace)
            }
          }
        }
        }
      }


    }

    if symbol.flags.is(Flags.Sealed) || symbol.flags.is(Flags.Enum)
    then deriveSumDecoder
    else deriveProductDecoder


  }

  inline def gen[A] = ${ deriveDecoder[A] }

}

object DeriveJsonEncoder { self =>

  private def deriveEncoder[A: Type](using Quotes): Expr[JsonEncoder[A]] = {
    import quotes.reflect.*, quotes.*
    val symbol = TypeRepr.of[A].typeSymbol
    def encoder(tpe: Type[?]): Expr[JsonEncoder[?]] =
      tpe match { case '[t] => Expr.summon[JsonEncoder[t]].getOrElse(deriveEncoder[t]) }

    def leafTypeSymbols(of: Symbol): List[Symbol] =
      of.children.flatMap { child =>
        if   child.flags.is(Flags.Case)
        then List(child)
        else leafTypeSymbols(child)
      }

    def deriveSumEncoder = {

      val discrim =
        symbol.annotations
        .map(_.asExpr)
        .collectFirst { case '{ new zio.json.jsonDiscriminator(${ Expr(n) }: String)} => n }
      val subTypeSymbols = leafTypeSymbols(symbol)
      val subTypeRefs    = subTypeSymbols.map(_.typeRef)

      // creates a match expression
      def fromSubClassTo[T: Type](value: Expr[A], rhs: TypeRepr => Expr[T]): Expr[T] =
        Match(
          value.asTerm,
          subTypeRefs.zip(subTypeSymbols).map { (subType, subSymbol) =>
            if subSymbol.flags.is(Flags.Enum) && subSymbol.primaryConstructor.paramSymss.isEmpty
            then  CaseDef(Ident(subSymbol.termRef), None, rhs(subType).asTerm)
            else {
              val sym = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, subType)
              val pattern = Bind(sym, Typed(Ref(sym), TypeIdent(subType.typeSymbol)))
              CaseDef(pattern, None, rhs(subType).asTerm)
            }
          }
        ).asExprOf[T]

      def encoderFor(value: Expr[A]): Expr[JsonEncoder[_ <: A]] =
        fromSubClassTo(value, subclassType => encoder(subclassType.asType).asExprOf[JsonEncoder[_ <: A]])

      def nameFor(value: Expr[A]): Expr[String] =
        fromSubClassTo(
          value,
          subclassType => Expr {
            subclassType.typeSymbol.annotations.map(_.asExpr).collectFirst {
              case '{ new zio.json.jsonHint(${ Expr(name) }: String) } => name
            }.getOrElse {
              if subclassType.typeSymbol.flags.is(Flags.Module) // does not match on enums with intention
              then subclassType.typeSymbol.companionModule.name
              else subclassType.typeSymbol.name
            }
          }
          )

      if (discrim.isEmpty) {
        '{ new JsonEncoder[A] {
          def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
            val encoder = ${ encoderFor('{ a }) }
            val name = ${ nameFor('{ a }) }

            out.write("{")
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(name, indent_, out)

            if   indent.isEmpty
            then out.write(":")
            else out.write(" : ")

            encoder.unsafeEncode(a.asInstanceOf, indent_, out)
            JsonEncoder.pad(indent, out)

            out.write("}")
          }

          final override def toJsonAST(a: A): Either[String, Json] = {
            val encoder = ${ encoderFor('{ a }) }
            val name = ${ nameFor('{ a }) }
            encoder.toJsonAST(a.asInstanceOf).map { inner => Json.Obj(Chunk(name -> inner)) }
          }
        }
        }
      } else {
        val hintField: Expr[String] = Expr(discrim.get)

        '{ new JsonEncoder[A] {
          def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
            val encoder = ${ encoderFor('{ a }) }
            val name = ${ nameFor('{ a }) }
            out.write("{")
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(${ hintField }, indent_, out)
            if   indent.isEmpty
            then out.write(":")
            else out.write(" : ")
            JsonEncoder.string.unsafeEncode(name, indent_, out)

            // whitespace is always off by 2 spaces at the end, probably not worth fixing
            val intermediate = new NestedWriter(out, indent_)
            encoder.unsafeEncode(a.asInstanceOf, indent, intermediate)
          }

          override final def toJsonAST(a: A): Either[String, Json] = {
            val encoder = ${ encoderFor('{ a }) }
            val name = ${ nameFor('{ a }) }
            val hintName: String = ${ hintField }
            encoder.toJsonAST(a.asInstanceOf).flatMap {
              case Json.Obj(fields) => Right(Json.Obj(fields :+ hintName -> Json.Str(name)))
              case _                => Left("Subtype is not encoded as an object")
            }
          }
        }
        }
      }
    }

    def deriveProductEncoder = {
      val fieldAnnotations =
        symbol.primaryConstructor.paramSymss.flatMap {
          _.map(s => s.name -> s.annotations.map(_.asExpr))
        }.toMap
      val caseFields = TypeTree.of[A].symbol.caseFields
        .filterNot { f =>
          fieldAnnotations(f.name)
            .collectFirst { case '{ new zio.json.jsonExclude() } => () }.isDefined
        }

      if (symbol.flags.is(Flags.Enum) && symbol.flags.is(Flags.JavaStatic)
        || symbol.flags.is(Flags.Module) || caseFields.isEmpty) {
        '{ new JsonEncoder[A] {
          def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
            out.write("{}")

          override final def toJsonAST(a: A): Either[String, Json] =
            Right(Json.Obj(Chunk.empty))
        } }
      } else {

        val fieldTypes = caseFields.map(_.tree).map { case ValDef(_, tpe, _) => tpe.tpe.asType }

        val clsAnnotations: Expr[List[Any]] =
          Expr.ofList(symbol.annotations.map(_.asExpr))

        val fieldNames: Expr[List[Either[String, String]]] = Expr {
          caseFields.map { p =>
            fieldAnnotations(p.name)
              .collectFirst { case '{ new zio.json.jsonField(${ Expr(name) }: String) } => Left(name) }
              .getOrElse(Right(p.name))
          }
        }


        def selectFieldByName(field: Symbol): Expr[A => Any] =
          '{ (a: A) => ${ Select('{ a }.asTerm, field).asExprOf[Any] } }

        val selectedFields: List[Expr[A => Any]]      = caseFields.map(selectFieldByName)
        val fieldEncoders: List[Expr[JsonEncoder[?]]] = fieldTypes.map(encoder)

        def fieldType(param: Symbol) = param.tree match { case ValDef(_, tpe, _) => tpe.tpe.asType }

        '{ new JsonEncoder[A] {
          val nameTransformer: String => String = ${ clsAnnotations }.collectFirst {
            case jsonMemberNames(transformNames) => transformNames
          }.getOrElse(identity[String] _)

          val names = ${ fieldNames }.map(_.fold(identity, nameTransformer))

          lazy val encoders: List[JsonEncoder[?]] = ${ Expr.ofList(fieldEncoders) }
          val fieldValues: List[A => Any] = ${ Expr.ofList(selectedFields) }

          def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
            out.write("{")

            var indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)

            var i = 0
            var prevFields = false

            while (i < encoders.size) {
              val encoder = encoders(i)
              val p = fieldValues.apply(i).apply(a)

              if (!encoder.isNothing(p.asInstanceOf)) {
                // if we have at least one field already, we need a comma
                if (prevFields) {
                  if (indent.isEmpty) {
                    out.write(",")
                  } else {
                    out.write(",")
                    JsonEncoder.pad(indent_, out)
                  }
                }

                JsonEncoder.string.unsafeEncode(names(i), indent_, out)

                if (indent.isEmpty) {
                  out.write(":")
                } else {
                  out.write(" : ")
                }

                encoder.unsafeEncode(p.asInstanceOf, indent_, out)
                prevFields = true // at least one field so far
              }

              i += 1
            }

            JsonEncoder.pad(indent, out)
            out.write("}")
          }

          override final def toJsonAST(a: A): Either[String, Json] =
              ${
                caseFields
                  .foldLeft[Expr[Either[String, Chunk[(String, Json)]]]]('{ Right(Chunk.empty) }) { case (accExpr, p) =>
                    val name =
                      fieldAnnotations(p.name)
                        .collectFirst {
                          case '{ new zio.json.jsonField(${Expr(name)}: String)} =>
                            Left(name)
                        }
                        .getOrElse(Right(p.name))

                    '{
                      ${ accExpr }.flatMap { chunk =>
                        ${ val tpe = fieldType(p)
                          tpe match {
                            case '[t] =>
                              '{
                                val mappedName = ${ Expr(name) }.fold(identity, nameTransformer)
                                ${ encoder(tpe) }.asInstanceOf[JsonEncoder[t]]
                                .toJsonAST(${ Select('{ a }.asTerm, p).asExprOf[t] })
                                .map { value =>
                                  if value == Json.Null
                                  then chunk
                                  else chunk :+ mappedName -> value
                                }
                              }
                          }
                        }
                      }
                    }
                  }
              }.map(Json.Obj.apply)
        }
        }
      }

    }

    if (symbol.flags.is(Flags.Sealed) || symbol.flags.is(Flags.Enum))
      && (symbol.flags.is(Flags.Trait) || symbol.flags.is(Flags.Abstract))
    then deriveSumEncoder
    else deriveProductEncoder
  }

  inline def gen[A]: JsonEncoder[A] = ${ deriveEncoder[A] }

  // intercepts the first `{` of a nested writer and discards it. We also need to
  // inject a `,` unless an empty object `{}` has been written.
  final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
    private[this] var first, second = true

    def write(c: Char): Unit = write(c.toString) // could be optimised

    def write(s: String): Unit =
      if (first || second) {
        var i = 0
        while (i < s.length) {
          val c = s.charAt(i)
          if (c == ' ' || c == '\n') {} else if (first && c == '{') {
            first = false
          } else if (second) {
            second = false
            if (c != '}') {
              out.write(',')
              JsonEncoder.pad(indent, out)
            }
            return out.write(s.substring(i))
          }
          i += 1
        }
      } else out.write(s)
  }
}

object DeriveJsonCodec {
  inline def gen[A] = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]

    JsonCodec(encoder, decoder)
  }
}
