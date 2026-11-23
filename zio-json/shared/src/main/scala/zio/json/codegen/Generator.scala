package zio.json.codegen

import zio.Chunk
import zio.json._
import zio.json.ast.Json
import zio.json.codegen.Generator.pascalFormat
import zio.json.codegen.JsonType._

import java.time.{ LocalDate, LocalDateTime }
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.collection.immutable.ListMap
import scala.math.BigDecimal.javaBigDecimal2bigDecimal
import scala.util.Try

/**
 * Generates Scala case classes, with `JsonCodec` instances, from a sample of JSON.
 *
 * @see [[https://zio.dev/zio-json/ ZIO JSON]]
 */
object Generator {

  /**
   * Renders the JSON string as a series of Scala case classes derived from the structure of the JSON.
   *
   * The classes are printed to the console in cyan. When the input is not valid JSON, it prints `Invalid JSON: ` followed by
   * the input.
   *
   * For example, the following JSON:
   *
   * ```json
   * {
   *   "foo": "bar",
   *   "baz": {
   *     "qux": "quux"
   *   }
   * }
   * ```
   *
   * Would print the following Scala code to the console:
   *
   * {{{
   * import zio.json._
   *
   * final case class RootObject(
   *   foo: String,
   *   baz: Baz
   * )
   *
   * object RootObject {
   *   implicit val codec: JsonCodec[RootObject] = DeriveJsonCodec.gen
   * }
   *
   * final case class Baz(
   *   qux: String
   * )
   *
   * object Baz {
   *   implicit val codec: JsonCodec[Baz] = DeriveJsonCodec.gen
   * }
   * }}}
   *
   * The top-level object is called `RootObject` and every nested object is a case class named after its field, written in
   * PascalCase. Integers become `Int` or `Long`, other numbers `Double` or `BigDecimal`, and arrays `List`. Strings that
   * are ISO dates, ISO date times or UUIDs become `java.time.LocalDate`, `java.time.LocalDateTime` or `java.util.UUID`. When the
   * elements of an array, or the objects of one name, differ, their types are unified and a field that is missing from some
   * of them becomes an `Option`.
   *
   * @param input
   *   The JSON text to read
   */
  def printCaseClasses(input: String): Unit =
    input.fromJson[ast.Json].toOption match {
      case Some(json) => println(scala.Console.CYAN + generate(json) + scala.Console.RESET)
      case _          => println(s"Invalid JSON: $input")
    }

  private[codegen] def pascalFormat(s: String): String = {
    val parts = s.split("[_\\-.]")
    parts.map(_.capitalize).mkString("")
  }

  private[codegen] def generate(json: ast.Json): String =
    render(unifyTypes(json))

}

/**
 * The kinds of JSON value that the generator distinguishes, used to build the case classes it prints.
 */
private[codegen] sealed trait JsonType extends Product with Serializable { self =>

  /**
   * Combines two inferred types into one that fits both, for example `Int` and `Long` into `Long`, or `null` and `String`
   * into `Option[String]`.
   *
   * Throws an `Exception` when the types cannot be combined.
   */
  def unify(that: JsonType): JsonType =
    (self, that) match {
      case (lhs, rhs) if lhs == rhs                 => lhs
      case (JLong, JInt)                            => JLong
      case (JInt, JLong)                            => JLong
      case (JDouble, JInt | JLong)                  => JDouble
      case (JInt | JLong, JDouble)                  => JDouble
      case (JBigDecimal, JDouble | JInt | JLong)    => JBigDecimal
      case (JDouble | JInt | JLong, JBigDecimal)    => JBigDecimal
      case (JObject(lhsFields), JObject(rhsFields)) => JObject(mergeFields(lhsFields, rhsFields))

      case (JOption(left), JNull)  => JOption(left)
      case (JNull, JOption(right)) => JOption(right)

      case (JOption(left), JOption(right)) => JOption(left unify right)

      case (left, JOption(right)) => JOption(left unify right)
      case (JOption(left), right) => JOption(left unify right)

      case (JNull, right) => JOption(right)
      case (left, JNull)  => JOption(left)

      case (JArray(left), JArray(right))                                                 => JArray(left unify right)
      case (CaseClass(left, leftFields), CaseClass(right, rightFields)) if left == right =>
        CaseClass(left, (leftFields unify rightFields).asInstanceOf[JObject])
      case (left, right) =>
        throw new Exception(s"""
                               |Cannot combine:
                               | LEFT: ${left.toString}
                               |RIGHT: ${right.toString}
                               |""".stripMargin)
    }

  /**
   * The Scala type name that is printed for this type.
   */
  def typeName: String = self match {
    case CaseClass(name, _)   => name
    case JObject(_)           => "RootObject"
    case JString              => "String"
    case JInt                 => "Int"
    case JLong                => "Long"
    case JDouble              => "Double"
    case JBigDecimal          => "BigDecimal"
    case JNull                => "null"
    case JBoolean             => "Boolean"
    case JLocalDate           => "java.time.LocalDate"
    case JLocalDateTime       => "java.time.LocalDateTime"
    case JUUID                => "java.util.UUID"
    case JOption(value)       => s"Option[${value.typeName}]"
    case JArray(value)        => s"List[${value.typeName}]"
    case Alternatives(values) => s"Alternatives[${values.map(_.typeName).mkString(", ")}]"
  }

  /**
   * Wraps `jsonType` in an `Option`, unless it already is one or is `null`.
   */
  def makeOptional(jsonType: JsonType): JsonType = jsonType match {
    case JNull          => JNull
    case JOption(value) => JOption(value)
    case other          => JOption(other)
  }

  /**
   * Combines the fields of two objects, unifying the types of fields with the same name.
   *
   * A field that only one side has becomes optional.
   */
  def mergeFields(
    lhs: ListMap[String, JsonType],
    rhs: ListMap[String, JsonType]
  ): ListMap[String, JsonType] = {
    val result = lhs.foldLeft(rhs) { case (acc, (name, lhsValue)) =>
      //        if (name == "thumbnails") {
      //          println(s"lhs: ${lhsValue.toString} rhs: ${rhs.get(name)}")
      //        }
      acc.get(name) match {
        case Some(rhsValue) => acc + (name -> lhsValue.unify(rhsValue))
        case _              => acc + (name -> makeOptional(lhsValue))
      }
    }

    // println((rhs.keySet -- result.keySet) ++ (result.keySet -- rhs.keySet))

    (rhs.keySet -- lhs.keySet).foldLeft(result) { case (acc, name) =>
      acc + (name -> makeOptional(rhs(name)))
    }
  }

}

/**
 * The kinds of JSON value that the generator distinguishes, and the functions that infer and render them.
 */
object JsonType {

  /**
   * A JSON object that is printed as a case class called `name`.
   */
  final case class CaseClass(name: String, fields: JObject) extends JsonType {
    /**
     * The name of the case class.
     */
    def displayName = name
  }

  /**
   * A JSON object, with the inferred type of each field in order.
   */
  final case class JObject(fields: ListMap[String, JsonType]) extends JsonType
  /**
   * A JSON string.
   */
  case object JString                                         extends JsonType
  /**
   * A JSON number that fits an `Int`.
   */
  case object JInt                                            extends JsonType
  /**
   * A JSON number that fits a `Long` but not an `Int`.
   */
  case object JLong                                           extends JsonType
  /**
   * A JSON number that is not an integer or is too large for a `Long`, up to `Double.MaxValue`.
   */
  case object JDouble                                         extends JsonType
  /**
   * A JSON number that is larger than `Double.MaxValue`.
   */
  case object JBigDecimal                                     extends JsonType
  /**
   * The JSON `null` value.
   */
  case object JNull                                           extends JsonType
  /**
   * A JSON boolean.
   */
  case object JBoolean                                        extends JsonType
  /**
   * A JSON string that is an ISO date.
   */
  case object JLocalDate                                      extends JsonType
  /**
   * A JSON string that is an ISO date and time.
   */
  case object JLocalDateTime                                  extends JsonType
  /**
   * A JSON string that is a UUID.
   */
  case object JUUID                                           extends JsonType
  /**
   * A type that may be missing or `null`.
   */
  final case class JOption(value: JsonType)                   extends JsonType
  /**
   * A JSON array whose elements have the type `value`.
   */
  final case class JArray(value: JsonType)                    extends JsonType
  /**
   * A set of alternative types.
   */
  final case class Alternatives(values: Chunk[JsonType])      extends JsonType

  /**
   * Renders all the case classes found in `jsonType`, once each, separated by blank lines.
   */
  def render(jsonType: JsonType): String = {
    val caseClasses = flattenCaseClasses(jsonType).distinct
    caseClasses.map(renderCaseClass).mkString("\n\n")
  }

  /**
   * Collects the case classes found in `jsonType` and in its nested fields, parents first.
   */
  def flattenCaseClasses(jsonType: JsonType): List[CaseClass] =
    jsonType match {
      case CaseClass(name, fields) =>
        CaseClass(name, fields) :: fields.fields.toList.flatMap(t => flattenCaseClasses(t._2))
      case JObject(fields) =>
        fields.values.flatMap(flattenCaseClasses).toList
      case JArray(values) =>
        flattenCaseClasses(values)
      case JOption(value) =>
        flattenCaseClasses(value)
      case _ =>
        Nil
    }

  /**
   * Renders one case class with its `JsonCodec` instance, as Scala source.
   */
  def renderCaseClass(clazz: CaseClass): String = {
    val fields = clazz.fields.fields.map { case (name, value) =>
      s"  $name: ${value.typeName}"
    }.mkString(",\n")

    s"""
final case class ${clazz.name}(
$fields
)

object ${clazz.name} {
  implicit val codec: JsonCodec[${clazz.name}] = DeriveJsonCodec.gen
}
         """.trim
  }

  /**
   * Infers the type of a JSON value.
   *
   * Numbers are typed by size, strings that parse as ISO date times, ISO dates or UUIDs are typed as such, objects become case classes named
   * after `key` in PascalCase (`RootObject` when there is no key), and the elements of an array are unified into one type.
   */
  def unifyTypes(json: Json, key: Option[String] = None): JsonType =
    json match {
      case Json.Null          => JNull
      case Json.Arr(elements) =>
        JArray(elements.map(unifyTypes(_, key)).reduce(_ unify _))
      case Json.Bool(_)     => JBoolean
      case Json.Str(string) =>
        val localDateTime =
          Try(LocalDateTime.parse(string, DateTimeFormatter.ISO_DATE_TIME)).toOption.map(_ => JLocalDateTime)
        lazy val localDate = Try(LocalDate.parse(string)).toOption.map(_ => JLocalDate)
        lazy val uuid      = Try(UUID.fromString(string)).toOption.map(_ => JUUID)
        localDateTime.orElse(localDate).orElse(uuid).getOrElse(JString)
      case Json.Num(bigDecimal) =>
        if (bigDecimal.isValidInt) JInt
        else if (bigDecimal.isValidLong) JLong
        else if (bigDecimal <= Double.MaxValue) JDouble
        else JBigDecimal
      case Json.Obj(fields) =>
        val result = JObject {
          fields.foldLeft(ListMap.empty[String, JsonType]) { case (acc, (name, value)) =>
            acc + (name -> unifyTypes(value, Some(name)))
          }
        }
        key match {
          case Some(key) => CaseClass(pascalFormat(key), result)
          case _         => CaseClass("RootObject", result)
        }
    }

}
