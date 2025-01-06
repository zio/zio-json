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

object Generator {

  /**
   * Renders the JSON string as a series of Scala case classes derived from the structure of the JSON.
   *
   * For example, the following JSON:
   *
   * {{{
   *   {
   *     "foo": "bar",
   *     "baz": {
   *       "qux": "quux"
   *     }
   *   }
   * }}}
   *
   * Would print the following Scala code to the console:
   *
   * {{{
   *   final case class RootObject(
   *     foo: String,
   *     baz: Baz
   *   )
   *
   *   object RootObject {
   *     implicit val codec: JsonCoder[RootObject] = DeriveJsonCodec.gen
   *   }
   *
   *   final case class Baz(
   *     qux: String
   *   )
   *
   *   object Baz {
   *   implicit val codec: JsonCoder[Baz] = DeriveJsonCodec.gen
   *   }
   * }}}
   */
  def printCaseClasses(input: String): Unit =
    input.fromJson[ast.Json].toOption match {
      case Some(json) => println(scala.Console.CYAN + generate(json) + scala.Console.RESET)
      case None       => println(s"Invalid JSON: ${input}")
    }

  private[codegen] def pascalFormat(s: String): String = {
    val parts = s.split("[_\\-.]")
    parts.map(_.capitalize).mkString("")
  }

  private[codegen] def generate(json: ast.Json): String =
    render(unifyTypes(json))

}

private[codegen] sealed trait JsonType extends Product with Serializable { self =>

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

      case (JArray(left), JArray(right)) => JArray(left unify right)
      case (CaseClass(left, leftFields), CaseClass(right, rightFields)) if left == right =>
        CaseClass(left, (leftFields unify rightFields).asInstanceOf[JObject])
      case (left, right) =>
        throw new Exception(s"""
                               |Cannot combine:
                               | LEFT: ${left.toString}
                               |RIGHT: ${right.toString}
                               |""".stripMargin)
    }

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

  def makeOptional(jsonType: JsonType): JsonType = jsonType match {
    case JNull          => JNull
    case JOption(value) => JOption(value)
    case other          => JOption(other)
  }

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
        case None           => acc + (name -> makeOptional(lhsValue))
      }
    }

    // println((rhs.keySet -- result.keySet) ++ (result.keySet -- rhs.keySet))

    (rhs.keySet -- lhs.keySet).foldLeft(result) { case (acc, name) =>
      acc + (name -> makeOptional(rhs(name)))
    }
  }

}

object JsonType {

  final case class CaseClass(name: String, fields: JObject) extends JsonType {
    def displayName = name
  }

  final case class JObject(fields: ListMap[String, JsonType]) extends JsonType
  case object JString                                         extends JsonType
  case object JInt                                            extends JsonType
  case object JLong                                           extends JsonType
  case object JDouble                                         extends JsonType
  case object JBigDecimal                                     extends JsonType
  case object JNull                                           extends JsonType
  case object JBoolean                                        extends JsonType
  case object JLocalDate                                      extends JsonType
  case object JLocalDateTime                                  extends JsonType
  case object JUUID                                           extends JsonType
  final case class JOption(value: JsonType)                   extends JsonType
  final case class JArray(value: JsonType)                    extends JsonType
  final case class Alternatives(values: Chunk[JsonType])      extends JsonType

  def render(jsonType: JsonType): String = {
    val caseClasses = flattenCaseClasses(jsonType).distinct
    caseClasses.map(renderCaseClass).mkString("\n\n")
  }

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

  def unifyTypes(json: Json, key: Option[String] = None): JsonType =
    json match {
      case Json.Null => JNull
      case Json.Arr(elements) =>
        JArray(elements.map(unifyTypes(_, key)).reduce(_ unify _))
      case Json.Bool(_) => JBoolean
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
          case None      => CaseClass("RootObject", result)
        }
    }

}
