package zio.json

import zio.json.JsonCodecConfiguration.SumTypeHandling.DiscriminatorField
import zio.json.ast.Json
import zio.test._
import zio.Chunk

import scala.collection.immutable
import scala.collection.mutable

object AnnotationsCodecSpec extends ZIOSpecDefault {

  def spec = suite("ConfigurableDeriveCodecSpec")(
    suite("annotations overrides")(
      suite("string")(
        test("should override field name mapping") {
          @jsonMemberNames(SnakeCase)
          case class ClassWithFields(someField: Int, someOtherField: String)

          val expectedStr = """{"some_field":1,"some_other_field":"a"}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should specify discriminator") {
          @jsonDiscriminator("$type")
          sealed trait ST

          object ST {
            case object CaseObj          extends ST
            case class CaseClass(i: Int) extends ST

            implicit lazy val codec: JsonCodec[ST] = DeriveJsonCodec.gen
          }

          val expectedStr     = """{"$type":"CaseClass","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should override sum type mapping") {
          @jsonHintNames(SnakeCase)
          @jsonDiscriminator("$type")
          sealed trait ST

          object ST {
            case object CaseObj          extends ST
            case class CaseClass(i: Int) extends ST

            implicit lazy val codec: JsonCodec[ST] = DeriveJsonCodec.gen
          }

          val expectedStr     = """{"$type":"case_class","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should prevent extra fields") {
          @jsonNoExtraFields
          case class ClassWithFields(someField: Int, someOtherField: String)

          val jsonStr = """{"someField":1,"someOtherField":"a","extra":123}"""

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonStr.fromJson[ClassWithFields].isLeft
          )
        },
        test("use explicit null values") {
          @jsonExplicitNull
          case class OptionalField(a: Option[Int])

          val expectedStr = """{"a":null}"""
          val expectedObj = OptionalField(None)

          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("do not write empty collections") {
          @jsonExplicitEmptyCollection(false)
          case class EmptySeq(a: Seq[Int])

          val expectedStr = """{}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        }
      ),
      suite("AST")(
        test("should override field name mapping") {
          @jsonMemberNames(SnakeCase)
          case class ClassWithFields(someField: Int, someOtherField: String)

          val expectedAST = Json.Obj("some_field" -> Json.Num(1), "some_other_field" -> Json.Str("a"))
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should specify discriminator") {
          @jsonDiscriminator("$type")
          sealed trait ST

          object ST {
            case object CaseObj          extends ST
            case class CaseClass(i: Int) extends ST

            implicit lazy val codec: JsonCodec[ST] = DeriveJsonCodec.gen
          }

          val expectedAST     = Json.Obj("$type" -> Json.Str("CaseClass"), "i" -> Json.Num(1))
          val expectedObj: ST = ST.CaseClass(i = 1)

          assertTrue(
            expectedAST.as[ST].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should prevent extra fields") {
          @jsonNoExtraFields
          case class ClassWithFields(someField: Int, someOtherField: String)

          val jsonAST = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"), "extra" -> Json.Num(1))

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[ClassWithFields].isLeft
          )
        },
        test("use explicit null values") {
          @jsonExplicitNull
          case class OptionalField(a: Option[Int])

          val jsonAST     = Json.Obj("a" -> Json.Null)
          val expectedObj = OptionalField(None)

          implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

          assertTrue(jsonAST.as[OptionalField].toOption.get == expectedObj, expectedObj.toJsonAST == Right(jsonAST))
        },
        test("do not write empty collections") {
          @jsonExplicitEmptyCollection(false)
          case class EmptySeq(a: Seq[Int])

          val jsonAST     = Json.Obj("a" -> Json.Arr())
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(jsonAST.as[EmptySeq].toOption.get == expectedObj, expectedObj.toJsonAST == Right(jsonAST))
        }
      )
    ),
    suite("explicit empty collections")(
      suite("should write empty collections if set to true")(
        test("for an array") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyArray].toOption.get.a.isEmpty, expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          @jsonExplicitEmptyCollection(true)
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyChunk].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyIndexedSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a linear seq") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyLinearSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list set") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a tree set") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyTreeSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyList(a: List[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyList(List.empty)

          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyList].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a vector") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyVector].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          @jsonExplicitEmptyCollection(true)
          case class EmptySet(a: Set[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          @jsonExplicitEmptyCollection(true)
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedSet].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a map") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a hash map") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyMutableMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted map") {
          @jsonExplicitEmptyCollection(true)
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list map") {
          @jsonExplicitEmptyCollection(true)
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        }
      ),
      suite("should not write empty collections if set to false")(
        test("for an array") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyArray].toOption.get.a.isEmpty, expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          @jsonExplicitEmptyCollection(false)
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyChunk].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyIndexedSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a linear seq") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyLinearSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list set") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a treeSet") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyTreeSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyList(a: List[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyList(List.empty)

          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyList].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a vector") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyVector].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          @jsonExplicitEmptyCollection(false)
          case class EmptySet(a: Set[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          @jsonExplicitEmptyCollection(false)
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedSet].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a map") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a hashMap") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyMutableMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted map") {
          @jsonExplicitEmptyCollection(false)
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list map") {
          @jsonExplicitEmptyCollection(false)
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        }
      )
    )
  )
}
