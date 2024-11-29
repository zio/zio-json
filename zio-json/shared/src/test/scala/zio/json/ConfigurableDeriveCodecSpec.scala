package zio.json

import zio.json.JsonCodecConfiguration.SumTypeHandling.DiscriminatorField
import zio.json.ast.Json
import zio.test._
import zio.Chunk

import scala.collection.immutable
import scala.collection.mutable

object ConfigurableDeriveCodecSpec extends ZIOSpecDefault {
  case class ClassWithFields(someField: Int, someOtherField: String)

  sealed trait ST

  object ST {
    case object CaseObj          extends ST
    case class CaseClass(i: Int) extends ST
  }

  case class OptionalField(a: Option[Int])

  def spec = suite("ConfigurableDeriveCodecSpec")(
    suite("defaults")(
      suite("string")(
        test("should not map field names by default") {
          val expectedStr = """{"someField":1,"someOtherField":"a"}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should not use discriminator by default") {
          val expectedStr     = """{"CaseObj":{}}"""
          val expectedObj: ST = ST.CaseObj

          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should allow extra fields by default") {
          val jsonStr     = """{"someField":1,"someOtherField":"a","extra":123}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonStr.fromJson[ClassWithFields].toOption.get == expectedObj
          )
        }
      ),
      suite("AST")(
        test("should not map field names by default") {
          val expectedAST = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"))
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should not use discriminator by default") {
          val expectedAST     = Json.Obj("CaseObj" -> Json.Obj())
          val expectedObj: ST = ST.CaseObj

          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ST].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should allow extra fields by default") {
          val jsonAST     = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"), "extra" -> Json.Num(1))
          val expectedObj = ClassWithFields(1, "a")

          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[ClassWithFields].toOption.get == expectedObj
          )
        }
      )
    ),
    suite("overrides")(
      suite("string")(
        test("should override field name mapping") {
          val expectedStr = """{"some_field":1,"some_other_field":"a"}"""
          val expectedObj = ClassWithFields(1, "a")

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(fieldNameMapping = SnakeCase)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should specify discriminator") {
          val expectedStr     = """{"$type":"CaseClass","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("$type"))
          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should override sum type mapping") {
          val expectedStr     = """{"$type":"case_class","i":1}"""
          val expectedObj: ST = ST.CaseClass(i = 1)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("$type"), sumTypeMapping = SnakeCase)
          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[ST].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("should prevent extra fields") {
          val jsonStr = """{"someField":1,"someOtherField":"a","extra":123}"""

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(allowExtraFields = false)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonStr.fromJson[ClassWithFields].isLeft
          )
        }
      ),
      suite("AST")(
        test("should override field name mapping") {
          val expectedAST = Json.Obj("some_field" -> Json.Num(1), "some_other_field" -> Json.Str("a"))
          val expectedObj = ClassWithFields(1, "a")

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(fieldNameMapping = SnakeCase)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ClassWithFields].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should specify discriminator") {
          val expectedAST     = Json.Obj("$type" -> Json.Str("CaseClass"), "i" -> Json.Num(1))
          val expectedObj: ST = ST.CaseClass(i = 1)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(sumTypeHandling = DiscriminatorField("$type"))
          implicit val codec: JsonCodec[ST] = DeriveJsonCodec.gen

          assertTrue(
            expectedAST.as[ST].toOption.get == expectedObj,
            expectedObj.toJsonAST.toOption.get == expectedAST
          )
        },
        test("should prevent extra fields") {
          val jsonAST = Json.Obj("someField" -> Json.Num(1), "someOtherField" -> Json.Str("a"), "extra" -> Json.Num(1))

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(allowExtraFields = false)
          implicit val codec: JsonCodec[ClassWithFields] = DeriveJsonCodec.gen

          assertTrue(
            jsonAST.as[ClassWithFields].isLeft
          )
        }
      )
    ),
    suite("explicit nulls")(
      test("write null if configured") {
        val expectedStr = """{"a":null}"""
        val expectedObj = OptionalField(None)

        implicit val config: JsonCodecConfiguration =
          JsonCodecConfiguration(explicitNulls = true)
        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(
          expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
          expectedObj.toJson == expectedStr
        )
      }
    ),
    suite("explicit empty collections")(
      suite("should write empty collections if set to true")(
        test("for an array") {
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyArray].toOption.get.a.isEmpty, expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyChunk].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyIndexedSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a linear seq") {
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyLinearSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list set") {
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a tree set") {
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyTreeSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          case class EmptyList(a: List[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyList(List.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyList].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a vector") {
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyVector].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          case class EmptySet(a: Set[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedSet].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a map") {
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a hash map") {
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyMutableMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted map") {
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list map") {
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = true)
          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        }
      ),
      suite("should not write empty collections if set to false")(
        test("for an array") {
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyArray].toOption.get.a.isEmpty, expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyChunk].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyIndexedSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a linear seq") {
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyLinearSeq].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list set") {
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a treeSet") {
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyTreeSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          case class EmptyList(a: List[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyList(List.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyList].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a vector") {
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyVector].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          case class EmptySet(a: Set[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashSet].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedSet].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a map") {
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a hashMap") {
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptyMutableMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted map") {
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(
            expectedStr.fromJson[EmptySortedMap].toOption.get == expectedObj,
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list map") {
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = false)
          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListMap].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
        }
      )
    )
  )
}
