package zio.json

import zio.json.ast.Json
import zio.test._
import zio.Chunk

import scala.collection.immutable
import scala.collection.mutable

object AnnotationsCodecSpec extends ZIOSpecDefault {

  def spec = suite("AnnotationsCodecSpec")(
    suite("annotations overrides")(
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
        @jsonExplicitEmptyCollections(false)
        case class EmptySeq(a: Seq[Int])

        val expectedStr = """{}"""

        implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

        assertTrue(EmptySeq(Seq.empty).toJson == expectedStr)
      }
    ),
    suite("annotations overrides AST")(
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
        @jsonExplicitEmptyCollections(false)
        case class EmptySeq(a: Seq[Int])

        val jsonAST = Json.Obj()

        implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

        assertTrue(EmptySeq(Seq.empty).toJsonAST == Right(jsonAST))
      }
    ),
    suite("explicit empty collections")(
      suite("should fill in missing empty collections and write empty collections")(
        test("for an array") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyArray].toOption.exists(_.a.isEmpty), expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptySeq].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyChunk].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyIndexedSeq].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a linear seq") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyLinearSeq].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list set") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyListSet].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a tree set") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyTreeSet].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyList(a: List[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyList(List.empty)

          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyList].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a vector") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyVector].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a set") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptySet(a: Set[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptySet].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a hash set") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyHashSet].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted set") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptySortedSet].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a map") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a hash map") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyHashMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a mutable map") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyMutableMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted map") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptySortedMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list map") {
          @jsonExplicitEmptyCollections(true, whenDecoding = false)
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyListMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        }
      ),
      suite("should not write empty collections and fail missing empty collections")(
        test("for an array") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyArray].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          @jsonExplicitEmptyCollections(false)
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyChunk].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyIndexedSeq].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a linear seq") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyLinearSeq].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a list set") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a treeSet") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyTreeSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyList(a: List[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyList(List.empty)

          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyList].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a vector") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyVector].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          @jsonExplicitEmptyCollections(false)
          case class EmptySet(a: Set[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          @jsonExplicitEmptyCollections(false)
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySortedSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a map") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a hashMap") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyMutableMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a sorted map") {
          @jsonExplicitEmptyCollections(false)
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySortedMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a list map") {
          @jsonExplicitEmptyCollections(false)
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListMap].isLeft, expectedObj.toJson == expectedStr)
        }
      )
    )
  )
}
