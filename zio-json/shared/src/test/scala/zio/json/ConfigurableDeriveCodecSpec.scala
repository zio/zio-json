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
      },
      test("do not write nulls by default") {
        val expectedStr = """{}"""
        val expectedObj = OptionalField(None)

        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(
          expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
          expectedObj.toJson == expectedStr
        )
      },
      test("do not fail on missing null values") {
        val expectedStr = """{}"""
        val expectedObj = OptionalField(None)

        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(expectedStr.fromJson[OptionalField].toOption.get == expectedObj, expectedObj.toJson == expectedStr)
      },
      test("write empty collections by default") {
        case class Empty(z: Option[Int])
        case class EmptyObj(a: Empty)
        case class EmptySeq(a: Seq[Int])

        val expectedObjStr = """{"a":{}}"""
        val expectedSeqStr = """{"a":[]}"""
        val expectedObj    = EmptyObj(Empty(None))
        val expectedSeq    = EmptySeq(Seq.empty)

        implicit val emptyCodec: JsonCodec[Empty]       = DeriveJsonCodec.gen
        implicit val emptyObjCodec: JsonCodec[EmptyObj] = DeriveJsonCodec.gen
        implicit val emptySeqCodec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

        assertTrue(
          expectedObjStr.fromJson[EmptyObj].toOption.get == expectedObj,
          expectedObj.toJson == expectedObjStr,
          expectedSeqStr.fromJson[EmptySeq].toOption.get == expectedSeq,
          expectedSeq.toJson == expectedSeqStr
        )
      },
      test("fail on decoding missing empty collections by default") {
        case class Empty(z: Option[Int])
        case class EmptyObj(a: Empty)
        case class EmptySeq(b: Seq[Int])

        implicit val codecEmpty: JsonCodec[Empty]       = DeriveJsonCodec.gen[Empty]
        implicit val codecEmptyObj: JsonCodec[EmptyObj] = DeriveJsonCodec.gen[EmptyObj]
        implicit val codecEmptySeq: JsonCodec[EmptySeq] = DeriveJsonCodec.gen[EmptySeq]

        assertTrue(
          """{}""".fromJson[EmptyObj] == Left(".a(missing)"),
          """{}""".fromJson[EmptySeq] == Left(".b(missing)")
        )
      }
    ),
    suite("AST defaults")(
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
      },
      test("do not write nulls by default") {
        val jsonAST     = Json.Obj()
        val expectedObj = OptionalField(None)

        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(
          jsonAST.as[OptionalField].toOption.get == expectedObj,
          expectedObj.toJsonAST == Right(jsonAST)
        )
      },
      test("write empty collections by default") {
        case class Empty(z: Option[Int])
        case class EmptyObj(a: Empty)
        case class EmptySeq(a: Seq[Int])

        val expectedSeqJson = Json.Obj("a" -> Json.Arr())
        val expectedObjJson = Json.Obj("a" -> Json.Obj())
        val expectedObj     = EmptyObj(Empty(None))
        val expectedSeq     = EmptySeq(Seq.empty)

        implicit val emptyCodec: JsonCodec[Empty]       = DeriveJsonCodec.gen
        implicit val emptyObjCodec: JsonCodec[EmptyObj] = DeriveJsonCodec.gen
        implicit val emptySeqCodec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

        assertTrue(
          expectedObj.toJsonAST == Right(expectedObjJson),
          expectedSeq.toJsonAST == Right(expectedSeqJson),
          expectedObjJson.as[EmptyObj] == Right(expectedObj),
          expectedSeqJson.as[EmptySeq] == Right(expectedSeq)
        )
      },
      test("fail on decoding missing empty collections by default") {
        case class Empty(z: Option[Int])
        case class EmptyObj(a: Empty)
        case class EmptySeq(b: Seq[Int])

        implicit val codecEmpty: JsonDecoder[Empty]       = DeriveJsonDecoder.gen[Empty]
        implicit val codecEmptyObj: JsonDecoder[EmptyObj] = DeriveJsonDecoder.gen[EmptyObj]
        implicit val codecEmptySeq: JsonDecoder[EmptySeq] = DeriveJsonDecoder.gen[EmptySeq]

        assertTrue(
          Json.Obj().as[EmptyObj] == Left(".a(missing)"),
          Json.Obj().as[EmptySeq] == Left(".b(missing)")
        )
      }
    ),
    suite("override defaults")(
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
      },
      test("use explicit null values") {
        val expectedStr = """{"a":null}"""
        val expectedObj = OptionalField(None)

        implicit val config: JsonCodecConfiguration =
          JsonCodecConfiguration(explicitNulls = true)
        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(
          expectedStr.fromJson[OptionalField].toOption.get == expectedObj,
          expectedObj.toJson == expectedStr
        )
      },
      test("do not write empty collections") {
        case class Empty(z: Option[Int])
        case class EmptyObj(a: Empty)
        case class EmptySeq(b: Seq[Int])

        val expectedStr      = """{}"""
        val expectedEmptyObj = EmptyObj(Empty(None))
        val expectedEmptySeq = EmptySeq(Seq.empty)

        implicit val config: JsonCodecConfiguration = JsonCodecConfiguration(explicitEmptyCollections =
          ExplicitEmptyCollections(whenDecoding = false, whenEncoding = false)
        )
        implicit val codecEmpty: JsonCodec[Empty]       = DeriveJsonCodec.gen[Empty]
        implicit val codecEmptyObj: JsonCodec[EmptyObj] = DeriveJsonCodec.gen[EmptyObj]
        implicit val codecEmptySeq: JsonCodec[EmptySeq] = DeriveJsonCodec.gen[EmptySeq]

        assertTrue(
          expectedEmptyObj.toJson == expectedStr,
          expectedEmptySeq.toJson == expectedStr,
          expectedStr.fromJson[EmptyObj] == Right(expectedEmptyObj),
          expectedStr.fromJson[EmptySeq] == Right(expectedEmptySeq)
        )
      },
      test("decode missing empty collections with defaults") {
        case class EmptySeq(b: Seq[Int] = Seq(1))
        case class EmptyObj(a: EmptySeq)

        val expectedStr = """{}"""
        val expectedSeq = EmptySeq(Seq(1))
        val expectedObj = EmptyObj(expectedSeq)

        implicit val config: JsonCodecConfiguration =
          JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
        implicit val codec: JsonCodec[EmptySeq]    = DeriveJsonCodec.gen
        implicit val codecObj: JsonCodec[EmptyObj] = DeriveJsonCodec.gen

        assertTrue(
          expectedStr.fromJson[EmptySeq].toOption.get == expectedSeq,
          expectedStr.fromJson[EmptyObj].toOption.get == expectedObj
        )
      }
    ),
    suite("override AST defaults")(
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
      },
      test("use explicit null values") {
        val jsonAST     = Json.Obj("a" -> Json.Null)
        val expectedObj = OptionalField(None)

        implicit val config: JsonCodecConfiguration =
          JsonCodecConfiguration(explicitNulls = true)
        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(jsonAST.as[OptionalField].toOption.get == expectedObj, expectedObj.toJsonAST == Right(jsonAST))
      },
      test("fail on decoding missing explicit nulls") {
        val jsonStr = """{}"""

        implicit val config: JsonCodecConfiguration =
          JsonCodecConfiguration(explicitNulls = true)
        implicit val codec: JsonCodec[OptionalField] = DeriveJsonCodec.gen

        assertTrue(jsonStr.fromJson[OptionalField].isLeft)
      } @@ TestAspect.ignore,
      test("do not write empty collections") {
        case class Empty(z: Option[Int])
        case class EmptyObj(a: Empty)
        case class EmptySeq(b: Seq[Int])

        val expectedJson     = Json.Obj()
        val expectedEmptyObj = EmptyObj(Empty(None))
        val expectedEmptySeq = EmptySeq(Seq.empty)

        implicit val config: JsonCodecConfiguration = JsonCodecConfiguration(explicitEmptyCollections =
          ExplicitEmptyCollections(whenDecoding = false, whenEncoding = false)
        )
        implicit val emptyCodec: JsonCodec[Empty]       = DeriveJsonCodec.gen
        implicit val emptyObjCodec: JsonCodec[EmptyObj] = DeriveJsonCodec.gen
        implicit val emptySeqCodec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

        assertTrue(
          expectedEmptyObj.toJsonAST == Right(expectedJson),
          expectedEmptySeq.toJsonAST == Right(expectedJson),
          expectedJson.as[EmptyObj] == Right(expectedEmptyObj),
          expectedJson.as[EmptySeq] == Right(expectedEmptySeq)
        )
      }
    ),
    suite("explicit empty collections")(
      suite("should fill in missing empty collections and write empty collections")(
        test("for an array") {
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyArray].toOption.exists(_.a.isEmpty), expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptySeq].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyChunk].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyIndexedSeq].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a linear seq") {
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyLinearSeq].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list set") {
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyListSet].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a tree set") {
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyTreeSet].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          case class EmptyList(a: List[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyList(List.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyList].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a vector") {
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyVector].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          case class EmptySet(a: Set[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptySet].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyHashSet].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{"a":[]}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptySortedSet].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a map") {
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyMap].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a hash map") {
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyHashMap].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptyMutableMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a sorted map") {
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(
            """{}""".fromJson[EmptySortedMap].toOption.contains(expectedObj),
            expectedObj.toJson == expectedStr
          )
        },
        test("for a list map") {
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{"a":{}}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(whenDecoding = false))
          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue("""{}""".fromJson[EmptyListMap].toOption.contains(expectedObj), expectedObj.toJson == expectedStr)
        }
      ),
      suite("should not write empty collections and fail missing empty collections")(
        test("for an array") {
          case class EmptyArray(a: Array[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyArray(Array.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyArray] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyArray].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a seq") {
          case class EmptySeq(a: Seq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySeq(Seq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptySeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySeq].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a chunk") {
          case class EmptyChunk(a: Chunk[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyChunk(Chunk.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyChunk] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyChunk].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for an indexed seq") {
          case class EmptyIndexedSeq(a: IndexedSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyIndexedSeq(IndexedSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyIndexedSeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyIndexedSeq].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a linear seq") {
          case class EmptyLinearSeq(a: immutable.LinearSeq[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyLinearSeq(immutable.LinearSeq.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyLinearSeq] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyLinearSeq].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a list set") {
          case class EmptyListSet(a: immutable.ListSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyListSet(immutable.ListSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyListSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a treeSet") {
          case class EmptyTreeSet(a: immutable.TreeSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyTreeSet(immutable.TreeSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyTreeSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyTreeSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a list") {
          case class EmptyList(a: List[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyList(List.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyList] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyList].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a vector") {
          case class EmptyVector(a: Vector[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyVector(Vector.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyVector] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyVector].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a set") {
          case class EmptySet(a: Set[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySet(Set.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptySet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a hash set") {
          case class EmptyHashSet(a: immutable.HashSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashSet(immutable.HashSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyHashSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a sorted set") {
          case class EmptySortedSet(a: immutable.SortedSet[Int])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedSet(immutable.SortedSet.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptySortedSet] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySortedSet].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a map") {
          case class EmptyMap(a: Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMap(Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a hashMap") {
          case class EmptyHashMap(a: immutable.HashMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyHashMap(immutable.HashMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyHashMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyHashMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a mutable map") {
          case class EmptyMutableMap(a: mutable.Map[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyMutableMap(mutable.Map.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyMutableMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyMutableMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a sorted map") {
          case class EmptySortedMap(a: collection.SortedMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptySortedMap(collection.SortedMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptySortedMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptySortedMap].isLeft, expectedObj.toJson == expectedStr)
        },
        test("for a list map") {
          case class EmptyListMap(a: immutable.ListMap[String, String])
          val expectedStr = """{}"""
          val expectedObj = EmptyListMap(immutable.ListMap.empty)

          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(false))
          implicit val codec: JsonCodec[EmptyListMap] = DeriveJsonCodec.gen

          assertTrue(expectedStr.fromJson[EmptyListMap].isLeft, expectedObj.toJson == expectedStr)
        }
      )
    )
  )
}
