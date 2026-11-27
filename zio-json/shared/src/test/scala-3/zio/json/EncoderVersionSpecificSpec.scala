package zio.json

import zio.json.ast.Json
import zio.json.internal.Write
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

object EncoderVersionSpecificSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("EncoderVersionSpecific")(
      suite("toJson")(
        test("collections") {
          assert(immutable.ArraySeq[Int]().toJson)(equalTo("[]")) &&
          assert(immutable.ArraySeq(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(immutable.ArraySeq[String]().toJsonPretty)(equalTo("[]")) &&
          assert(immutable.ArraySeq("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]"))
        },
        test("IArray") {
          assert(IArray.empty[Int].toJson)(equalTo("[]")) &&
          assert(IArray(1, 2, 3).toJson)(equalTo("[1,2,3]")) &&
          assert(IArray.empty[String].toJsonPretty)(equalTo("[]")) &&
          assert(IArray("foo", "bar").toJsonPretty)(equalTo("[\n  \"foo\",\n  \"bar\"\n]"))
        },
        test("Derives for a product type") {
          case class Foo(bar: String) derives JsonEncoder

          val json = Foo("bar").toJson
          assertTrue(json == """{"bar":"bar"}""")
        },
        test("Derives for a sum enum Enumeration type") {
          enum Foo derives JsonEncoder:
            case Bar
            case Baz
            case Qux

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """"Qux"""")
        },
        test("Derives for a sum enum Enumeration type with enumValuesAsStrings = false") {
          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(enumValuesAsStrings = false)

          enum Foo derives JsonEncoder:
            case Bar
            case Baz
            case Qux

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """{"Qux":{}}""")
        },
        test("Derives for a sum enum Enumeration type with discriminator") {
          @jsonDiscriminator("$type")
          enum Foo derives JsonEncoder:
            case Bar
            case Baz
            case Qux

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """{"$type":"Qux"}""")
        },
        test("Derives for a sum sealed trait Enumeration type") {
          sealed trait Foo derives JsonEncoder
          object Foo:
            case object Bar extends Foo
            case object Baz extends Foo
            case object Qux extends Foo

          val json = (Foo.Qux: Foo).toJson
          assertTrue(json == """"Qux"""")
        },
        test("Derives for a sum ADT type") {
          enum Foo derives JsonEncoder:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          val json = (Foo.Qux(Foo.Bar): Foo).toJson
          assertTrue(json == """{"Qux":{"foo":{"Bar":{}}}}""")
        },
        test("Derives and encodes for a union of string-based literals") {
          case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonEncoder

          assertTrue(Foo("A", Some("A")).toJson == """{"aOrB":"A","optA":"A"}""")
        },
        test("Derives and encodes for a custom map key string-based union type") {
          case class Foo(aOrB: Map["A" | "B", Int]) derives JsonEncoder

          assertTrue(Foo(Map("A" -> 1, "B" -> 2)).toJson == """{"aOrB":{"A":1,"B":2}}""")
        },
        test("@jsonExplicitEmptyCollections encodes empty collections") {
          import WithExplicitEmpty._
          // With annotation (encoding=true, the default), empty list SHOULD be written as "items":[]
          val result = WithExplicitEmpty(List.empty, "test").toJson
          assertTrue(result == """{"items":[],"name":"test"}""")
        },
        test("@jsonExplicitEmptyCollections(encoding = false) omits empty collections") {
          import WithExplicitEmptyEncodingOff._
          // With encoding=false, empty list should be omitted
          val result = WithExplicitEmptyEncodingOff(List.empty, "test").toJson
          assertTrue(result == """{"name":"test"}""")
        },
        test("@jsonExplicitEmptyCollections encodes non-empty collections normally") {
          import WithExplicitEmpty._
          val result = WithExplicitEmpty(List(1, 2, 3), "test").toJson
          assertTrue(result == """{"items":[1,2,3],"name":"test"}""")
        }
      ),
      suite("toJsonAST")(
        test("collections") {
          val arrEmpty = Json.Arr()
          val arr123   = Json.Arr(Json.Num(1), Json.Num(2), Json.Num(3))

          assert(immutable.ArraySeq[Int]().toJsonAST)(isRight(equalTo(arrEmpty))) &&
          assert(immutable.ArraySeq(1, 2, 3).toJsonAST)(isRight(equalTo(arr123)))
        }
      ),
      suite("named tuples")(
        test("encodes a single value tuple") {
          type Person = (name: String)
          assertTrue(((name = "John"): Person).toJson == """{"name":"John"}""")
        },
        test("encodes multi value hetrogenous tuple") {
          type Person = (name: String, age: Int)
          assertTrue(((name = "John", age = 30): Person).toJson == """{"name":"John","age":30}""")
        },
        test("encode multi value with optional value present") {
          type Person = (name: String, age: Option[Int])
          assertTrue(((name = "John", age = Some(30)): Person).toJson == """{"name":"John","age":30}""")
        },
        test("encode multi value with optional value absent") {
          type Person = (name: String, age: Option[Int])
          assertTrue(((name = "John", age = None): Person).toJson == """{"name":"John"}""")
        },
        test("supports json configuration overrides") {
          type Person = (firstName: String, lastName: Option[String])
          implicit val config: JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)
          assertTrue(
            ((
              firstName = "John",
              lastName = Some("Doe")
            ): Person).toJson == """{"first_name":"John","last_name":"Doe"}"""
          )
        },
        test("encodes to a JSON AST") {
          type Person = (name: String, age: Int)
          assertTrue(
            ((name = "John", age = 30): Person).toJsonAST ==
              Right(Json.Obj("name" -> Json.Str("John"), "age" -> Json.Num(30)))
          )
        },
        test("omits absent optional values from the JSON AST") {
          type Person = (name: String, age: Option[Int], nickname: Option[String])
          assertTrue(
            ((name = "John", age = None, nickname = Some("JJ")): Person).toJsonAST ==
              Right(Json.Obj("name" -> Json.Str("John"), "nickname" -> Json.Str("JJ"))),
            ((name = "John", age = None, nickname = None): Person).toJsonAST ==
              Right(Json.Obj("name" -> Json.Str("John")))
          )
        },
        test("applies field name mapping to the JSON AST") {
          type Person = (firstName: String, lastName: String)
          implicit val config: JsonCodecConfiguration = JsonCodecConfiguration(fieldNameMapping = SnakeCase)
          assertTrue(
            ((firstName = "John", lastName = "Doe"): Person).toJsonAST ==
              Right(Json.Obj("first_name" -> Json.Str("John"), "last_name" -> Json.Str("Doe")))
          )
        },
        test("propagates a failing field encoder from toJsonAST") {
          final case class Opaque(value: Int)
          implicit val opaqueEncoder: JsonEncoder[Opaque] = new JsonEncoder[Opaque] {
            def unsafeEncode(a: Opaque, indent: Option[Int], out: Write): Unit = out.write(a.value.toString)
            override def toJsonAST(a: Opaque): Either[String, Json]            = Left("boom")
          }
          type Row = (id: Int, payload: Opaque)
          val row: Row = (id = 1, payload = Opaque(2))
          assertTrue(row.toJson == """{"id":1,"payload":2}""", row.toJsonAST == Left("boom"))
        },
        test("pretty prints") {
          type Person = (name: String, age: Option[Int], score: Int)
          assertTrue(
            ((name = "John", age = Some(30), score = 1): Person).toJsonPretty ==
              "{\n  \"name\" : \"John\",\n  \"age\" : 30,\n  \"score\" : 1\n}",
            ((name = "John", age = None, score = 1): Person).toJsonPretty ==
              "{\n  \"name\" : \"John\",\n  \"score\" : 1\n}"
          )
        },
        test("encodes nested named tuples") {
          type Person = (name: String, address: (city: String, zip: Option[String]))
          val person: Person = (name = "John", address = (city = "Paris", zip = None))
          assertTrue(
            person.toJson == """{"name":"John","address":{"city":"Paris"}}""",
            person.toJsonPretty == "{\n  \"name\" : \"John\",\n  \"address\" : {\n    \"city\" : \"Paris\"\n  }\n}",
            person.toJsonAST == Right(
              Json.Obj("name" -> Json.Str("John"), "address" -> Json.Obj("city" -> Json.Str("Paris")))
            )
          )
        },
        test("does not emit a leading comma when the first field is skipped") {
          type Person = (age: Option[Int], name: String, nickname: Option[String])
          assertTrue(
            ((age = None, name = "John", nickname = None): Person).toJson == """{"name":"John"}""",
            ((age = None, name = "John", nickname = Some("JJ")): Person).toJson == """{"name":"John","nickname":"JJ"}"""
          )
        },
        test("encodes an empty object when every field is skipped") {
          type Person = (age: Option[Int], nickname: Option[String])
          val person: Person = (age = None, nickname = None)
          assertTrue(person.toJson == "{}", person.toJsonAST == Right(Json.Obj()))
        },
        test("encodes the empty named tuple") {
          assertTrue(
            NamedTuple.Empty.toJson == "{}",
            NamedTuple.Empty.toJsonPretty == "{}",
            NamedTuple.Empty.toJsonAST == Right(Json.Obj()),
            NamedTuple.Empty.toJson.fromJson[NamedTuple.Empty] == Right(NamedTuple.Empty)
          )
        },
        test("encodes an empty named tuple nested in another") {
          type Event = (name: String, meta: NamedTuple.Empty)
          val event: Event = (name = "click", meta = NamedTuple.Empty)
          assertTrue(
            event.toJson == """{"name":"click","meta":{}}""",
            event.toJsonPretty == "{\n  \"name\" : \"click\",\n  \"meta\" : {}\n}",
            event.toJsonAST == Right(Json.Obj("name" -> Json.Str("click"), "meta" -> Json.Obj())),
            event.toJson.fromJson[Event] == Right(event)
          )
        },
        test("encodes an empty named tuple the same way as an empty case class") {
          final case class Empty()
          val ccEncoder: JsonEncoder[Empty] = DeriveJsonEncoder.gen[Empty]
          assertTrue(
            NamedTuple.Empty.toJson == ccEncoder.encodeJson(Empty(), None).toString,
            NamedTuple.Empty.toJsonPretty == ccEncoder.encodeJson(Empty(), Some(0)).toString,
            NamedTuple.Empty.toJsonAST == ccEncoder.toJsonAST(Empty())
          )
        },
        test("encodes absent optional values as null with explicitNulls") {
          type Person = (name: String, age: Option[Int])
          implicit val config: JsonCodecConfiguration = JsonCodecConfiguration(explicitNulls = true)
          val person: Person                          = (name = "John", age = None)
          assertTrue(
            person.toJson == """{"name":"John","age":null}""",
            person.toJsonAST == Right(Json.Obj("name" -> Json.Str("John"), "age" -> Json.Null))
          )
        },
        test("encodes empty collections by default") {
          type Basket = (owner: String, items: List[Int])
          val basket: Basket = (owner = "me", items = Nil)
          assertTrue(
            basket.toJson == """{"owner":"me","items":[]}""",
            basket.toJsonAST == Right(Json.Obj("owner" -> Json.Str("me"), "items" -> Json.Arr()))
          )
        },
        test("omits empty collections when explicitEmptyCollections encoding is disabled") {
          type Basket = (owner: String, items: List[Int])
          implicit val config: JsonCodecConfiguration =
            JsonCodecConfiguration(explicitEmptyCollections = ExplicitEmptyCollections(encoding = false))
          val empty: Basket    = (owner = "me", items = Nil)
          val nonEmpty: Basket = (owner = "me", items = List(1))
          assertTrue(
            empty.toJson == """{"owner":"me"}""",
            empty.toJsonAST == Right(Json.Obj("owner" -> Json.Str("me"))),
            nonEmpty.toJson == """{"owner":"me","items":[1]}"""
          )
        },
        test("escapes field names") {
          type Odd = (`first name`: String, `say "hi"`: Int)
          val odd: Odd = (`first name` = "John", `say "hi"` = 1)
          assertTrue(
            odd.toJson == """{"first name":"John","say \"hi\"":1}""",
            odd.toJsonAST == Right(Json.Obj("first name" -> Json.Str("John"), "say \"hi\"" -> Json.Num(1)))
          )
        },
        test("encodes named tuples with more than 22 fields") {
          type Wide = (
            a: Int,
            b: Int,
            c: Int,
            d: Int,
            e: Int,
            f: Int,
            g: Int,
            h: Int,
            i: Int,
            j: Int,
            k: Int,
            l: Int,
            m: Int,
            n: Int,
            o: Int,
            p: Int,
            q: Int,
            r: Int,
            s: Int,
            t: Int,
            u: Int,
            v: Int,
            w: Int
          )
          val wide: Wide = (
            a = 1,
            b = 2,
            c = 3,
            d = 4,
            e = 5,
            f = 6,
            g = 7,
            h = 8,
            i = 9,
            j = 10,
            k = 11,
            l = 12,
            m = 13,
            n = 14,
            o = 15,
            p = 16,
            q = 17,
            r = 18,
            s = 19,
            t = 20,
            u = 21,
            v = 22,
            w = 23
          )
          val fields = ('a' to 'w').zipWithIndex.map((name, i) => name.toString -> (i + 1))
          assertTrue(
            wide.toJson == fields.map((name, n) => s""""$name":$n""").mkString("{", ",", "}"),
            wide.toJsonAST == Right(Json.Obj(fields.map((name, n) => name -> Json.Num(n)): _*)),
            wide.toJson.fromJson[Wide] == Right(wide)
          )
        },
        test("round trips through the decoder") {
          type Person = (name: String, age: Option[Int], tags: List[String], address: (city: String, zip: Int))
          val people: List[Person] = List(
            (name = "John", age = Some(30), tags = List("a", "b"), address = (city = "Paris", zip = 75001)),
            (name = "Jane", age = None, tags = Nil, address = (city = "Rome", zip = 100))
          )
          assertTrue(
            people.forall(p => p.toJson.fromJson[Person] == Right(p)),
            people.forall(p => p.toJsonPretty.fromJson[Person] == Right(p)),
            people.forall(p => p.toJsonAST.flatMap(_.as[Person]) == Right(p))
          )
        },
        test("matches the encoding of an equivalent case class") {
          type Person = (firstName: String, middleName: Option[String], tags: List[String], age: Int)
          final case class PersonCC(firstName: String, middleName: Option[String], tags: List[String], age: Int)

          def compare(implicit config: JsonCodecConfiguration): TestResult = {
            val ntEncoder: JsonEncoder[Person]   = JsonEncoder[Person]
            val ccEncoder: JsonEncoder[PersonCC] = DeriveJsonEncoder.gen[PersonCC]
            val values                           = List(
              PersonCC("John", None, Nil, 1),
              PersonCC("John", Some("Q"), List("a", "b"), 2)
            )
            values.map { cc =>
              val nt: Person = (firstName = cc.firstName, middleName = cc.middleName, tags = cc.tags, age = cc.age)
              assertTrue(
                ntEncoder.encodeJson(nt, None).toString == ccEncoder.encodeJson(cc, None).toString,
                ntEncoder.encodeJson(nt, Some(0)).toString == ccEncoder.encodeJson(cc, Some(0)).toString,
                ntEncoder.toJsonAST(nt) == ccEncoder.toJsonAST(cc)
              )
            }.reduce(_ && _)
          }

          compare(JsonCodecConfiguration()) &&
          compare(JsonCodecConfiguration(fieldNameMapping = SnakeCase, explicitNulls = true)) &&
          compare(
            JsonCodecConfiguration(
              fieldNameMapping = KebabCase,
              explicitEmptyCollections = ExplicitEmptyCollections(encoding = false)
            )
          ) &&
          compare(
            JsonCodecConfiguration(
              explicitNulls = true,
              explicitEmptyCollections = ExplicitEmptyCollections(encoding = false)
            )
          )
        }
      )
    )
}
