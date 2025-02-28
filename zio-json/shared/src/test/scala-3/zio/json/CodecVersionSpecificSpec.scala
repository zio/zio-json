package zio.json

import zio.test.Assertion._
import zio.test._

import scala.collection.immutable

object CodecVersionSpecificSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] =
    suite("CodecVersionSpecific")(
      test("ArraySeq") {
        val jsonStr  = """["5XL","2XL","XL"]"""
        val expected = immutable.ArraySeq("5XL", "2XL", "XL")
        assert(jsonStr.fromJson[immutable.ArraySeq[String]])(isRight(equalTo(expected)))
      },
      test("Derives for a product type") {
        assertZIO(typeCheck {
          """
          case class Foo(bar: String) derives JsonCodec

          Foo("bar").toJson.fromJson[Foo]
        """
        })(isRight(anything))
      },
      test("Derives for a sum type") {
        assertZIO(typeCheck {
          """
          enum Foo derives JsonCodec:
            case Bar
            case Baz(baz: String)
            case Qux(foo: Foo)

          (Foo.Qux(Foo.Bar): Foo).toJson.fromJson[Foo]
        """
        })(isRight(anything))
      },
      test("Derives and encodes for a union of string-based literals") {
        case class Foo(aOrB: "A" | "B", optA: Option["A"]) derives JsonCodec

        assertTrue(Foo("A", Some("A")).toJson.fromJson[Foo] == Right(Foo("A", Some("A"))))
      },
      test("Custom codec for union of standard types using an internal API") {
        import zio.json.internal._

        type Value = Null | String | Int | Boolean

        final case class MyDomain(v: Value)

        object MyDomain:
          given JsonCodec[MyDomain] = new JsonCodec[MyDomain](
            (a: MyDomain, indent: Option[Int], out: Write) => a.v match {
              case i: Int => SafeNumbers.write(i, out)
              case b: Boolean => out.write(if (b) "true" else "false")
              case s: String => JsonEncoder.string.unsafeEncode(s, indent, out)
              case null      => out.write("null")
            },
            (trace: List[JsonError], in: RetractReader) => new MyDomain({
              val c = in.nextNonWhitespace()
              if (c == '"') {
                in.retract()
                Lexer.string(trace, in).toString
              } else if (c == 't' && in.readChar() == 'r' && in.readChar() == 'u' && in.readChar() == 'e') {
                true
              } else if (c == 'f' && in.readChar() == 'a' && in.readChar() == 'l' && in.readChar() == 's' && in.readChar() == 'e') {
                false
              } else if (c == 'n' && in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') {
                null
              } else {
                in.retract()
                Lexer.int(trace, in)
              }
            }))

        assertTrue(List(MyDomain("xxx"), MyDomain(777), MyDomain(true), MyDomain(false), MyDomain(null))
          .toJson.fromJson[List[MyDomain]] ==
            Right(List(MyDomain("xxx"), MyDomain(777), MyDomain(true), MyDomain(false), MyDomain(null))))
      }
    )
}
