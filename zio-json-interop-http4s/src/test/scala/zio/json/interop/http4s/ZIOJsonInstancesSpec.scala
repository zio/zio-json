package zio.json.interop.http4s

import org.http4s._
import org.typelevel.ci._
import zio.Task
import zio.interop.catz._
import zio.json._
import zio.test.Assertion._
import zio.test._

import java.nio.charset.StandardCharsets

object ZIOJsonInstancesSpec extends ZIOSpecDefault {

  final case class Test(string: String, int: Int)
  private implicit val decoder: JsonCodec[Test] = DeriveJsonCodec.gen[Test]

  val spec = suite("json instances")(
    suite("jsonEncoderOf") {
      test("returns an EntityEncoder that can encode for the given effect and type") {
        check(Gen.string, Gen.int) { (s, i) =>
          val result = jsonEncoderOf[Task, Test]
            .toEntity(Test(s, i))
            .body
            .compile
            .toList
            .map(v => new String(v.toArray, StandardCharsets.UTF_8))

          assertZIO(result)(equalTo(s"""{"string":"$s","int":$i}"""))
        }
      }
    },
    suite("jsonOf")(
      test("returns an EntityDecoder that can decode for the given effect and type")(
        check(Gen.string, Gen.int) { (s, i) =>
          val media = Request[Task]()
            .withEntity(s"""{"string":"$s","int":$i}""")
            .withHeaders(Header.Raw(ci"Content-Type", "application/json"))

          assertZIO(jsonOf[Task, Test].decode(media, true).value)(isRight(equalTo(Test(s, i))))
        }
      ),
      test("returns MalformedMessageBodyFailure when json is empty") {
        val media = Request[Task]()
          .withEntity("")
          .withHeaders(Header.Raw(ci"Content-Type", "application/json"))

        assertZIO(jsonOf[Task, Test].decode(media, true).value)(
          isLeft(equalTo(MalformedMessageBodyFailure("Invalid JSON: empty body")))
        )
      },
      test("returns MalformedMessageBodyFailure when json is invalid") {
        val media = Request[Task]()
          .withEntity("""{"bad" "json"}""")
          .withHeaders(Header.Raw(ci"Content-Type", "application/json"))

        assertZIO(jsonOf[Task, Test].decode(media, true).value)(
          isLeft(equalTo(MalformedMessageBodyFailure("(expected ':' got '\"')")))
        )
      },
      test("returns MalformedMessageBodyFailure when message body is not a json") {
        val media = Request[Task]()
          .withEntity("not a json")
          .withHeaders(Header.Raw(ci"Content-Type", "text/plain"))

        assertZIO(jsonOf[Task, Test].decode(media, true).value)(isLeft(isSubtype[MediaTypeMismatch](anything)))
      }
    )
  )
}
