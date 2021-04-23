package zio.json.ast

import zio.test.Assertion._
import zio.test._

object JsonSpec extends DefaultRunnableSpec {
  val spec: ZSpec[Environment, Failure] =
    suite("Json")(
      suite("get")(
        test("downField") {
          val obj = Json.Obj("a" -> Json.Num(1), "b" -> Json.Num(2), "c" -> Json.Null)

          assert(obj.get(JsonCursor.field("a")))(isRight(equalTo(Json.Num(1)))) &&
          assert(obj.get(JsonCursor.field("b")))(isRight(equalTo(Json.Num(2)))) &&
          assert(obj.get(JsonCursor.field("c")))(isRight(equalTo(Json.Null))) &&
          assert(obj.get(JsonCursor.field("d")))(isLeft(equalTo("No such field: 'd'")))
        },
        test("downElement") {
          val downHashTags = JsonCursor.field("entities").isObject.field("hashtags").isArray

          val fstHashTag = downHashTags.element(0)
          val sndHashTag = downHashTags.element(1)
          val trdHashTag = downHashTags.element(2)

          assert(tweet.get(fstHashTag))(isRight(equalTo(Json.Str("twitter")))) &&
          assert(tweet.get(sndHashTag))(isRight(equalTo(Json.Str("developer")))) &&
          assert(tweet.get(trdHashTag))(isLeft)
        },
        test("filterType") {
          val filterBool = JsonCursor.filter(JsonType.Bool)
          val filterNum  = JsonCursor.filter(JsonType.Bool)
          val filterStr  = JsonCursor.filter(JsonType.Str)

          assert(Json.Str("test").get(filterStr))(isRight) &&
          assert(Json.Str("test").get(filterBool))(isLeft) &&
          assert(Json.Str("test").get(filterNum))(isLeft)
        },
        test(">>>") {
          val downUser       = JsonCursor.field("user")
          val downId         = JsonCursor.field("id")
          val downUserDownId = downUser.isObject >>> downId

          assert(tweet.get(downUserDownId))(
            isRight(equalTo(Json.Num(6200)))
          )
        }
      )
    )

  val tweet: Json.Obj =
    Json.Obj(
      "id" -> Json.Num(8500),
      "user" -> Json.Obj(
        "id"   -> Json.Num(6200),
        "name" -> Json.Str("Twitter API")
      ),
      "entities" -> Json.Obj(
        "hashtags" -> Json.Arr(
          Json.Str("twitter"),
          Json.Str("developer")
        )
      )
    )
}
