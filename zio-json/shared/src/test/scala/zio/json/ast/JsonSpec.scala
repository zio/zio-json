package zio.json.ast

import zio.json._
import zio.test.Assertion._
import zio.test._

object JsonSpec extends ZIOSpecDefault {

  val spec: Spec[Environment, Any] =
    suite("Json")(
      suite("delete")(
        suite("scalar")(
          test("success") {
            assert(Json.Str("str").delete(JsonCursor.Identity.isString))(isRight(equalTo(Json.Null)))
          },
          test("failure") {
            assert(Json.Str("str").delete(JsonCursor.Identity.isNumber))(isLeft)
          }
        ),
        suite("nested")(
          test("success") {
            val downHashTags = JsonCursor.field("entities").isObject.field("hashtags").isArray

            val firstRoleCursor = downHashTags.element(0).filterType(JsonType.Str)
            val userIdCursor    = JsonCursor.field("user").isObject.field("id").filterType(JsonType.Num)

            assert(tweet.delete(firstRoleCursor))(
              isRight(
                equalTo(
                  Json.Obj(
                    "id" -> Json.Num(8500),
                    "user" -> Json.Obj(
                      "id"   -> Json.Num(6200),
                      "name" -> Json.Str("Twitter API")
                    ),
                    "entities" -> Json.Obj(
                      "hashtags" -> Json.Arr(
                        Json.Str("developer")
                      )
                    )
                  )
                )
              )
            ) &&
            assert(tweet.delete(userIdCursor))(
              isRight(
                equalTo(
                  Json.Obj(
                    "id" -> Json.Num(8500),
                    "user" -> Json.Obj(
                      "name" -> Json.Str("Twitter API")
                    ),
                    "entities" -> Json.Obj(
                      "hashtags" -> Json.Arr(
                        Json.Str("twitter"),
                        Json.Str("developer")
                      )
                    )
                  )
                )
              )
            )
          },
          test("failure") {
            val missingElement = JsonCursor.field("entities").isObject.field("hashtags").isArray.element(2)
            assert(tweet.delete(missingElement))(isLeft)
          }
        )
      ),
      suite("equals")(
        test("mismatched Json subtypes") {
          val nul: Json  = Json.Null
          val num: Json  = Json.Num(1)
          val str: Json  = Json.Str("hello")
          val bool: Json = Json.Bool(true)
          val arr: Json  = Json.Arr(nul, num, str)
          val obj: Json = Json.Obj(
            "nul"  -> nul,
            "num"  -> num,
            "str"  -> str,
            "bool" -> bool,
            "arr"  -> arr,
            "obj"  -> Json.Obj("more" -> str, "andMore" -> bool)
          )

          assertTrue(List(nul, num, str, bool, arr, obj).combinations(2).forall {
            case fst :: snd :: Nil => fst != snd
            case _                 => false
          })
        },
        test("object order does not matter for equality") {
          val obj1 = Json.Obj(
            "foo" -> Json.Str("1"),
            "bar" -> Json.Str("2"),
            "baz" -> Json.Arr(Json.Bool(true), Json.Num(2))
          )

          val obj2 = Json.Obj(
            "baz" -> Json.Arr(Json.Bool(true), Json.Num(2)),
            "bar" -> Json.Str("2"),
            "foo" -> Json.Str("1")
          )

          assertTrue(obj1 == obj2 && obj2 == obj1)
        },
        test("equality fails for different objects of the same size with different keys") {
          val obj1 = Json.Obj(
            "quux" -> Json.Str("1"),
            "bar"  -> Json.Str("2")
          )

          val obj2 = Json.Obj(
            "baz" -> Json.Arr(Json.Bool(true), Json.Num(2)),
            "bar" -> Json.Str("2")
          )

          assertTrue(obj1 != obj2 && obj2 != obj1)
        },
        test("equality fails for different objects of different sizes") {
          val obj1 = Json.Obj(
            "quux" -> Json.Str("1"),
            "bar"  -> Json.Str("2")
          )

          val obj2 = Json.Obj(
            "quux" -> Json.Str("1"),
            "bar"  -> Json.Str("2"),
            "maux" -> Json.Str("3")
          )

          assertTrue(obj1 != obj2 && obj2 != obj1)
        }
      ),
      suite("hashCode")(
        test("objects with the same elements regardless of order have the same hashCode") {
          val obj1 = Json.Obj(
            "foo" -> Json.Str("1"),
            "bar" -> Json.Str("2"),
            "baz" -> Json.Arr(Json.Bool(true), Json.Num(2))
          )

          val obj2 = Json.Obj(
            "baz" -> Json.Arr(Json.Bool(true), Json.Num(2)),
            "bar" -> Json.Str("2"),
            "foo" -> Json.Str("1")
          )

          assertTrue(obj1.hashCode == obj2.hashCode)
        }
      ),
      suite("foldUp")(
        test("folds the structure bottom-up (starting at the leaves)") {
          val obj =
            Json.Obj(
              "one" -> Json.Obj(
                "three" -> Json.Obj(
                  "five" -> Json.Obj(
                    "seven" -> Json.Arr(Json.Str("eight")),
                    "six"   -> Json.Num(6)
                  ),
                  "four" -> Json.Null
                ),
                "two" -> Json.Bool(true)
              )
            )
          val result = obj.foldUp(Vector.empty[String])(collectObjKeysAndArrElements)
          assertTrue(result == Vector("eight", "seven", "six", "five", "four", "three", "two", "one"))
        }
      ),
      suite("foldDown")(
        test("folds the structure top-down (starting at the root)") {
          val obj =
            Json.Obj(
              "one" -> Json.Obj(
                "two" -> Json.Bool(true),
                "three" -> Json.Obj(
                  "four" -> Json.Null,
                  "five" -> Json.Obj(
                    "six"   -> Json.Num(6),
                    "seven" -> Json.Arr(Json.Str("eight"))
                  )
                )
              )
            )
          val result = obj.foldDown(Vector.empty[String])(collectObjKeysAndArrElements)
          assertTrue(result == Vector("one", "two", "three", "four", "five", "six", "seven", "eight"))
        }
      ),
      test("arrays with the same elements in a different order will not have the same hashCode") {
        val arr1 = Json.Arr(Json.Str("one"), Json.Obj("two" -> Json.Num(2)), Json.Num(3))
        val arr2 = Json.Arr(Json.Num(3), Json.Str("one"), Json.Obj("two" -> Json.Num(2)))
        assertTrue(arr1.hashCode != arr2.hashCode)
      },
      test("arrays with the same elements in the same order will have the same hashCode") {
        val arr1 = Json.Arr(Json.Str("one"), Json.Obj("two" -> Json.Num(2)), Json.Num(3))
        val arr2 = Json.Arr(Json.Str("one"), Json.Obj("two" -> Json.Num(2)), Json.Num(3))
        assertTrue(arr1.hashCode == arr2.hashCode)
      },
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
        test(">>>, object") {
          val downUser       = JsonCursor.field("user")
          val downId         = JsonCursor.field("id")
          val downUserDownId = downUser.isObject >>> downId

          assert(tweet.get(downUserDownId))(
            isRight(equalTo(Json.Num(6200)))
          )
        },
        test(">>>, array, filterType") {
          val downHashtags = JsonCursor.field("entities").isObject.field("hashtags")
          val asArray      = JsonCursor.filter(JsonType.Arr)
          val downFirst    = JsonCursor.element(0)

          val combined = downHashtags >>> asArray >>> downFirst

          assert(tweet.get(combined))(
            isRight(equalTo(Json.Str("twitter")))
          )
        },
        test(">>>, array, filterType (second operand of >>> is complex)") {
          val downEntities = JsonCursor.field("entities")
          val downHashtag =
            JsonCursor.isObject >>> JsonCursor.field("hashtags") >>> JsonCursor.isArray >>> JsonCursor.element(0)

          val combined = downEntities >>> downHashtag

          assert(tweet.get(combined))(
            isRight(equalTo(Json.Str("twitter")))
          )
        },
        test(">>>, combination of some methods of JsonCursor (second operand of >>> is complex)") {
          val posts: Json = """{"posts": [{"id": 0, "title": "foo"}]}""".fromJson[Json].toOption.get

          val downPosts = JsonCursor.field("posts")
          val downTitle = JsonCursor.isArray >>> JsonCursor.element(0) >>> JsonCursor.isObject >>>
            JsonCursor.field("title") >>> JsonCursor.isString
          val combined = downPosts >>> downTitle

          assert(posts.get(combined))(
            isRight(equalTo(Json.Str("foo")))
          )
        },
        test(">>>, identity") {
          val obj = Json.Obj("a" -> Json.Num(1))

          val fieldA   = JsonCursor.field("a")
          val identity = JsonCursor.identity

          val num = obj.get(fieldA >>> identity)

          assert(num)(isRight(equalTo(Json.Num(1))))
        }
      ),
      suite("intersect")(
        test("object + object") {
          val left = Json.Obj(
            "a" -> Json.Num(1),
            "b" -> Json.Num(2)
          )

          val right = Json.Obj(
            "a" -> Json.Num(2),
            "b" -> Json.Num(2),
            "c" -> Json.Num(4)
          )

          assert(left.intersect(right))(
            isRight(
              equalTo(
                Json.Obj(
                  "b" -> Json.Num(2)
                )
              )
            )
          )
        },
        test("object, deep") {
          val intersected = tweet.intersect(
            Json.Obj(
              "id" -> Json.Num(8501),
              "user" -> Json.Obj(
                "id"   -> Json.Num(6200),
                "name" -> Json.Str("Twitter API")
              )
            )
          )

          assert(intersected)(
            isRight(
              equalTo(
                Json.Obj(
                  "user" -> Json.Obj(
                    "id"   -> Json.Num(6200),
                    "name" -> Json.Str("Twitter API")
                  )
                )
              )
            )
          )
        },
        test("array") {
          val left = Json.Arr(
            Json.Obj("id"            -> Json.Num(1), "authenticated" -> Json.Bool(true)),
            Json.Obj("authenticated" -> Json.Bool(true)),
            Json.Obj("id"            -> Json.Num(1), "authenticated" -> Json.Bool(true))
          )

          val right = Json.Arr(
            Json.Obj("id"            -> Json.Num(1), "authenticated" -> Json.Bool(false)),
            Json.Obj("authenticated" -> Json.Bool(true))
          )

          val intersected = left.intersect(right)

          assert(intersected)(
            isRight(
              equalTo(
                Json.Arr(
                  Json.Obj(
                    "authenticated" -> Json.Bool.True
                  )
                )
              )
            )
          )
        },
        test("array - duplicates") {
          val left = Json.Arr(
            Json.Str("a"),
            Json.Str("a"),
            Json.Str("b"),
            Json.Str("b"),
            Json.Str("b"),
            Json.Str("c")
          )

          val right = Json.Arr(
            Json.Str("a"),
            Json.Str("b"),
            Json.Str("b"),
            Json.Str("b"),
            Json.Str("c"),
            Json.Str("c")
          )

          val intersected = left.intersect(right)

          assert(intersected)(
            isRight(
              equalTo(
                Json.Arr(
                  Json.Str("a"),
                  Json.Str("b"),
                  Json.Str("b"),
                  Json.Str("b"),
                  Json.Str("c")
                )
              )
            )
          )
        },
        test("scalar") {
          assert(Json.Null.intersect(Json.Bool.True))(isLeft(equalTo("Non compatible types"))) && assert(
            Json.Num(1).intersect(Json.Arr(Json.Num(1)))
          )(isLeft(equalTo("Non compatible types"))) && assert(Json.Str("1").intersect(Json.Arr(Json.Str("1"))))(
            isLeft(equalTo("Non compatible types"))
          ) && assert(Json.Num(1).intersect(Json.Arr(Json.Num(1))))(isLeft(equalTo("Non compatible types")))
        }
      ),
      suite("merge")(
        test("object + object") {
          val left = Json.Obj(
            "a" -> Json.Num(1),
            "b" -> Json.Num(2)
          )

          val right = Json.Obj(
            "b" -> Json.Num(3),
            "c" -> Json.Num(4)
          )

          assert(left.merge(right))(
            equalTo(
              Json.Obj(
                "a" -> Json.Num(1),
                "b" -> Json.Num(3),
                "c" -> Json.Num(4)
              )
            )
          )
        },
        test("object, deep") {
          val merged = tweet.merge(
            Json.Obj(
              "user" -> Json.Obj(
                "private" -> Json.Bool.False
              )
            )
          )

          assert(merged)(
            equalTo(
              Json.Obj(
                "id" -> Json.Num(8500),
                "user" -> Json.Obj(
                  "id"      -> Json.Num(6200),
                  "name"    -> Json.Str("Twitter API"),
                  "private" -> Json.Bool.False
                ),
                "entities" -> Json.Obj(
                  "hashtags" -> Json.Arr(
                    Json.Str("twitter"),
                    Json.Str("developer")
                  )
                )
              )
            )
          )
        },
        test("array") {
          val left = Json.Arr(
            Json.Obj("id" -> Json.Num(1))
          )

          val right = Json.Arr(
            Json.Obj("authenticated" -> Json.Bool(true))
          )

          val merged = left.merge(right)

          assert(merged)(
            equalTo(
              Json.Arr(
                Json.Obj(
                  "id"            -> Json.Num(1),
                  "authenticated" -> Json.Bool.True
                )
              )
            )
          )
        },
        test("scalar") {
          assert(Json.Null.merge(Json.Bool.True))(
            equalTo(
              Json.Bool.True
            )
          )
        }
      ),
      suite("relocate") {
        suite("nested")(
          test("success") {
            val userCursor     = JsonCursor.field("user").isObject
            val entitiesCursor = JsonCursor.field("entities")

            assert(tweet.relocate(userCursor, entitiesCursor))(
              isRight(
                equalTo(
                  Json.Obj(
                    "id" -> Json.Num(8500),
                    "entities" -> Json.Obj(
                      "id"   -> Json.Num(6200),
                      "name" -> Json.Str("Twitter API")
                    )
                  )
                )
              )
            )
          },
          test("failure - from") {
            val fromCursor = JsonCursor.field("user").isArray
            val toCursor   = JsonCursor.field("entities")

            assert(tweet.relocate(fromCursor, toCursor))(isLeft)
          },
          test("failure - to") {
            val fromCursor = JsonCursor.field("user").isObject
            val toCursor   = JsonCursor.field("entities").isBool

            assert(tweet.relocate(fromCursor, toCursor))(isLeft)
          }
        )
      },
      suite("transformAt")(
        suite("scalar")(
          test("success") {
            assert(Json.Str("str").transformAt(JsonCursor.Identity.isString) { str =>
              Json.Str(str.value + ("1"))
            })(isRight(equalTo(Json.Str("str1"))))
          },
          test("failure") {
            assert(Json.Str("str").transformAt(JsonCursor.Identity.isNumber)(identity))(isLeft)
          }
        ),
        suite("nested")(
          test("success") {
            val downHashTags = JsonCursor.field("entities").isObject.field("hashtags").isArray

            val firstRoleCursor = downHashTags.element(0).filterType(JsonType.Str)
            val userIdCursor    = JsonCursor.field("user").isObject.field("id").filterType(JsonType.Num)

            assert(tweet.transformAt(firstRoleCursor)(jstr => Json.Str(jstr.value.capitalize)))(
              isRight(
                equalTo(
                  Json.Obj(
                    "id" -> Json.Num(8500),
                    "user" -> Json.Obj(
                      "id"   -> Json.Num(6200),
                      "name" -> Json.Str("Twitter API")
                    ),
                    "entities" -> Json.Obj(
                      "hashtags" -> Json.Arr(
                        Json.Str("Twitter"),
                        Json.Str("developer")
                      )
                    )
                  )
                )
              )
            ) &&
            assert(tweet.transformAt(userIdCursor)(json => Json.Num(json.value.intValue() + 1)))(
              isRight(
                equalTo(
                  Json.Obj(
                    "id" -> Json.Num(8500),
                    "user" -> Json.Obj(
                      "id"   -> Json.Num(6201),
                      "name" -> Json.Str("Twitter API")
                    ),
                    "entities" -> Json.Obj(
                      "hashtags" -> Json.Arr(
                        Json.Str("twitter"),
                        Json.Str("developer")
                      )
                    )
                  )
                )
              )
            )
          },
          test("failure") {
            val missingElement = JsonCursor.field("entities").isObject.field("hashtags").isArray.element(2)
            assert(tweet.transformAt(missingElement)(identity))(isLeft)
          }
        )
      )
    )

  lazy val tweet: Json.Obj =
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

  def collectObjKeysAndArrElements(acc: Vector[String], next: Json): Vector[String] =
    next match {
      case Json.Obj(fields)   => acc ++ fields.map(_._1)
      case Json.Arr(elements) => acc ++ elements.collect { case Json.Str(s) => s }.toVector
      case Json.Bool(_)       => acc
      case Json.Str(_)        => acc
      case Json.Num(_)        => acc
      case Json.Null          => acc
    }
}
