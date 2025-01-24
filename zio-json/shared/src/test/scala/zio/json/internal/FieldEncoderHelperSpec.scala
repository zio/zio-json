package zio.json
package internal

import zio.json.ast.Json
import zio.Chunk
import zio.test._

object FieldEncoderSpec extends ZIOSpecDefault {
  val spec = suite("FieldEncoder")(
    suite("encodeOrSkip")(
      suite("OptionEncoder")(
        test("should skip encoding None when withExplicitNulls is false") {
          val helper = new FieldEncoder(
            1,
            "test",
            JsonEncoder.option(JsonEncoder.int),
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          var called = false
          helper.encodeOrSkip(None)(() => called = true)
          assertTrue(!called)
        },
        test("should encode None when withExplicitNulls is true") {
          val helper = new FieldEncoder(
            1,
            "test",
            JsonEncoder.option(JsonEncoder.int),
            withExplicitNulls = true,
            withExplicitEmptyCollections = false
          )
          var called = false
          helper.encodeOrSkip(None)(() => called = true)
          assertTrue(called)
        }
      ),
      suite("CollectionEncoder")(
        suite("for a List")(
          test("should encode empty collections when withExplicitEmptyCollections is true") {
            val helper = new FieldEncoder(
              1,
              "test",
              implicitly[JsonEncoder[List[Int]]],
              withExplicitNulls = false,
              withExplicitEmptyCollections = true
            )
            var called = false
            helper.encodeOrSkip(Nil)(() => called = true)
            assertTrue(called)
          },
          test("should not encode empty collections when withExplicitEmptyCollections is false") {
            val helper = new FieldEncoder(
              1,
              "test",
              implicitly[JsonEncoder[List[Int]]],
              withExplicitNulls = false,
              withExplicitEmptyCollections = false
            )
            var called = false
            helper.encodeOrSkip(Nil)(() => called = true)
            assertTrue(!called)
          }
        ),
        suite("for a case class")(
          test("should encode case classes with empty collections when withExplicitEmptyCollections is true") {
            case class Test(list: List[Int], option: Option[Int])
            val helper = new FieldEncoder(
              1,
              "test",
              DeriveJsonEncoder.gen[Test],
              withExplicitNulls = false,
              withExplicitEmptyCollections = true
            )
            var called = false
            helper.encodeOrSkip(Test(Nil, None))(() => called = true)
            assertTrue(called)
          },
          test("should not encode case classes with empty collections when withExplicitEmptyCollections is false") {
            case class Test(list: List[Int], option: Option[Int])
            val helper = new FieldEncoder(
              1,
              "test",
              DeriveJsonEncoder.gen[Test],
              withExplicitNulls = false,
              withExplicitEmptyCollections = false
            )
            var called = false
            helper.encodeOrSkip(Test(Nil, None))(() => called = true)
            assertTrue(!called)
          },
          test(
            "should also not encode case classes with empty options when withExplicitEmptyCollections is false, even when withExplicitNulls is true"
          ) {
            case class Test(list: List[Int], option: Option[Int])
            val helper = new FieldEncoder(
              1,
              "test",
              DeriveJsonEncoder.gen[Test],
              withExplicitNulls = true,
              withExplicitEmptyCollections = false
            )
            var called = false
            helper.encodeOrSkip(Test(Nil, None))(() => called = true)
            assertTrue(!called)
          }
        )
      )
    ),
    suite("encodeOrDefault")(
      suite("OptionEncoder")(
        test("should use the default encoding None when withExplicitNulls is false") {
          val helper = new FieldEncoder(
            1,
            "test",
            JsonEncoder.option(JsonEncoder.int),
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(None)(() => Left(""), Right(expected)) == Right(expected)
          )
        },
        test("should encode None when withExplicitNulls is true") {
          val helper = new FieldEncoder(
            1,
            "test",
            JsonEncoder.option(JsonEncoder.int),
            withExplicitNulls = true,
            withExplicitEmptyCollections = false
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(None)(() => Right(expected), Left("")) == Right(expected)
          )
        }
      ),
      suite("CollectionEncoder")(
        test("should encode empty collections when withExplicitEmptyCollections is true") {
          val helper = new FieldEncoder(
            1,
            "test",
            implicitly[JsonEncoder[List[Int]]],
            withExplicitNulls = false,
            withExplicitEmptyCollections = true
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(Nil)(() => Right(expected), Left("")) == Right(expected)
          )
        },
        test("should not encode empty collections when withExplicitEmptyCollections is false") {
          val helper = new FieldEncoder(
            1,
            "test",
            implicitly[JsonEncoder[List[Int]]],
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(Nil)(() => Left(""), Right(expected)) == Right(expected)
          )
        }
      ),
      suite("for a case class")(
        test("should encode case classes with empty collections when withExplicitEmptyCollections is true") {
          case class Test(list: List[Int], option: Option[Int])
          val helper = new FieldEncoder(
            1,
            "test",
            DeriveJsonEncoder.gen[Test],
            withExplicitNulls = false,
            withExplicitEmptyCollections = true
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(Test(Nil, None))(
              () => Right(expected),
              Left("")
            ) == Right(expected)
          )
        },
        test("should not encode case classes with empty collections when withExplicitEmptyCollections is false") {
          case class Test(list: List[Int], option: Option[Int])
          val helper = new FieldEncoder(
            1,
            "test",
            DeriveJsonEncoder.gen[Test],
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(Test(Nil, None))(() => Left(""), Right(expected)) == Right(expected)
          )
        },
        test(
          "should also not encode case classes with empty options when withExplicitEmptyCollections is false, even when withExplicitNulls is true"
        ) {
          case class Test(list: List[Int], option: Option[Int])
          val helper = new FieldEncoder(
            1,
            "test",
            DeriveJsonEncoder.gen[Test],
            withExplicitNulls = true,
            withExplicitEmptyCollections = false
          )
          val expected = Chunk(("a", Json.Bool.True))
          assertTrue(
            helper.encodeOrDefault(Test(Nil, None))(
              () => Left(""),
              Right(expected)
            ) == Right(expected)
          )
        }
      )
    )
  )
}
