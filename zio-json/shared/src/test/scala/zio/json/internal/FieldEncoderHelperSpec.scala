package zio.json
package internal

import zio.test._

object FieldEncoderSpec extends ZIOSpecDefault {
  val spec = suite("FieldEncoder.encodeOrSkip")(
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
  )
}
