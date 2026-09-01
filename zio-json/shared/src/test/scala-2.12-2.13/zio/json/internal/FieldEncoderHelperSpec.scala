package zio.json
package internal

import zio.test._

object FieldEncoderSpec extends ZIOSpecDefault {
  val spec = suite("FieldEncoder")(
    suite("encodeOrDefault")(
      suite("OptionEncoder")(
        test("should use the default encoding None when withExplicitNulls is false") {
          val helper = FieldEncoder(
            1,
            "test",
            JsonEncoder.option(JsonEncoder.int),
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          assertTrue(helper.skip(None))
        },
        test("should encode None when withExplicitNulls is true") {
          val helper = FieldEncoder(
            1,
            "test",
            JsonEncoder.option(JsonEncoder.int),
            withExplicitNulls = true,
            withExplicitEmptyCollections = false
          )
          assertTrue(!helper.skip(None))
        }
      ),
      suite("CollectionEncoder")(
        test("should encode empty collections when withExplicitEmptyCollections is true") {
          val helper = FieldEncoder(
            1,
            "test",
            implicitly[JsonEncoder[List[Int]]],
            withExplicitNulls = false,
            withExplicitEmptyCollections = true
          )
          assertTrue(!helper.skip(Nil))
        },
        test("should not encode empty collections when withExplicitEmptyCollections is false") {
          val helper = FieldEncoder(
            1,
            "test",
            implicitly[JsonEncoder[List[Int]]],
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          assertTrue(helper.skip(Nil))
        }
      ),
      suite("for a case class")(
        test("should encode case classes with empty collections when withExplicitEmptyCollections is true") {
          case class Test(list: List[Int], option: Option[Int])
          val helper = FieldEncoder(
            1,
            "test",
            DeriveJsonEncoder.gen[Test],
            withExplicitNulls = false,
            withExplicitEmptyCollections = true
          )
          assertTrue(!helper.skip(Test(Nil, None)))
        },
        test("should encode case classes with empty collections when withExplicitEmptyCollections is false") {
          case class Test(list: List[Int], option: Option[Int])
          val helper = FieldEncoder(
            1,
            "test",
            DeriveJsonEncoder.gen[Test],
            withExplicitNulls = false,
            withExplicitEmptyCollections = false
          )
          assertTrue(!helper.skip(Test(Nil, None)))
        },
        test(
          "should encode case classes with empty options when withExplicitEmptyCollections is false, even when withExplicitNulls is true"
        ) {
          case class Test(list: List[Int], option: Option[Int])
          val helper = FieldEncoder(
            1,
            "test",
            DeriveJsonEncoder.gen[Test],
            withExplicitNulls = true,
            withExplicitEmptyCollections = false
          )
          assertTrue(!helper.skip(Test(Nil, None)))
        }
      )
    )
  )
}
