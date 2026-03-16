package enumeratum

import zio.json._
import zio.test._

object ZioJsonKeySpec extends ZIOSpecDefault {

  def spec = suite("ZioJsonKey")(
    suite("to JSON")(
      test("should work") {
        assertTrue(
          Map(ZioJsonShirtSize.Small -> 5, ZioJsonShirtSize.Large -> 10).toJson
            .fromJson[Map[String, Int]] == Right(Map("Small" -> 5, "Large" -> 10))
        )
      }
    ),
    suite("from JSON")(
      test("should work") {
        assertTrue(
          """{"Medium":100,"Large":15}"""
            .fromJson[Map[ZioJsonShirtSize, Int]] == Right(
            Map[ZioJsonShirtSize, Int](
              ZioJsonShirtSize.Medium -> 100,
              ZioJsonShirtSize.Large  -> 15
            )
          )
        )
      },
      test("should fail for invalid keys") {
        assertTrue(
          """{"XXL":100}""".fromJson[Map[ZioJsonShirtSize, Int]] ==
            Left(".XXL('XXL' is not a member of enum ZioJsonShirtSize)")
        )
      }
    )
  )

}
