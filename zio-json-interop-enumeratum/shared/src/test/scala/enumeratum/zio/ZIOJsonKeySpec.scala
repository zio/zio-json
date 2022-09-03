package enumeratum.zio
import zio.Chunk
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.Assertion._
import zio.test._

object ZIOJsonKeySpec extends ZIOSpecDefault {
  def spec =
    suite("ZIOJsonKeySpec")(
      test("JsonEncoder should work")(
        assert(Map(ShirtSize.Small -> 5, ShirtSize.Large -> 10).toJsonAST)(
          isRight(equalTo(Json.Obj(Chunk("Small" -> Json.Num(5), "Large" -> Json.Num(10)))))
        )
      ),
      test("JsonDecoder should work")(
        assert(Json.Obj(Chunk("Medium" -> Json.Num(100), "Large" -> Json.Num(15))).as[Map[ShirtSize, Int]])(
          isRight(equalTo(Map(ShirtSize.Medium -> 100, ShirtSize.Large -> 15)))
        )
      )
    )

}
