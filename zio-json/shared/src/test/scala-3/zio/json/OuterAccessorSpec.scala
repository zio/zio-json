package zio.json

import zio.json._
import zio.test._

object OuterTypes:
  case class Type1(s: String) derives JsonCodec
  case class Type2(i: Int) derives JsonCodec
  case class CaseClassToJsonDerive(unionType: Type1 | Type2)

object OuterAccessorSpec extends ZIOSpecDefault {

  val spec = suite("OuterAccessor")(
    test("derives codec for locally-defined enum wrapping union type") {
      given JsonCodec[OuterTypes.CaseClassToJsonDerive] = {
        enum EnumInsteadOfUnionType:
          case T1(t: OuterTypes.Type1)
          case T2(t: OuterTypes.Type2)

        given JsonCodec[OuterTypes.Type1 | OuterTypes.Type2] = {
          given JsonCodec[EnumInsteadOfUnionType.T1] =
            summon[JsonCodec[OuterTypes.Type1]].transform(EnumInsteadOfUnionType.T1.apply, _.t)

          given JsonCodec[EnumInsteadOfUnionType.T2] =
            summon[JsonCodec[OuterTypes.Type2]].transform(EnumInsteadOfUnionType.T2.apply, _.t)

          DeriveJsonCodec
            .gen[EnumInsteadOfUnionType]
            .transform(
              {
                case EnumInsteadOfUnionType.T1(t) => t
                case EnumInsteadOfUnionType.T2(t) => t
              },
              {
                case t: OuterTypes.Type1 => EnumInsteadOfUnionType.T1(t)
                case t: OuterTypes.Type2 => EnumInsteadOfUnionType.T2(t)
              }
            )
        }

        DeriveJsonCodec.gen[OuterTypes.CaseClassToJsonDerive]
      }
      val json = """{"unionType":{"T1":{"s":"hello"}}}"""
      assertTrue(json.fromJson[OuterTypes.CaseClassToJsonDerive].isRight)
    }
  )
}
