package zio.json.golden

import zio.json._
import zio.json.golden._
import zio.test._
import zio.test.magnolia.DeriveGen
import zio._

object GoldenSpec extends ZIOSpecDefault {

  sealed trait SumType

  object SumType {
    case object Case1 extends SumType
    case object Case2 extends SumType
    case object Case3 extends SumType

    implicit val jsonCodec: JsonCodec[SumType] = DeriveJsonCodec.gen
  }

  case class RecordType(long: Long, string: String, subRecord: RecordType.SubRecord)

  object RecordType {
    case class SubRecord(int: Int)
    object SubRecord {
      implicit val jsonCodec: JsonCodec[SubRecord] = DeriveJsonCodec.gen
    }

    implicit val jsonCodec: JsonCodec[RecordType] = DeriveJsonCodec.gen
  }

  def spec: Spec[TestEnvironment with Scope, Any] = suite("GoldenSpec")(
    goldenTest(DeriveGen[Int]),
    goldenTest(DeriveGen[SumType]),
    goldenTest(DeriveGen[RecordType]), {
      implicit val config: GoldenConfiguration = GoldenConfiguration.default.copy(relativePath = "int")
      goldenTest(DeriveGen[Int])
    }, {
      implicit val config: GoldenConfiguration = GoldenConfiguration.default.copy(relativePath = "sumtype")
      goldenTest(DeriveGen[SumType])
    }, {
      implicit val config: GoldenConfiguration = GoldenConfiguration.default.copy(relativePath = "recordtype")
      goldenTest(DeriveGen[RecordType])
    }
  )

}
