package zio.json.golden

import zio._
import zio.json._
import zio.test.TestAspect.exceptScala212
import zio.test._
import zio.test.magnolia.DeriveGen

object GoldenSpec extends ZIOSpecDefault {

  sealed trait SumType

  object SumType {
    case object Case1  extends SumType
    case object Case2  extends SumType
    case class Case3() extends SumType

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

  final case class FilteredGenType(a: java.math.BigDecimal)
  object FilteredGenType {
    implicit val jsonCodec: JsonCodec[FilteredGenType] = DeriveJsonCodec.gen

    val anyFilteredGenType: Gen[Any, FilteredGenType] = {

      /**
       * Copied from zio-json/shared/src/test/scala/zio/json/Gens.scala
       */
      val genBigDecimal: Gen[Any, java.math.BigDecimal] =
        Gen
          .bigDecimal((BigDecimal(2).pow(128) - 1) * -1, BigDecimal(2).pow(128) - 1)
          .map(_.bigDecimal)
          .filter(_.toBigInteger.bitLength < 128)

      genBigDecimal.map(FilteredGenType.apply)
    }
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
    }, {
      implicit val config: GoldenConfiguration =
        GoldenConfiguration.default.copy(relativePath = "filteredgentype", sampleSize = 100)
      goldenTest(FilteredGenType.anyFilteredGenType)
    } @@ exceptScala212 // Quick & Dirty fix. Scala 2.12 generates BigDecimal differently making the test fail for no good reason.
  )

}
