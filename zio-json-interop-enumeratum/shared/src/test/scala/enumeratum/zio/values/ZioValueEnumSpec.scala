package enumeratum.zio.values

import enumeratum.values._
import zio.Trace
import zio.internal.stacktracer.SourceLocation
import zio.json.{ EncoderOps, JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }
import zio.test._
import zio.test.Assertion._

object ZioValueEnumSpec extends ZIOSpecDefault {

  def spec = suite("ZioValueEnumSpec")(
    testZIOEnum("LongZIOEnum", ZIOContentType),
    testZIOEnum("ShortZIOEnum", ZIODrinks),
    testZIOEnum("IntZIOEnum", ZIOLibraryItem),
    testZIOEnum("StringZIOEnum", ZIOOperatingSystem),
    testZIOKeyEnum("StringZIOEnum", ZIOOperatingSystem),
    testZIOEnum("CharEnum", ZIOAlphabet),
    testZIOEnum("ByteEnum", ZIOBites),
    testZIOEnum("IntZIOEnum with val value members", ZIOMovieGenre)
  )
  // Test method that generates tests for most primitve-based ValueEnums when given a simple descriptor and the enum
  private def testZIOEnum[ValueType: JsonDecoder: JsonEncoder, EntryType <: ValueEnumEntry[
    ValueType
  ]: JsonEncoder: JsonDecoder](
    enumKind: String,
    enum: ValueEnum[ValueType, EntryType] with ZIOValueEnum[ValueType, EntryType]
  )(implicit
    testConstructor: TestConstructor[Nothing, _],
    sourceLocation: SourceLocation,
    trace: Trace
  ) =
    suite(enumKind)(
      test(s"$enumKind to JSON should work") {
        enum.values.map { entry =>
          assert(entry.toJsonAST)(equalTo(entry.value.toJsonAST))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }
      },
      test(s"$enumKind parse to members when given proper JSON") {
        enum.values.map { entry =>
          assert(entry.toJsonAST.flatMap(_.as[EntryType]))(isRight(equalTo(entry)))
        }.foldLeft(assertCompletes) { case (a, b) => a && b }

      }
    )
  //      describe("from Json") {
//
//
//
//        it("should fail to parse random JSON to members") {
//          val failures =
//            Seq(Json.fromString("GOBBLYGOOKITY"), Json.fromInt(Int.MaxValue)).map(_.as[EntryType])
//          failures.foreach { f =>
//            f.isLeft shouldBe true
//            f.leftMap(_.history shouldBe Nil)
//          }
//        }
//
//      }

//    }

  private def testZIOKeyEnum[EntryType <: ValueEnumEntry[String]: JsonFieldEncoder: JsonFieldDecoder](
    enumKind: String,
    enum: ValueEnum[String, EntryType] with ZIOValueEnum[String, EntryType]
  )(implicit
    testConstructor: TestConstructor[Nothing, _],
    sourceLocation: SourceLocation,
    trace: Trace
  ) =
    suite(s"$enumKind as Key")(
      test("to JSON should work") {
        val map = enum.values.toStream.zip(Stream.from(1)).toMap
        assert(map.toJsonAST.flatMap(_.as[Map[EntryType, Int]]))(isRight(equalTo(map)))
      },
      test("from JSON should fail to parse random JSON into a map") {
        val invalidJsonMap =
          Stream
            .from(1)
            .map(_.toString)
            .take(10)
            .toStream
            .zip(Stream.from(1))
            .toMap
            .toJsonAST
            .flatMap(_.as[Map[EntryType, Int]])
        assertTrue(invalidJsonMap.isLeft)
      }
    )

}

sealed abstract class ZIOContentType(val value: Long, name: String) extends LongEnumEntry

case object ZIOContentType extends LongEnum[ZIOContentType] with LongZIOEnum[ZIOContentType] {

  val values = findValues

  case object Text  extends ZIOContentType(value = 1L, name = "text")
  case object Image extends ZIOContentType(value = 2L, name = "image")
  case object Video extends ZIOContentType(value = 3L, name = "video")
  case object Audio extends ZIOContentType(value = 4L, name = "audio")

}

sealed abstract class ZIODrinks(val value: Short, name: String) extends ShortEnumEntry

case object ZIODrinks extends ShortEnum[ZIODrinks] with ShortZIOEnum[ZIODrinks] {

  case object OrangeJuice extends ZIODrinks(value = 1, name = "oj")
  case object AppleJuice  extends ZIODrinks(value = 2, name = "aj")
  case object Cola        extends ZIODrinks(value = 3, name = "cola")
  case object Beer        extends ZIODrinks(value = 4, name = "beer")

  val values = findValues

}

sealed abstract class ZIOLibraryItem(val value: Int, val name: String) extends IntEnumEntry

case object ZIOLibraryItem extends IntEnum[ZIOLibraryItem] with IntZIOEnum[ZIOLibraryItem] {

  // A good mix of named, unnamed, named + unordered args
  case object Book     extends ZIOLibraryItem(value = 1, name = "book")
  case object Movie    extends ZIOLibraryItem(name = "movie", value = 2)
  case object Magazine extends ZIOLibraryItem(3, "magazine")
  case object CD       extends ZIOLibraryItem(4, name = "cd")

  val values = findValues

}

sealed abstract class ZIOOperatingSystem(val value: String) extends StringEnumEntry

case object ZIOOperatingSystem extends StringEnum[ZIOOperatingSystem] with StringZIOEnum[ZIOOperatingSystem] {

  case object Linux   extends ZIOOperatingSystem("linux")
  case object OSX     extends ZIOOperatingSystem("osx")
  case object Windows extends ZIOOperatingSystem("windows")
  case object Android extends ZIOOperatingSystem("android")

  val values = findValues

}

sealed abstract class ZIOMovieGenre extends IntEnumEntry

case object ZIOMovieGenre extends IntEnum[ZIOMovieGenre] with IntZIOEnum[ZIOMovieGenre] {

  case object Action extends ZIOMovieGenre {
    val value = 1
  }
  case object Comedy extends ZIOMovieGenre {
    val value: Int = 2
  }
  case object Romance extends ZIOMovieGenre {
    val value = 3
  }

  val values = findValues

}

sealed abstract class ZIOAlphabet(val value: Char) extends CharEnumEntry

case object ZIOAlphabet extends CharEnum[ZIOAlphabet] with CharZIOEnum[ZIOAlphabet] {

  case object A extends ZIOAlphabet('A')
  case object B extends ZIOAlphabet('B')
  case object C extends ZIOAlphabet('C')
  case object D extends ZIOAlphabet('D')

  val values = findValues

}

sealed abstract class ZIOBites(val value: Byte) extends ByteEnumEntry

object ZIOBites extends ByteEnum[ZIOBites] with ByteZIOEnum[ZIOBites] {
  val values = findValues

  case object OneByte   extends ZIOBites(1)
  case object TwoByte   extends ZIOBites(2)
  case object ThreeByte extends ZIOBites(3)
  case object FourByte  extends ZIOBites(4)
}
