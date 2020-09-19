package testzio.json

import scala.collection.immutable

import zio.Chunk
import zio.json._
import zio.json.ast._
import scalaprops._
import Property.{ implies, prop, property }
import scala.collection.mutable

// testOnly *RoundtripTest
object RoundtripTest extends Scalaprops {

  def roundtrip[A: JsonEncoder: JsonDecoder](a: A) =
    prop(parser.decode[A](a.toJson) == Right(a)) and
      prop(parser.decode[A](a.toJsonPretty) == Right(a))

  // arbitrary strings are not guaranteed to roundtrip due to normalisation of
  // some unicode characters, but we could still test this on a subset of
  // strings if we wanted to create the Gens

  val booleans = property { i: Boolean => roundtrip(i) }

  val bytes   = property { i: Byte => roundtrip(i) }
  val shorts  = property { i: Short => roundtrip(i) }
  val ints    = property { i: Int => roundtrip(i) }
  val longs   = property { i: Long => roundtrip(i) }
  val bigints = property { i: java.math.BigInteger => implies(i.bitLength < 128, roundtrip(i)) }

  implicit val floatShrinker: Shrink[Float]   = Shrink.empty
  implicit val doubleShrinker: Shrink[Double] = Shrink.empty

  // NaN / Infinity is tested manually, because of == semantics
  val floats  = property { i: Float => implies(i.isFinite, roundtrip(i)) }
  val doubles = property { i: Double => implies(i.isFinite, roundtrip(i)) }

  implicit lazy val astGen: Gen[Json] = Gen.sized { size =>
    val entry: Gen[(String, Json)] = Gen.delay(Gen.apply2(Gen.asciiString, astGen)((a, b) => (a, b)))
    // objects and arrays should get smaller with depth to avoid infinite recursion
    val size_             = 0 min (size - 1)
    val obj: Gen[Json] = Gen.delay(Gen.listOfN(size_, entry)).map(Chunk.fromIterable(_)).map(Json.Obj(_))
    val arr: Gen[Json] = Gen.delay(Gen.listOfN(size_, astGen)).map(Chunk.fromIterable(_)).map(Json.Arr(_))
    val boo: Gen[Json] = Gen[Boolean].map(Json.Bool(_))
    val str: Gen[Json] = Gen.asciiString.map(Json.Str(_))
    val num: Gen[Json] = for {
      num <- Gen[java.math.BigDecimal]
      // fallback to null if we ever get a number that is too big
    } yield
      if (num.unscaledValue.bitLength > 128) Json.Null
      else Json.Num(num)

    val nul: Gen[Json] = Gen.value(Json.Null)

    Gen.oneOf(obj, arr, boo, str, num)
  }

  implicit val strShrinker: Shrink[String] = Shrink.shrink { txt =>
    if (txt.isEmpty) Stream.empty[String]
    else Stream(txt.drop(1), txt.reverse.drop(1).reverse)
  }

  implicit lazy val astShrinker: Shrink[Json] = Shrink.shrink {
    case Json.Obj(entries) => Shrink.list[(String, Json)].apply(entries.toList).map(Chunk.fromIterable(_)).map(Json.Obj(_))
    case Json.Arr(entries)  => Shrink.list[Json].apply(entries.toList).map(Chunk.fromIterable(_)).map(Json.Arr(_))
    case Json.Bool(_)      => Stream.empty[Json]
    case Json.Str(txt)     => strShrinker(txt).map(Json.Str(_))
    case Json.Num(_)       => Stream.empty[Json]
    case Json.Null            => Stream.empty[Json]
  }

  val asts = property { i: Json => roundtrip(i) }

}
