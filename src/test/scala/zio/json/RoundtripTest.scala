package zio.json

import scala.collection.immutable

import zio.json
import json._
import json.syntax._
import scalaprops._
import Property.{ implies, prop, property }
import scala.collection.mutable

// testOnly *RoundtripTest
object RoundtripTest extends Scalaprops {

  def roundtrip[A: Encoder: Decoder](a: A) = prop(json.parser.decode[A](a.toJson) == Right(a))

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

  implicit lazy val astGen: Gen[JsValue] = Gen.sized { size =>
    val entry: Gen[(String, JsValue)] = Gen.delay(Gen.apply2(Gen.asciiString, astGen)((a, b) => (a, b)))
    // objects and arrays should get smaller with depth to avoid infinite recursion
    val size_             = 0 min (size - 1)
    val obj: Gen[JsValue] = Gen.delay(Gen.listOfN(size_, entry)).map(JsObject(_))
    val arr: Gen[JsValue] = Gen.delay(Gen.listOfN(size_, astGen)).map(JsArray(_))
    val boo: Gen[JsValue] = Gen[Boolean].map(JsBoolean(_))
    val str: Gen[JsValue] = Gen.asciiString.map(JsString(_))
    val num: Gen[JsValue] = for {
      num <- Gen[java.math.BigDecimal]
      // fallback to null if we ever get a number that is too big
    } yield
      if (num.unscaledValue.bitLength > 128) JsNull
      else JsNumber(num)

    val nul: Gen[JsValue] = Gen.value(JsNull)

    Gen.oneOf(obj, arr, boo, str, num)
  }

  implicit val strShrinker: Shrink[String] = Shrink.shrink { txt =>
    if (txt.isEmpty) Stream.empty[String]
    else Stream(txt.drop(1), txt.reverse.drop(1).reverse)
  }

  implicit lazy val astShrinker: Shrink[JsValue] = Shrink.shrink {
    case JsObject(entries) => Shrink.list[(String, JsValue)].apply(entries).map(JsObject(_))
    case JsArray(entries)  => Shrink.list[JsValue].apply(entries).map(JsArray(_))
    case JsBoolean(_)      => Stream.empty[JsValue]
    case JsString(txt)     => strShrinker(txt).map(JsString(_))
    case JsNumber(_)       => Stream.empty[JsValue]
    case JsNull            => Stream.empty[JsValue]
  }

  val asts = property { i: JsValue => roundtrip(i) }

}
