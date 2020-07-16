package zio.json.internal

import scalaprops._
import Property.{implies, prop, property}
import utest._

// testOnly *StringMatrix*
object StringMatrixProps extends Scalaprops {
  val nonEmptyString: Gen[String] = Gen.nonEmptyString(Gen.alphaNumChar)
  val testStrings: Gen[List[String]] =
    Gen.choose(1, 63).flatMap(n => Gen.sequenceNList(n, nonEmptyString))

  def matcher(xs: List[String], test: String): List[String] = {
    val m = new StringMatrix(xs.toArray)
    var bs = test.zipWithIndex.foldLeft(m.initial) {
      case (bs, (c, i)) => m.update(bs, i, c)
    }
    bs = m.exact(bs, test.length)
    matches(xs, bs)
  }

  def matches(xs: List[String], bitset: Long): List[String] = {
    var hits: List[String] = Nil
    var i = 0
    while (i < xs.length) {
      if (((bitset >>> i) & 1L) == 1L)
        hits = xs(i) :: hits
      i += 1
    }
    hits
  }

  val positiveSucceeds = property { xs: List[String] =>
    xs.map(s => prop(matcher(xs, s).contains(s))).reduce(_ and _)
  }(testStrings, Shrink.empty)

  val negativeFails = property { xs: List[String] =>
    implies(
      !xs.exists(_.startsWith("wibble")),
      prop(matcher(xs, "wibble") == Nil)
    )
  }(testStrings, Shrink.empty)

  val substringFails = property { xs: List[String] =>
    implies(xs.length > 1, prop(matcher(xs, xs.mkString) == Nil))
  }(testStrings, Shrink.empty)

  val trivial = property { s: String =>
    prop(matcher(List(s), s) == List(s))
  }(nonEmptyString, Shrink.empty)
}

object StringMatrixTest extends TestSuite {

  val tests = Tests {
    test("exact match is a substring") {
      StringMatrixProps.matcher(
        List("retweeted_status", "retweeted"),
        "retweeted"
      ) ==> List("retweeted")
    }
  }

}
