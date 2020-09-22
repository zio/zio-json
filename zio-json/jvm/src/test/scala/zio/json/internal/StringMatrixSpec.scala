package testzio.json.internal

import zio.json.internal._
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, _ }

object StringMatrixSpec extends DefaultRunnableSpec {
  def spec = suite("StringMatrix")(
    testM("positive succeeds") {
      // Watch out: TestStrings were passed
      check(genTestStrings) { xs =>
        val asserts = xs.map(s => matcher(xs, s).contains(s))

        assert(asserts)(forall(isTrue))
      }
    },
    testM("negative fails") {
      check(genTestStrings.filterNot(_.startsWith("wibble"))) { xs =>
        assert(matcher(xs, "wibble"))(isEmpty)
      }
    },
    testM("substring fails") {
      check(genTestStrings.filter(_.length > 1)) { xs =>
        assert(matcher(xs, xs.mkString))(isEmpty)
      }
    },
    testM("trivial") {
      check(genNonEmptyString)(s => assert(matcher(List(s), s))(equalTo(List(s))))
    },
    test("exact match is a substring") {
      assert(
        matcher(
          List("retweeted_status", "retweeted"),
          "retweeted"
        )
      )(equalTo(List("retweeted")))
    }
  )

  val genNonEmptyString =
    Gen.alphaNumericString.filter(_.nonEmpty)

  val genTestStrings =
    for {
      n  <- Gen.int(1, 63)
      xs <- Gen.listOfN(n)(genNonEmptyString)
    } yield xs

  private def matcher(xs: List[String], test: String): List[String] = {
    val m = new StringMatrix(xs.toArray)
    var bs = test.zipWithIndex.foldLeft(m.initial) {
      case (bs, (c, i)) => m.update(bs, i, c.toInt)
    }
    bs = m.exact(bs, test.length)
    matches(xs, bs)
  }

  private def matches(xs: List[String], bitset: Long): List[String] = {
    var hits: List[String] = Nil
    var i                  = 0
    while (i < xs.length) {
      if (((bitset >>> i) & 1L) == 1L)
        hits = xs(i) :: hits
      i += 1
    }
    hits
  }
}
