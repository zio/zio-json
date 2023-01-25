package testzio.json.internal

import zio.json.internal._
import zio.test.Assertion._
import zio.test._

object StringMatrixSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] = suite("StringMatrix")(
    test("basic positive succeeds") {
      val names = List("a", "b")
      val aliases = List("c" -> 0, "d" -> 1)
      val asserts =
        names.map(s => matcher(names, aliases, s).contains(s)) ++
        aliases.map(a => matcher(names, aliases, a._1).contains(a._1))
      assert(asserts)(forall(isTrue))
    },
    test("positive succeeds") {
      // Watch out: TestStrings were passed
      check(genTestStrings) { xs =>
        val asserts = xs.map(s => matcher(xs, List.empty, s).contains(s))

        assert(asserts)(forall(isTrue))
      }
    },
    test("negative fails") {
      check(genTestStrings.filterNot(_.startsWith("wibble")))(xs => assert(matcher(xs, List.empty, "wibble"))(isEmpty))
    },
    test("substring fails") {
      check(genTestStrings.filter(_.length > 1))(xs => assert(matcher(xs, List.empty, xs.mkString))(isEmpty))
    },
    test("trivial") {
      check(genNonEmptyString)(s => assert(matcher(List(s), List.empty, s))(equalTo(List(s))))
    },
    test("exact match is a substring") {
      assert(
        matcher(
          List("retweeted_status", "retweeted"),
          List.empty,
          "retweeted"
        )
      )(equalTo(List("retweeted")))
    },
    test("alias positive succeeds") {
      // Watch out: TestStrings were passed
      check(genTestStringsAndAliases) { case (xs, aliases) =>
        val asserts =
          xs.map(s => matcher(xs, List.empty, s).contains(s)) ++
          aliases.map(alias => matcher(xs, aliases, alias._1).contains(alias._1))

        assert(asserts)(forall(isTrue))
      }
    },
    test("alias negative fails") {
      check(
        genTestStringsAndAliases.filterNot { case (xs, aliases) =>
          xs.exists(_.startsWith("wibble")) || aliases.exists(_._1.startsWith("wibble"))
        }
      ) { case (xs, aliases) =>
        assert(matcher(xs, aliases, "wibble"))(isEmpty)
      }
    },
    test("alias substring fails") {
      check(
        genTestStringsAndAliases.filter { case (xs, aliases) => xs.length + aliases.length > 1 }
      ) { case (xs, aliases) =>
        assert(matcher(xs, aliases, xs.mkString + aliases.map(_._1).mkString))(isEmpty)
      }
    },
    test("alias trivial") {
      check(genNonEmptyString.filterNot(_.startsWith("wibble")))(s => assert(matcher(List("wibble"), List(s -> 0), s))(equalTo(List(s))))
    },
    test("alias exact match is a substring") {
      assert(
        matcher(
          List("wibble"),
          List("retweeted_status" -> 0, "retweeted" -> 0),
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

  val genTestStringsAndAliases =
    for {
      xsn     <- Gen.int(1, 63)
      xs      <- Gen.listOfN(xsn)(genNonEmptyString)
      an      <- Gen.int(0, 63 - xsn)
      aliases <- Gen.listOfN(an)(genNonEmptyString.filter(x => !xs.contains(x)) <*> Gen.int(0, xsn - 1))
    } yield (xs, aliases)

  private def matcher(xs: List[String], aliases: List[(String, Int)], test: String): List[String] = {
    val m = new StringMatrix(xs.toArray, aliases.toArray)
    var bs = test.zipWithIndex.foldLeft(m.initial) { case (bs, (c, i)) =>
      m.update(bs, i, c.toInt)
    }
    bs = m.exact(bs, test.length)
    matches(xs ++ aliases.map(_._1), bs)
  }

  private def matches(xsAndAliases: List[String], bitset: Long): List[String] = {
    var hits: List[String] = Nil
    var i                  = 0
    while (i < xsAndAliases.length) {
      if (((bitset >>> i) & 1L) == 1L)
        hits = xsAndAliases(i) :: hits
      i += 1
    }
    hits
  }
}
