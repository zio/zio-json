package testzio.json.internal

import zio.json.internal._
import zio.test.Assertion._
import zio.test._

object StringMatrixSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] = suite("StringMatrix")(
    test("basic positive succeeds") {
      val names   = List("a", "b")
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
    test("first resolves to field index") {
      check(genTestStrings) { xs =>
        val m = new StringMatrix(xs.toArray)
        val asserts = xs.indices.map { i =>
          val test = xs(i)
          var bs = test.zipWithIndex.foldLeft(m.initial) { case (bs, (c, i)) =>
            m.update(bs, i, c.toInt)
          }
          bs = m.exact(bs, test.length)
          m.first(bs) == i
        }
        assert(asserts)(forall(isTrue))
      }
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
      check(genNonEmptyString.filterNot(_.startsWith("wibble")))(s =>
        assert(matcher(List("wibble"), List(s -> 0), s))(equalTo(List(s)))
      )
    },
    test("alias exact match is a substring") {
      assert(
        matcher(
          List("wibble"),
          List("retweeted_status" -> 0, "retweeted" -> 0),
          "retweeted"
        )
      )(equalTo(List("retweeted")))
    },
    test("alias first resolves to aliased field index") {
      check(genTestStringsAndAliases) { case (xs, aliases) =>
        val m = new StringMatrix(xs.toArray, aliases.toArray)
        val asserts = aliases.indices.map { i =>
          val test = aliases(i)._1
          var bs = test.zipWithIndex.foldLeft(m.initial) { case (bs, (c, i)) =>
            m.update(bs, i, c.toInt)
          }
          bs = m.exact(bs, test.length)
          m.first(bs) == aliases(i)._2
        }
        assert(asserts)(forall(isTrue))
      }
    }
  )

  val genNonEmptyString =
    Gen.alphaNumericString.filter(_.nonEmpty)

  val genTestStrings =
    for {
      n  <- Gen.int(1, 63)
      xs <- Gen.setOfN(n)(genNonEmptyString)
    } yield xs.toList

  val genTestStringsAndAliases =
    for {
      xsn    <- Gen.int(1, 63)
      xs     <- Gen.setOfN(xsn)(genNonEmptyString)
      an     <- Gen.int(0, 63 - xsn)
      aliasF <- Gen.setOfN(an)(genNonEmptyString.filter(a => !xs.contains(a))).map(_.toList)
      aliasN <- Gen.listOfN(an)(Gen.int(0, xsn - 1))
    } yield (xs.toList, aliasF zip aliasN)

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
