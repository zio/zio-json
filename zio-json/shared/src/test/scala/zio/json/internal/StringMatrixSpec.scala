package zio.json.internal

import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object StringMatrixSpec extends ZIOSpecDefault {
  val spec: Spec[Environment, Any] = suite("StringMatrix")(
    test("basic positive succeeds") {
      val names   = Array("\uD83D\uDE00" /* a surrogate pair for the grinning face */, "a", "b")
      val aliases = Array("c" -> 0, "d" -> 1)
      val asserts =
        (names.map(s => matcher(names, aliases, s).contains(s)) ++
          aliases.map(a => matcher(names, aliases, a._1).contains(a._1))).toVector
      assert(asserts)(forall(isTrue))
    },
    test("positive succeeds") {
      // Watch out: TestStrings were passed
      check(genTestStrings) { xs =>
        val asserts = xs.map(s => matcher(xs, Array.empty, s).contains(s)).toVector

        assert(asserts)(forall(isTrue))
      }
    },
    test("negative fails") {
      check(genTestStrings.filterNot(_.startsWith("wibble")))(xs =>
        assert(matcher(xs, Array.empty, "wibble").toVector)(isEmpty)
      )
    },
    test("substring fails") {
      check(genTestStrings.filter(_.length > 1))(xs => assert(matcher(xs, Array.empty, xs.mkString).toVector)(isEmpty))
    },
    test("trivial") {
      check(genNonEmptyString)(s => assert(matcher(Array(s), Array.empty, s).toVector)(equalTo(Vector(s))))
    },
    test("exact match is a substring") {
      assert(
        matcher(
          Array("retweeted_status", "retweeted"),
          Array.empty,
          "retweeted"
        ).toVector
      )(equalTo(Vector("retweeted")))
    },
    test("first resolves to field index") {
      check(genTestStrings) { xs =>
        val m = new StringMatrix(xs)
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
          (xs.map(s => matcher(xs, Array.empty, s).contains(s)) ++
            aliases.map(alias => matcher(xs, aliases, alias._1).contains(alias._1))).toVector

        assert(asserts)(forall(isTrue))
      }
    },
    test("alias negative fails") {
      check(
        genTestStringsAndAliases.filterNot { case (xs, aliases) =>
          xs.exists(_.startsWith("wibble")) || aliases.exists(_._1.startsWith("wibble"))
        }
      ) { case (xs, aliases) =>
        assert(matcher(xs, aliases, "wibble").toVector)(isEmpty)
      }
    },
    test("alias substring fails") {
      check(
        genTestStringsAndAliases.filter { case (xs, aliases) => xs.length + aliases.length > 1 }
      ) { case (xs, aliases) =>
        assert(matcher(xs, aliases, xs.mkString + aliases.map(_._1).mkString).toVector)(isEmpty)
      }
    },
    test("alias trivial") {
      check(genNonEmptyString.filterNot(_.startsWith("wibble")))(s =>
        assert(matcher(Array("wibble"), Array(s -> 0), s).toVector)(equalTo(Vector(s)))
      )
    },
    test("alias exact match is a substring") {
      assert(
        matcher(
          Array("wibble"),
          Array("retweeted_status" -> 0, "retweeted" -> 0),
          "retweeted"
        ).toVector
      )(equalTo(Vector("retweeted")))
    },
    test("alias first resolves to aliased field index") {
      check(genTestStringsAndAliases) { case (xs, aliases) =>
        val m = new StringMatrix(xs, aliases)
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
  ) @@ jvm(samples(100)) @@ js(samples(10)) @@ native(samples(10))

  val genNonEmptyString =
    Gen.alphaNumericString.filter(_.nonEmpty)

  val genTestStrings =
    for {
      n  <- Gen.int(1, 64)
      xs <- Gen.setOfN(n)(genNonEmptyString)
    } yield xs.toArray

  val genTestStringsAndAliases =
    for {
      xsn    <- Gen.int(1, 64)
      xs     <- Gen.setOfN(xsn)(genNonEmptyString)
      an     <- Gen.int(0, 64 - xsn)
      aliasF <- Gen.setOfN(an)(genNonEmptyString.filter(a => !xs.contains(a))).map(_.toArray)
      aliasN <- Gen.listOfN(an)(Gen.int(0, xsn - 1)).map(_.toArray)
    } yield (xs.toArray, aliasF zip aliasN)

  private def matcher(xs: Array[String], aliases: Array[(String, Int)], test: String): Array[String] = {
    val m = new StringMatrix(xs, aliases)
    var bs = test.foldLeft(m.initial) {
      var i = 0
      (bs, c) =>
        val nm = m.update(bs, i, c.toInt)
        i += 1
        nm
    }
    bs = m.exact(bs, test.length)
    matches(xs ++ aliases.map(_._1), bs)
  }

  private def matches(xsAndAliases: Array[String], bitset: Long): Array[String] = {
    val hits = Array.newBuilder[String]
    var i    = 0
    while (i < xsAndAliases.length) {
      if (((bitset >>> i) & 1L) != 0) hits += xsAndAliases(i)
      i += 1
    }
    hits.result()
  }
}
