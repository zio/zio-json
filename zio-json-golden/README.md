# zio-json-golden

This module provides golden testing for your data types.

## What is golden testing?

Golden testing, also known as snapshot testing, is a way to ensure that the way your data type is encoded and decoded the way you expect.

By golden testing your datatype, you save the expected serialization output into a separate file. That file is then used as a reference from which you ensure that your data type can be serialized and deserialized.


[Further reading](https://ro-che.info/articles/2017-12-04-golden-tests)


### Traditional way

Without golden testing, it's usually done the following way:

```scala
import zio.json._
import zio.test._
object EncodeDecodeSpec extends ZIOSpecDefault {

  case class Banana(curvature: Double)
  object Banana {
    implicit val codec: JsonCodec[Banana] = DeriveJsonCodec.gen[Banana]
  }

  def spec = suite("EncodeDecodeSpec")(
    test("Encode/decode test for Banana") {
      val banana   = Banana(0.5)
      val expected = """{"curvature":0.5}"""
      assertTrue(
        expected.fromJson[Banana].toOption.get == banana,
        banana.toJson == expected
      )
    }
  )
}
```
That's a lot of boilerplate for such a simple test.
Let's show how we can do better using zio-json-golden.

### Simple Example
```scala
import zio.json._
import zio.test._
import zio.json.golden._
object EncodeDecodeSpec extends ZIOSpecDefault {

  case class Banana(curvature: Double)
  object Banana {
    implicit val codec: JsonCodec[Banana] = DeriveJsonCodec.gen[Banana]
  }

  def spec = suite("EncodeDecodeSpec")(
    goldenTest(DeriveGen[Banana])
  )
}
```

This test will generate a reference file under `src/test/resources/golden/`.

The test will fail the first time it is run since there's no reference file. The file will have `_new` suffix appended to it. To fix the test, simply remove `_new` from the file name and run the test.

If the `Banana` datatype is modified in an incompatible way in the future, then this will fail the test and generate a `_changed` file.
If the change is intended, then simply copy the contents of the `_changed` file into the current reference file and re-run the test. If the change is not intended, then the test has served its purpose!

### Configuration

It's possible to override the default configuration of the relative path under the `golden` directory (in case of type name conflicts for instance) and change the default sample size (which is 20).

This can be done by supplying a different `GoldenConfiguration` to the scope

```scala
  {
    implicit val config: GoldenConfiguration = ???
    goldenTest(DeriveGen[SumType])
  }

```


