package zio.json

import zio._
import zio.json.ast.Json
import zio.json.TestUtils._
import zio.test._

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Parity of `decodeJson(Chunk[Byte])` against `decodeJson(CharSequence)` over every JSON file in the test resources.
 *
 * These corpora exist to break parsers: JSONTestSuite alone carries invalid UTF-8, byte order marks, lone surrogates,
 * pathological nesting and numbers, and the real world fixtures are large enough to span many window refills. Whether
 * zio-json accepts or rejects any given file is beside the point here — both paths simply have to agree, so the sweep
 * stays meaningful even for the inputs the parser rightly refuses.
 *
 * JVM only: it needs classpath resource enumeration, and the comparison is against the JVM's UTF-8 decoder, which the
 * JS and Native ones do not always match on malformed input.
 */
object ChunkDecoderCorpusSpec extends ZIOSpecDefault {

  private def resourceBytes(path: String): Chunk[Byte] = {
    val in = getClass.getClassLoader.getResourceAsStream(path)
    try {
      val out  = new java.io.ByteArrayOutputStream()
      val data = Array.ofDim[Byte](2048)
      var len  = in.read(data)
      while (len != -1) {
        out.write(data, 0, len)
        len = in.read(data)
      }
      Chunk.fromArray(out.toByteArray)
    } finally in.close()
  }

  /** The chunk shapes worth exercising: read in place, and the windowed path in two arrangements. */
  private def shapes(bs: Chunk[Byte]): List[(String, Chunk[Byte])] = {
    val arr  = bs.toArray
    val half = arr.length / 2
    List(
      "ByteArray" -> Chunk.fromArray(arr),
      "Concat"    -> (Chunk.fromArray(arr.take(half)) ++ Chunk.fromArray(arr.drop(half))),
      "Slice"     -> Chunk.fromArray(Array[Byte](1, 2) ++ arr).drop(2)
    )
  }

  /**
   * Compared as rendered text rather than with `==`, because `Json.equals` folds an object's fields into a `Map`: two
   * structurally identical ASTs with a duplicated key compare unequal, so `==` reports a difference where there is none
   * (`y_object_duplicated_key.json`). Rendering keeps field order and duplicates, so it is the faithful comparison
   * here.
   */
  private def render[A](result: Either[String, A]): String = result.fold("Left:" + _, v => "Right:" + v)

  /** Returns a description of each disagreement, empty when every shape matched the `CharSequence` path. */
  private def disagreements(path: String, bs: Chunk[Byte]): List[String] = {
    def compare[A](label: String)(implicit d: JsonDecoder[A]): List[String] = {
      // both sides start from the same bytes, so a mis-decoded multi-byte char is not what is being compared
      val expected = render(new String(bs.toArray, UTF_8).fromJson[A])
      shapes(bs).collect {
        case (shape, c) if render(c.fromJson[A]) != expected => s"$path [$label/$shape]"
      }
    }

    compare[Json]("Json") ::: compare[List[Json]]("List[Json]") ::: compare[Map[String, Json]]("Map")
  }

  /** `expected` guards against the sweep quietly covering nothing if the resources ever move. */
  private def sweep(folder: String, expected: Int): ZIO[Any, IOException, TestResult] =
    for {
      paths <- getResourcePaths(folder)
      files  = paths.filter(p => p.endsWith(".json") || p.endsWith(".jsonlines")).sorted
      bad    = files.flatMap(p => disagreements(p, resourceBytes(s"$folder/$p")))
    } yield assertTrue(files.length >= expected, bad == Vector.empty[String])

  val spec: Spec[Environment, IOException] =
    suite("ChunkDecoderCorpus")(
      test("agrees with the CharSequence path on every JSONTestSuite file") {
        // 283 files from JSONTestSuite by Nicolas Seriot: https://github.com/nst/JSONTestSuite
        sweep("json_test_suite", expected = 283)
      },
      test("agrees with the CharSequence path on the jawn corpus") {
        sweep("jawn", expected = 8)
      },
      test("agrees with the CharSequence path on the real world fixtures") {
        val files = List(
          "google_maps_api_response.json",
          "google_maps_api_compact_response.json",
          "google_maps_api_error_response.json",
          "google_maps_api_attack0.json",
          "google_maps_api_attack1.json",
          "google_maps_api_attack2.json",
          "google_maps_api_attack3.json",
          "google_maps_api_extra.json",
          "twitter_api_response.json",
          "twitter_api_compact_response.json",
          "twitter_api_error_response.json",
          "che.geo.json",
          "che-2.geo.json",
          "che-err.geo.json"
        )

        assertTrue(files.flatMap(p => disagreements(p, resourceBytes(p))) == List.empty[String])
      },
      test("agrees with the CharSequence path on every truncation of a real payload") {
        // walks the parser into its end-of-input and retract corners against a document of real shape and size
        val all = resourceBytes("google_maps_api_compact_response.json").toArray

        val bad = (0 to all.length by 7).flatMap { n =>
          val prefix   = Chunk.fromArray(all.take(n))
          val expected = render(new String(prefix.toArray, UTF_8).fromJson[Json])
          if (render(prefix.fromJson[Json]) != expected) Some(n) else None
        }

        assertTrue(all.length > 8192, bad == Vector.empty[Int])
      }
    )
}
