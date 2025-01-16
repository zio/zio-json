package zio.json

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import zio.Chunk

import scala.collection.immutable

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class CollectionDecoderBenchmarks {
  private[this] var encodedArray: String  = null
  private[this] var encodedObject: String = null

  @Setup
  def setup(): Unit = {
    encodedArray = (for (i <- 1 to 1000) yield s"test_$i").toVector.toJson
    encodedObject = (for (i <- 1 to 1000) yield s"k_$i" -> "test").toMap.toJson
  }

  @Benchmark
  def decodeChunk: Either[String, Chunk[String]] =
    encodedArray.fromJson[Chunk[String]]

  @Benchmark
  def decodeList: Either[String, List[String]] =
    encodedArray.fromJson[immutable.List[String]]

  @Benchmark
  def decodeMap: Either[String, Map[String, String]] =
    encodedObject.fromJson[immutable.Map[String, String]]

  @Benchmark
  def decodeSet: Either[String, Set[String]] =
    encodedArray.fromJson[immutable.Set[String]]

  @Benchmark
  def decodeSeq: Either[String, immutable.Seq[String]] =
    encodedArray.fromJson[immutable.Seq[String]]

  @Benchmark
  def decodeSortedSet: Either[String, Set[String]] =
    encodedArray.fromJson[immutable.SortedSet[String]]

  @Benchmark
  def decodeSortedMap: Either[String, collection.SortedMap[String, String]] =
    encodedObject.fromJson[collection.SortedMap[String, String]]

  @Benchmark
  def decodeVector: Either[String, Vector[String]] =
    encodedArray.fromJson[immutable.Vector[String]]
}
