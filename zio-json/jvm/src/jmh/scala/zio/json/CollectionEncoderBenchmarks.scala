package zio.json

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import zio.Chunk

import scala.collection.{ SortedMap, immutable }

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class CollectionEncoderBenchmarks {
  private[this] var stringsChunk: Chunk[String]                   = null
  private[this] var stringsList: List[String]                     = null
  private[this] var stringsMap: immutable.Map[String, String]     = null
  private[this] var stringsSeq: immutable.Seq[String]             = null
  private[this] var stringsSet: immutable.Set[String]             = null
  private[this] var stringsSortedMap: SortedMap[String, String]   = null
  private[this] var stringsSortedSet: immutable.SortedSet[String] = null
  private[this] var stringsVector: immutable.Vector[String]       = null

  @Setup
  def setup(): Unit = {
    val vector = (for (i <- 1 to 1000) yield s"test_$i").toVector
    val map    = (for (i <- 1 to 1000) yield s"key_$i" -> "test").toMap

    stringsChunk = Chunk.fromIterable(vector)
    stringsList = vector.toList
    stringsMap = map
    stringsSeq = Seq.from(vector)
    stringsSet = vector.toSet
    stringsSortedMap = immutable.SortedMap.from(map)
    stringsSortedSet = immutable.SortedSet.from(vector)
    stringsVector = vector
  }

  @Benchmark
  def encodeChunk: String =
    stringsChunk.toJson

  @Benchmark
  def encodeList: String =
    stringsList.toJson

  @Benchmark
  def encodeMap: String =
    stringsMap.toJson

  @Benchmark
  def encodeSeq: String =
    stringsSeq.toJson

  @Benchmark
  def encodeSet: String =
    stringsSet.toJson

  @Benchmark
  def encodeSortedMap: String =
    stringsSortedMap.toJson

  @Benchmark
  def encodeSortedSet: String =
    stringsSortedSet.toJson

  @Benchmark
  def encodeVector: String =
    stringsVector.toJson
}
