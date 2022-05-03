package zio.json

import org.openjdk.jmh.annotations._
import zio.Chunk
import zio.json.uuid.UUIDParser
import zio.test.Gen

import java.util.UUID
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class UUIDBenchmarks {
  private[this] var unparsedUUIDChunk: Chunk[String] = _

  @Setup
  def setup(): Unit = {
    val section1   = Gen.long(0x0L, 0xffffffffL).map(_.toHexString)
    val section234 = Gen.long(0x0L, 0xffffL).map(_.toHexString)
    val section5   = Gen.long(0x0L, 0xffffffffffffL).map(_.toHexString)

    val gen = for {
      s1 <- section1
      s2 <- section234
      s3 <- section234
      s4 <- section234
      s5 <- section5
    } yield s"$s1-$s2-$s3-$s4-$s5"

    unparsedUUIDChunk = zio.Runtime.default.unsafeRun(gen.runCollectN(10000).map(Chunk.fromIterable))
  }

  @Benchmark
  def parseInBuiltUUID: Chunk[UUID] =
    unparsedUUIDChunk.map(UUID.fromString)

  @Benchmark
  def parseCustom: Chunk[UUID] =
    unparsedUUIDChunk.map(UUIDParser.unsafeParse)
}
