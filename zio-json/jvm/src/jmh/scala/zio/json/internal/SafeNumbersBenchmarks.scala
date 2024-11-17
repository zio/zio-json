package zio.json.internal

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class SafeNumbersBenchInt {

  // @Param(Array("100", "1000"))
  var size: Int = 10000

  // invalid input. e.g. out of range longs
  var invalids: Array[String] = _
  // numbers randomly distributed
  var valids: Array[String] = _

  @Setup
  def setup(): Unit = {
    val r = new scala.util.Random(0L)

    invalids = Array.fill(size) {
      if (r.nextBoolean())
        r.between(Long.MinValue, Int.MinValue.toLong).toString
      else
        r.between(Int.MaxValue.toLong, Long.MaxValue).toString
    }
    valids = Array.fill(size)(r.nextInt().toString)

    // sanity checks
    assert(decodeStdlibInvalid().toList == decodeFommilInvalid().toList)
    assert(decodeStdlibValid().toList == decodeFommilValid().toList)
  }

  def stdlib(s: String): IntOption =
    try IntSome(s.toInt)
    catch {
      case _: java.lang.NumberFormatException => IntNone
    }

  @Benchmark
  def decodeStdlibValid(): Array[IntOption] = valids.map(stdlib)

  @Benchmark
  def decodeScala213Valid(): Array[Option[Int]] = valids.map(_.toIntOption)

  @Benchmark
  def decodeFommilValid(): Array[IntOption] = valids.map(SafeNumbers.int)

  @Benchmark
  def decodeFommilValidUnsafe(): Array[Int] = valids.map(UnsafeNumbers.int)

  @Benchmark
  def decodeStdlibInvalid(): Array[IntOption] = invalids.map(stdlib)

  @Benchmark
  def decodeScala213Invalid(): Array[Option[Int]] = invalids.map(_.toIntOption)

  @Benchmark
  def decodeFommilInvalid(): Array[IntOption] = invalids.map(SafeNumbers.int)

}

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class SafeNumbersBenchFloat {
  // @Param(Array("100", "1000"))
  var size: Int = 10000

  var invalids: Array[String] = _
  var valids: Array[String]   = _

  @Setup
  def setup(): Unit = {
    val r = new scala.util.Random(0L)

    invalids = Array.fill(size) {
      val s = r.nextString(10)
      stdlib(s) match {
        case FloatNone    => s
        case FloatSome(_) => "wibble"
      }
    }
    valids = Array.fill(size) {
      val e = r.nextInt(40) - 20
      var s = r.nextInt(0x000fffff) // 20 bits of precision
      if (r.nextBoolean()) s = -s
      s"${s}e${e}"
    }

    // sanity checks
    assert(decodeStdlibInvalid().toList == decodeFommilInvalid().toList)
    assert(decodeStdlibValid().toList == decodeFommilValid().toList)
  }

  def stdlib(s: String): FloatOption =
    try FloatSome(s.toFloat)
    catch {
      case _: java.lang.NumberFormatException => FloatNone
    }

  @Benchmark
  def decodeStdlibValid(): Array[FloatOption] = valids.map(stdlib)

  @Benchmark
  def decodeScala213Valid(): Array[Option[Float]] = valids.map(_.toFloatOption)

  @Benchmark
  def decodeFommilValid(): Array[FloatOption] = valids.map(SafeNumbers.float(_))

  @Benchmark
  def decodeFommilUnsafeValid(): Array[Float] =
    valids.map(UnsafeNumbers.float(_, 128))

  @Benchmark
  def decodeStdlibInvalid(): Array[FloatOption] = invalids.map(stdlib)

  @Benchmark
  def decodeScala213Invalid(): Array[Option[Float]] =
    invalids.map(_.toFloatOption)

  @Benchmark
  def decodeFommilInvalid(): Array[FloatOption] =
    invalids.map(SafeNumbers.float(_))

}

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class SafeNumbersBenchBigDecimal {
  // @Param(Array("100", "1000"))
  var size: Int = 10000

  var invalids: Array[String] = _
  var valids: Array[String]   = _

  @Setup
  def setup(): Unit = {
    val r = new scala.util.Random(0L)

    invalids = Array.fill(size) {
      val s = r.nextString(10)
      stdlib(s) match {
        case None    => s
        case Some(_) => "wibble"
      }
    }
    valids = Array.fill(size) {
      val bytes: Array[Byte] = Array.ofDim(8)
      r.nextBytes(bytes)
      val scale = r.nextInt()
      new java.math.BigDecimal(new java.math.BigInteger(bytes))
        .scaleByPowerOfTen(scale)
        .toString
    }

    // sanity checks
    assert(decodeStdlibInvalid().toList == decodeFommilInvalid().toList)
    assert(decodeStdlibValid().toList == decodeFommilValid().toList)
  }

  def stdlib(s: String): Option[java.math.BigDecimal] =
    try Some(new java.math.BigDecimal(s))
    catch {
      case _: java.lang.NumberFormatException => None
    }

  @Benchmark
  def decodeStdlibValid(): Array[Option[java.math.BigDecimal]] =
    valids.map(stdlib)

  @Benchmark
  def decodeFommilValid(): Array[Option[java.math.BigDecimal]] =
    valids.map(SafeNumbers.bigDecimal(_))

  @Benchmark
  def decodeFommilUnsafeValid(): Array[java.math.BigDecimal] =
    valids.map(UnsafeNumbers.bigDecimal(_, 128))

  @Benchmark
  def decodeStdlibInvalid(): Array[Option[java.math.BigDecimal]] =
    invalids.map(stdlib)

  @Benchmark
  def decodeFommilInvalid(): Array[Option[java.math.BigDecimal]] =
    invalids.map(SafeNumbers.bigDecimal(_))

}
