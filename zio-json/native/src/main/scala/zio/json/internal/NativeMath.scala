package zio.json.internal

import scala.scalanative.unsafe._

// FIXME: Replace by an _efficient_ cross-platform version later, see: https://github.com/scala-native/scala-native/issues/2473
@extern
private[internal] object NativeMath {
  @name("zio_json_multiply_high")
  def multiplyHigh(x: Long, y: Long): Long = extern

  @name("zio_json_unsigned_multiply_high")
  def unsignedMultiplyHigh(x: Long, y: Long): Long = extern
}
