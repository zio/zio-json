package zio.json.internal

import java.nio.charset.StandardCharsets.UTF_8

/**
 * On Scala.js, [[FastStringWrite]] is backed by a native JS string, so the cheapest route to UTF-8 bytes is the string
 * itself; `getBytes` performs the platform's own lone-surrogate replacement, which is what the byte output is specified
 * against.
 */
private[zio] object Utf8Bytes {
  def fromWrite(write: FastStringWrite): Array[Byte] = write.toString.getBytes(UTF_8)
}
