package testzio.json

import zio._
import zio.blocking._
import zio.stream._
import java.io.IOException
import java.io.FileNotFoundException

object TestUtils {
  def writeFile(path: String, s: String): Unit = {
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(path))
    bw.write(s)
    bw.close()
  }

  def getResourceAsString(res: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(res)
    try {
      val baos        = new java.io.ByteArrayOutputStream()
      val data        = Array.ofDim[Byte](2048)
      var len: Int    = 0
      def read(): Int = { len = is.read(data); len }
      while (read() != -1)
        baos.write(data, 0, len)
      baos.toString("UTF-8")
    } finally is.close()
  }

  def getResourceAsStringM(res: String): ZIO[Blocking, IOException, String] =
    ZStream.managed {
      val acquire = effectBlockingIO(getClass.getClassLoader.getResourceAsStream(res)).flatMap { x =>
        if (x == null)
          ZIO.fail(new FileNotFoundException(s"No such resource: '$res'"))
        else
          ZIO.succeed(x)
      }

      ZManaged.fromAutoCloseable(acquire)
    }.flatMap(inputStream => ZStream.fromInputStream(inputStream).transduce(ZTransducer.utf8Decode))
      .fold("")(_ ++ _)

  def asChars(str: String): CharSequence =
    new zio.json.internal.FastCharSequence(str.toCharArray)

  def getResourceAsReader(res: String): zio.json.internal.RetractReader =
    new zio.json.internal.WithRetractReader(
      new java.io.InputStreamReader(
        getClass.getClassLoader.getResourceAsStream(res),
        "UTF-8"
      )
    )

}
