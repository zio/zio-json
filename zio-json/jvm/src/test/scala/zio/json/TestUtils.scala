package zio.json

import zio._
import zio.stream._

import java.io.{ File, IOException }

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

  def getResourceAsStringM(res: String): ZIO[Any, IOException, String] =
    ZStream
      .fromResource(res)
      .via(ZPipeline.utf8Decode)
      .run(ZSink.foldLeftChunks("")((acc, c) => acc ++ c.mkString))

  def getResourcePaths(folderPath: String): ZIO[Any, IOException, Vector[String]] =
    ZIO.attemptBlockingIO {
      val url    = getClass.getClassLoader.getResource(folderPath)
      val folder = new File(url.getPath)

      folder.listFiles.toVector.map(p => folder.toPath.relativize(p.toPath).toString)
    }

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
