package testzio.json

import java.io.{ File, FileNotFoundException, IOException }
import java.math.BigInteger

import zio._
import zio.blocking._
import zio.random.Random
import zio.stream._
import zio.test.{ Gen, Sized }

object TestUtils {
  val genBigInteger: Gen[Random, BigInteger] =
    Gen
      .bigInt((BigInt(2).pow(128) - 1) * -1, BigInt(2).pow(128) - 1)
      .map(_.bigInteger)
      .filter(_.bitLength < 128)

  val genBigDecimal: Gen[Random, java.math.BigDecimal] =
    Gen
      .bigDecimal((BigDecimal(2).pow(128) - 1) * -1, BigDecimal(2).pow(128) - 1)
      .map(_.bigDecimal)
      .filter(_.toBigInteger.bitLength < 128)

  // Something seems to be up with zio-test’s Gen.usASCII, it returns
  // strings like 'Chunk(<>)' (Chunk#toString?) containing any ASCII chars
  // This generator matches ScalaProps
  val genUsAsciiString: Gen[Random with Sized, String] =
    Gen.string(Gen.oneOf(Gen.char('!', '~')))

  val genAlphaLowerString: Gen[Random with Sized, String] =
    Gen.string(Gen.oneOf(Gen.char('a', 'z')))

  def writeFile(path: String, s: String): Unit = {
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(path))
    bw.write(s)
    bw.close()
  }

  def getResourceAsString(res: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(res)
    try {
      val baos     = new java.io.ByteArrayOutputStream()
      val data     = Array.ofDim[Byte](2048)
      var len: Int = 0
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

  def getResourcePaths(folderPath: String): ZIO[Blocking, IOException, Vector[String]] =
    effectBlockingIO {
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
