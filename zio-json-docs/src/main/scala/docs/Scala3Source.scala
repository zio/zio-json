package docs

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.jdk.CollectionConverters._

/**
 * Prints Scala 3 example code into a documentation page.
 *
 * The docs project runs mdoc on Scala 2.13 only, so Scala 3 examples live as ordinary sources in the
 * `zio-json-docs-scala3` project, which CI compiles with Scala 3. A page prints them from a `scala mdoc:passthrough`
 * block:
 *
 * {{{
 * docs.Scala3Source.print("zio-json-docs-scala3/src/main/scala-3/zio/json/docs/Example.scala", "derives")
 * }}}
 *
 * A region is the code between a `// #region <name>` line and the next `// #endregion` line. Marker lines are never
 * printed, and the region's common indentation is removed.
 */
object Scala3Source {

  private val RegionStart = """^\s*// #region\s+(\S+)\s*$""".r
  private val RegionEnd   = """^\s*// #endregion.*$""".r

  def read(path: String, region: String = ""): String = {
    val file = Seq(new File(path), new File("..", path)).find(_.exists()).getOrElse {
      throw new IllegalArgumentException(s"Scala 3 example file not found: $path")
    }
    val lines = Files.readAllLines(file.toPath, StandardCharsets.UTF_8).asScala.toVector

    val selected =
      if (region.isEmpty) lines
      else {
        val start = lines.indexWhere {
          case RegionStart(name) => name == region
          case _                 => false
        }
        if (start < 0) throw new IllegalArgumentException(s"Region '$region' not found in $path")
        val rest = lines.drop(start + 1)
        val end  = rest.indexWhere(RegionEnd.pattern.matcher(_).matches())
        if (end < 0) throw new IllegalArgumentException(s"Region '$region' in $path has no // #endregion")
        rest.take(end)
      }

    val code =
      selected.filterNot(l => RegionStart.pattern.matcher(l).matches() || RegionEnd.pattern.matcher(l).matches())
    val indent = code.filter(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length).reduceOption(_ min _).getOrElse(0)
    code.map(_.drop(indent)).mkString("\n").trim
  }

  def print(path: String, region: String = ""): Unit = {
    println("```scala")
    println(read(path, region))
    println("```")
  }
}
