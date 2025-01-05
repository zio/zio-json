package zio.json.yaml

import org.yaml.snakeyaml.DumperOptions.{ FlowStyle, LineBreak, NonPrintableStyle, ScalarStyle }
import org.yaml.snakeyaml.DumperOptions

import zio.json.ast.Json

case class YamlOptions(
  newDumperOptions: () => DumperOptions, // this to allow user to give a a pre-set dumper for unsupported options
  dropNulls: Boolean,
  indentation: Int,
  sequenceIndentation: Int,
  maxScalarWidth: Option[Int],
  lineBreak: LineBreak,
  flowStyle: Json => FlowStyle,
  scalarStyle: Json => ScalarStyle,
  keyStyle: String => ScalarStyle,
  nonPrintableStyle: NonPrintableStyle,
  indentWithIndicator: Boolean
)

object YamlOptions {
  private val defaultLineBreak: LineBreak =
    Set(LineBreak.MAC, LineBreak.WIN, LineBreak.UNIX)
      .find(_.getString == System.lineSeparator())
      .getOrElse(LineBreak.UNIX)

  val default: YamlOptions = YamlOptions(
    () => new DumperOptions(),
    dropNulls = true,
    indentation = 2,
    sequenceIndentation = 2,
    maxScalarWidth = Some(80),
    lineBreak = defaultLineBreak,
    flowStyle = _ => FlowStyle.AUTO,
    scalarStyle = _ => ScalarStyle.PLAIN,
    keyStyle = _ => ScalarStyle.PLAIN,
    nonPrintableStyle = NonPrintableStyle.ESCAPE,
    indentWithIndicator = true
  )
}
