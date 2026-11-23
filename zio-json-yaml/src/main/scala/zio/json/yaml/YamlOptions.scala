package zio.json.yaml

import org.yaml.snakeyaml.DumperOptions.{ FlowStyle, LineBreak, NonPrintableStyle, ScalarStyle }
import org.yaml.snakeyaml.DumperOptions

import zio.json.ast.Json

/**
 * Options for writing YAML with `toYaml` and `toYamlAST`.
 *
 * Start from `YamlOptions.default` and change the fields you need with `copy`.
 *
 * {{{
 * import org.yaml.snakeyaml.DumperOptions.FlowStyle
 * import zio.json.ast.Json
 *
 * val options = YamlOptions.default.copy(dropNulls = false, flowStyle = _ => FlowStyle.FLOW)
 *
 * Json.Obj("a" -> Json.Arr(Json.Num(1), Json.Null)).toYaml(options)
 * // Right("{a: [1, null]}\n")
 * }}}
 *
 * @param newDumperOptions
 *   Creates the SnakeYAML `DumperOptions` that writing starts from, for settings that have no field here. The fields
 *   `indentation`, `sequenceIndentation`, `maxScalarWidth`, `lineBreak` and `indentWithIndicator` are then applied on top
 *   of it.
 * @param dropNulls
 *   Whether object fields whose value is `Json.Null` are left out. Nulls inside arrays are always written.
 * @param indentation
 *   The number of spaces for each indentation level
 * @param sequenceIndentation
 *   The indentation of the `-` indicator of a sequence, passed to `DumperOptions#setIndicatorIndent`
 * @param maxScalarWidth
 *   The preferred maximum line width, after which long scalars are split over several lines. `None` turns splitting off.
 * @param lineBreak
 *   The line break to write
 * @param flowStyle
 *   Chooses the flow or block style of each mapping and sequence, from the `Json` value it is written for
 * @param scalarStyle
 *   Chooses the style of each scalar value (plain, quoted or literal), from the `Json` value it is written for. A plain
 *   string that contains a line break is written as a literal block.
 * @param keyStyle
 *   Chooses the style of each mapping key, from the key
 * @param nonPrintableStyle
 *   How strings with non-printable characters are written: escaped, or, for `NonPrintableStyle.BINARY`, as base64
 *   `!!binary` text
 * @param indentWithIndicator
 *   Passed to `DumperOptions#setIndentWithIndicator`
 */
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

/**
 * Instances of [[YamlOptions]].
 */
object YamlOptions {
  private val defaultLineBreak: LineBreak =
    Set(LineBreak.MAC, LineBreak.WIN, LineBreak.UNIX)
      .find(_.getString == System.lineSeparator())
      .getOrElse(LineBreak.UNIX)

  /**
   * The options used when none are given.
   *
   * Null object fields are dropped, indentation is 2 spaces for both the levels and the sequence indicator, scalars may be
   * split at 80 columns, the line break is the one of the platform when SnakeYAML knows it and Unix otherwise, and
   * mappings, sequences, scalars and keys use SnakeYAML's automatic or plain styles. Non-printable characters are escaped,
   * and the sequence indentation is counted with the indicator.
   */
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
