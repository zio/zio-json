package zio.json.yaml.internal

import org.yaml.snakeyaml.constructor.SafeConstructor
import org.yaml.snakeyaml.nodes.{ MappingNode, Node }
import org.yaml.snakeyaml.LoaderOptions

private[yaml] final class YamlValueConstruction extends SafeConstructor(new LoaderOptions()) {
  def toJavaValue(node: Node): AnyRef =
    getConstructor(node).construct(node)

  def processMappingNode(node: MappingNode): MappingNode = {
    flattenMapping(node)
    node
  }
}
