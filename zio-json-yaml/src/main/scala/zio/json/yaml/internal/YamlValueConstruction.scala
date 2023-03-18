package zio.json.yaml.internal

import org.yaml.snakeyaml.constructor.SafeConstructor
import org.yaml.snakeyaml.nodes.{ MappingNode, Node }
import scala.annotation.nowarn

@nowarn
private[yaml] final class YamlValueConstruction extends SafeConstructor {
  def toJavaValue(node: Node): AnyRef =
    getConstructor(node).construct(node)

  def processMappingNode(node: MappingNode): MappingNode = {
    flattenMapping(node)
    node
  }
}
