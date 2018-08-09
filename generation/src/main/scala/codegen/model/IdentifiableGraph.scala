package codegen.model

import codegen.model.Types.{AttributeMap, ID}
import util.SimpleSet

case class IdentifiableGraph(self: Identifiable, nodes: SimpleSet[ID, IdentifiableGraph]) {
  def name: String = self.name
  def id: ID = self.id
  def attributes: AttributeMap = self.spAttributes
  def ->(nodes: IdentifiableGraph*): IdentifiableGraph = {
    copy(self, nodes = this.nodes.addAll(nodes))
  }

  def collectNodes: List[IdentifiableGraph] = this :: nodes.toList.flatMap(_.collectNodes)
}


object IdentifiableGraph {
  implicit class IdentifiableToNodeGraph[A <: Identifiable](override val self: A) extends IdentifiableGraph(self, SimpleSet(_.self.id))

  def apply(self: Identifiable, nodes: SimpleSet[ID, IdentifiableGraph] = SimpleSet(_.self.id)): IdentifiableGraph = {
    new IdentifiableGraph(self, nodes)
  }
}
