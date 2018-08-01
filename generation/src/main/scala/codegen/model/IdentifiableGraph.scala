package codegen.model

import codegen.model.Types.{AttributeMap, ID}
import util.{SimpleSet, Utils}

case class IdentifiableGraph(self: Identifiable, className: String, nodes: SimpleSet[ID, IdentifiableGraph]) {
  def name: String = self.name
  def id: ID = self.id
  def attributes: AttributeMap = self.attributes
  def ->(nodes: IdentifiableGraph*): IdentifiableGraph = copy(self, nodes = this.nodes.addAll(nodes))
  def withClassName(name: String): IdentifiableGraph = copy(className = name)

  def collectNodes: List[IdentifiableGraph] = this :: nodes.toList.flatMap(_.collectNodes)
}


object IdentifiableGraph {
  implicit class IdentifiableToNodeGraph[A <: Identifiable](override val self: A) extends IdentifiableGraph(self, self.name, SimpleSet(_.self.id))

  def apply(self: Identifiable, nodes: SimpleSet[ID, IdentifiableGraph] = SimpleSet(_.self.id)): IdentifiableGraph = {
    new IdentifiableGraph(self, self.name, nodes)
  }
}
