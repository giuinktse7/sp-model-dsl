package codegen.model

import codegen.model.Types.{AttributeMap, ID}
import util.SimpleSet

case class IdentifiableGraph(self: Identifiable, nodes: SimpleSet[ID, IdentifiableGraph] = SimpleSet(_.self.id)) {
  def name: String = self.name
  def id: ID = self.id
  def attributes: AttributeMap = self.attributes
  def has(nodes: IdentifiableGraph*): IdentifiableGraph = copy(self, this.nodes.addAll(nodes))
}


object IdentifiableGraph {
  implicit class IdentifiableToNodeGraph[A <: Identifiable](override val self: A) extends IdentifiableGraph(self)
}
