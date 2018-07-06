package codegen.model

import Types.{AttributeMap, _}

sealed trait Identifiable extends Equals {
  val name: String
  val id: ID
  val attributes: AttributeMap

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Identifiable]
  override def equals(other: Any): Boolean = other match {
    case that: Identifiable => this.id == that.id
    case _ => false
  }
}

case class Operation(
                      name: String,
                      conditions: List[Condition] = List(),
                      attributes: AttributeMap = AttributeMap(),
                      id: ID = ID()
                    ) extends Identifiable

case class Thing(
                  name: String,
                  attributes: AttributeMap,
                  id: ID = ID()
                ) extends Identifiable

case class StructNode(
                       item: ID,
                      parent: Option[ID] = None,
                      nodeID: ID = ID(),
                      attributes: AttributeMap = AttributeMap())

case class Struct(
                   name: String,
                  items: IdentifiableGraph,
                  attributes: AttributeMap = AttributeMap(),
                  id: ID = ID()
                 ) extends Identifiable {
  // lazy val nodes: SimpleSet[ID, StructNode] = IdentifiableGraph.toNodes
}

object Struct {
  def apply(name: String)(items: IdentifiableGraph): Struct = new Struct(name, items)
}

case class OperationOrderSpecification(
                                        name: String,
                                       operationOrders: List[OperationOrder],
                                       attributes: AttributeMap = AttributeMap(),
                                       id: ID = ID()
                                      ) extends Identifiable