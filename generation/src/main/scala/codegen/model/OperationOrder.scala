package codegen.model

import Types._
import codegen.internal.Result

/**
  * SOP (Sequence of Operation)
  */
sealed trait OperationOrder {
  val operationOrders: List[OperationOrder]
  val nodeID: ID = ID()
}

case class Parallel(operationOrders: List[OperationOrder]) extends OperationOrder
case class Alternative(operationOrders: List[OperationOrder]) extends OperationOrder
case class Arbitrary(operationOrders: List[OperationOrder]) extends OperationOrder
case class Sequential(operationOrders: List[OperationOrder]) extends OperationOrder
case class SometimeSequence(operationOrders: List[OperationOrder]) extends OperationOrder
case class Other(operationOrders: List[OperationOrder]) extends OperationOrder
case class OperationNode(operation: ID, conditions: List[Condition] = List(), operationOrders: List[OperationOrder] = List()) extends OperationOrder

object OperationNode {
  def apply(id: ID): OperationNode = OperationNode(id, List())

  implicit def operationToNode(operation: Operation): OperationNode = apply(operation.id)
}


object OperationOrder {
  type Specification = OperationOrderSpecification
  object Specification {
    def apply(name: String, operationOrders: OperationOrder*) = OperationOrderSpecification(name, operationOrders.toList)
  }
  case object None extends OperationOrder {
    val operationOrders: List[OperationOrder]  = List[OperationOrder]()
  }

  object Sequential {
    def apply(operationOrders: OperationOrder*): Sequential = new Sequential(operationOrders.toList)
  }

  def apply(children: OperationOrder*): OperationOrder = OperationOrder(children.toList)
  def apply(children: List[OperationOrder]): OperationOrder = children match {
    case Nil => None
    case h :: Nil => h
    case _ => Parallel(children)
  }

  def apply(op: Operation) = OperationNode(op.id)
  def apply(op: Operation, children: OperationOrder) = OperationNode(op.id, List(), List(children))
  def apply(op: ID) = OperationNode(op)


}

