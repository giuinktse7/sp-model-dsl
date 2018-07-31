package codegen

import model._
import model.Types._
import IdentifiableGraph.IdentifiableToNodeGraph
import OperationNode.operationToNode


object ExampleModel {
  val id = java.util.UUID.randomUUID()
  java.util.UUID.fromString(id.toString)
/*
  def create(): List[Identifiable] = {
    val t1 = Thing("v1", AttributeMap("domain"-> List("foo", "bar")))
    val t2 = Thing("v2", AttributeMap("domain"-> List(true, false)))

    val conditionForO1 = Condition(AND(List(EQ(SVIDEval(t1.id), ValueHolder("foo")), NEQ(SVIDEval(t2.id), ValueHolder(true)))))
    val o1 = Operation("o1", List(conditionForO1))

    val conditionForO2 = Condition(EQ(SVIDEval(o1.id), ValueHolder("e")))
    val o2 = Operation("o2", List(conditionForO2))

    val r1 = Thing("r1", AttributeMap("someAttr" -> "kalle"))

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential(o1, o2))

    val graph = r1 has (
      o1 has (t1, t2),
      o2 has t2
    )


    val model: List[Identifiable] = List(t1, t2, o1, o2, r1, sop)

    model
  }
*/
}