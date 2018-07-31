package codegen.model

import Types.{AttributeMap, _}
import codegen.Attribute.{AttrObject, NamedAttrObject}

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

case class NamespacedIdentifiable(namespace: String, identifiable: Identifiable) extends Identifiable {
  override val name: String = s"${namespace}_${identifiable.name}"
  override val id: ID = identifiable.id
  override val attributes: AttributeMap = identifiable.attributes
}

case class Operation(
                      name: String,
                      conditions: List[Conditional] = List(),
                      attributes: AttributeMap = AttributeMap(),
                      id: ID = ID()
                    ) extends Identifiable

case class Thing(
                  name: String,
                  attributes: AttributeMap,
                  id: ID = ID()
                ) extends Identifiable

object Thing {
  def forGen(name: String, attributes: AttrObject): GenThing = GenThing(name, attributes)
  def forGen(name: String, attributes: NamedAttrObject): GenThing = GenThing(name, attributes)
}

case class GenThing(name: String, attrObject: NamedAttrObject, id: ID = ID()) extends Identifiable {
  def this(name: String, obj: AttrObject) {
    this(name, obj.nameByKey(name))
  }

  override val attributes: AttributeMap = attrObject.toSPValue
}

object GenThing {
  def apply(name: String, attrs: AttrObject): GenThing = new GenThing(name, attrs)
  def apply(name: String, attrs: NamedAttrObject): GenThing = new GenThing(name, attrs)
}

case class OperationOrderSpecification(
                                        name: String,
                                       operationOrders: List[OperationOrder],
                                       attributes: AttributeMap = AttributeMap(),
                                       id: ID = ID()
                                      ) extends Identifiable