package codegen.model

import Types.{AttributeMap, _}
import codegen.evaluate.SPStateValue
import codegen.internal.Attribute
import codegen.internal.Attribute.{AttrObject, NamedAttrObject, ValidAttr}
import play.api.libs.json.{JsObject, Json, Writes}

sealed trait Identifiable extends Equals {
  val name: String
  val id: ID
  val spAttributes: AttributeMap

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Identifiable]
  override def equals(other: Any): Boolean = other match {
    case that: Identifiable => this.id == that.id
    case _ => false
  }
}

case class NamespacedIdentifiable(namespace: String, identifiable: Identifiable) extends Identifiable {
  override val name: String = s"${namespace}_${identifiable.name}"
  override val id: ID = identifiable.id
  override val spAttributes: AttributeMap = identifiable.spAttributes
}

case class Operation(
                      name: String,
                      conditions: List[Condition] = List(),
                      spAttributes: AttributeMap = AttributeMap(),
                      id: ID = ID()
                    ) extends Identifiable

case class SPState(name: String = "state",
                   state: Map[ID, SPStateValue] = Map(),
                   spAttributes: AttributeMap = AttributeMap(),
                   id: ID = ID()) extends Identifiable {
  def get(id: ID): Option[SPStateValue] = state.get(id)
}

object SPState {
  implicit def SPStateValueConversion[A: Writes](a: A): SPStateValue.Value = SPStateValue.Value(Json.toJson(a))
}

case class Thing(
                  name: String,
                  spAttributes: AttributeMap,
                  id: ID = ID()
                ) extends Identifiable

object Thing {
  def forGen(name: String): GenThing = GenThing(name, AttrObject())
  def forGen(name: String, attributes: NamedAttrObject): GenThing = GenThing(name, attributes)
}

case class GenThing(name: String, attributes: Attribute, id: ID = ID()) extends Identifiable {

  def this(name: String, obj: AttrObject) {
    this(name, obj.nameByKey(name).asInstanceOf[Attribute])
  }

  override val spAttributes: AttributeMap = attributes.toSPValue match {
    case x: AttributeMap => x
    case x => Json.obj(Attribute.ValuePrefix -> x)
  }

  // TODO Use proper error handling instead of defaulting to entire object
  def domain: Attribute = attributes match {
    case o@NamedAttrObject(_, _) => o.get(Attribute.DomainKey).getOrElse(o)
    case o@AttrObject(_) => o.get(Attribute.DomainKey).getOrElse(o)
    case x => x
  }

  def setAttributes(attrs: (String, ValidAttr)*): GenThing = copy(attributes = Attribute(attrs:_*))
}

object GenThing {
  def apply(name: String, attrs: AttrObject): GenThing = new GenThing(name, attrs)
  def apply(name: String, attrs: NamedAttrObject): GenThing = new GenThing(name, attrs)
}

case class OperationOrderSpecification(
                                        name: String,
                                        operationOrders: List[OperationOrder],
                                        spAttributes: AttributeMap = AttributeMap(),
                                        id: ID = ID()
                                      ) extends Identifiable