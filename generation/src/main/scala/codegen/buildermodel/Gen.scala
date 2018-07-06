package codegen.buildermodel

import codegen.Attribute.{AttrObject, NamedAttrObject}
import codegen.model.Types.{AttributeMap, ID}

/**
  * These classes provide additional functionality for normal Identifiable classes. Their
  * purpose is to improve the DSL.
  */
object Gen {
  case class GenThing(name: String, attrObject: NamedAttrObject, id: ID = ID()) {
    def this(name: String, obj: AttrObject) {
      this(name, obj.nameByKey(name))
    }
  }

  object GenThing {
    def apply(name: String, attrs: AttrObject): GenThing = new GenThing(name, attrs)
    def apply(name: String, attrs: NamedAttrObject): GenThing = new GenThing(name, attrs)
  }
}