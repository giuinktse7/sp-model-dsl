package codegen.definition

import codegen.HasType

case class CaseClassDefinition(clazz: String, fields: FieldDefinition*) extends HasType { val `type`: String = clazz }
