package codegen.internal.definition

import codegen.internal.HasType

case class CaseClassDefinition(clazz: String, fields: FieldDefinition*) extends HasType { val `type`: String = clazz }
