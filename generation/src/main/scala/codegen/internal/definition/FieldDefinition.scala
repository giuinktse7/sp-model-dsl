package codegen.internal.definition

case class FieldDefinition(name: String, className: String)

object FieldDefinition {
  private def className[A](implicit m: Manifest[A]): String = m.toString()

  def apply[A](name: String)(implicit m: Manifest[A]): FieldDefinition = new FieldDefinition(name, className[A])
  def apply(name: String, className: String): FieldDefinition = new FieldDefinition(name, className)
}