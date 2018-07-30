package codegen.definition

case class CaseClass(name: String, outerDependencies: Set[String], fields: CaseVal*) {
  val dependencies: Set[String] = (fields.flatMap(_.dependencies) ++ outerDependencies).toSet
}

object CaseClass {
  def apply(name: String, fields: CaseVal*): CaseClass = new CaseClass(name, Set(), fields:_*)
  def withDependencies(name: String, dependencies: Set[String], fields: CaseVal*): CaseClass = new CaseClass(name, dependencies, fields:_*)
}