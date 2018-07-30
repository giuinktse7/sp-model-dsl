package codegen.definition

import codegen.Dependency

case class CaseClass(name: String, outerDependencies: Set[Dependency], fields: CaseVal*) {
  val dependencies: Set[Dependency] = (fields.flatMap(_.dependencies) ++ outerDependencies).toSet
}

object CaseClass {
  def apply(name: String, fields: CaseVal*): CaseClass = new CaseClass(name, Set(), fields:_*)
  def withDependencies(name: String, dependencies: Set[Dependency], fields: CaseVal*): CaseClass = new CaseClass(name, dependencies, fields:_*)
}