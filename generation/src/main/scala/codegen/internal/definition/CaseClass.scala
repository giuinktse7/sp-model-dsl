package codegen.internal.definition

import codegen.internal.Dependency


case class CaseClass(name: String, outerDependencies: Set[Dependency], caseVals: Seq[CaseVal]) {
  val dependencies: Set[Dependency] = (caseVals.flatMap(_.dependencies) ++ outerDependencies).toSet
  def prefixName(prefix: String): CaseClass = {
    if (prefix nonEmpty) copy(name = s"${prefix}_$name")
    else this
  }
}

object CaseClass {
  def apply(name: String, fields: CaseVal*): CaseClass = new CaseClass(name, Set(), fields)
  def withDependencies(name: String, dependencies: Set[Dependency], fields: CaseVal*): CaseClass = new CaseClass(name, dependencies, fields)
}