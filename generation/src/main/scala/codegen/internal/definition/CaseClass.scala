package codegen.internal.definition

import codegen.internal.Dependency


case class CaseClass(name: String, outerDependencies: Set[Dependency], caseVals: Seq[CaseVal], extending: List[String] = List()) {
  val dependencies: Set[Dependency] = (caseVals.flatMap(_.dependencies) ++ outerDependencies).toSet
  def prefixName(prefix: String): CaseClass = {
    if (prefix.nonEmpty) copy(name = s"${prefix}_$name")
    else this
  }

  def genExtends: String = extending.distinct match {
    case Nil => ""
    case h :: Nil => s"extends $h"
    case h :: t => s"extends $h${t.foldLeft("")((a, b) => s"$a with $b")}"
  }
}

object CaseClass {
  def apply(name: String, fields: CaseVal*): CaseClass = new CaseClass(name, Set(), fields)
  def withDependencies(name: String, dependencies: Set[Dependency], fields: CaseVal*): CaseClass = new CaseClass(name, dependencies, fields)
}