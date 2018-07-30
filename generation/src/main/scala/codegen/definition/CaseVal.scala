package codegen.definition

import codegen.{Dependency, Generate}
import codegen.Generate._

case class CaseVal(name: String, className: String, value: String, dependencies: Set[Dependency] = Set()) {
  def addDependencies(dependencies: Set[Dependency]): CaseVal = copy(dependencies = this.dependencies ++ dependencies)
  def addDependencies(dependencies: Dependency*): CaseVal = copy(dependencies = this.dependencies ++ dependencies)
}

object CaseVal {
  def qualified[A: Generate](name: String, a: A, qualifier: String) = new CaseVal(name, qualifier, a.generated.result)
  def rawQualified(name: String, value: String, qualifier: String) = new CaseVal(name, qualifier, value)
  def apply[A: Generate](name: String, a: A) = {
    val gen = a.generated
    new CaseVal(name, a.getClass.getSimpleName, gen.result, gen.dependencies)
  }

  /**
    * Creates a default CaseVal with value as default instance of the case class.
    * @param name Name of the case value
    * @param className Class of the case value
    */
  def defaultInstance(name: String, className: String) = new CaseVal(name, className, className + "()")
}