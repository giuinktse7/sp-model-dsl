package codegen.definition

import codegen.Generate
import codegen.Generate._

case class CaseVal(name: String, className: String, value: String)

object CaseVal {
  def apply[A: Generate](name: String, a: A) = new CaseVal(name, a.getClass.getSimpleName, a.generated.result)
  def instance(name: String, className: String) = new CaseVal(name, className, className + "()")
}