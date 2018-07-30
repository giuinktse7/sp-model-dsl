package codegen

import codegen.definition.CaseClass
import utest._

object GenerateTest extends TestSuite {
  override def tests: Tests = Tests {
    'fold - {
      "Sorts correctly" - {
        val case1 = CaseClassDependency(CaseClass("a"))
        val case2 = CaseClassDependency(CaseClass("b"))
        val case3 = CaseClassDependency(CaseClass("c"))
        val case4 = CaseClassDependency(CaseClass("d"))
        val case5 = CaseClassDependency(CaseClass("e"))
        val case6 = CaseClassDependency(CaseClass("f"))


        assert(
          Dependency.sortByPriority(Seq(case2, case1, case5, case4, case6, case3)) ==
          Seq(case1, case2, case3, case4, case5, case6)
        )
      }
    }
  }
}
