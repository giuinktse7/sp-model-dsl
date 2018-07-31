package codegen

import codegen.model._
import Generate.Implicits.{genOperation, genConditional}
import Generate.GenOps
import codegen.definition.{CaseClass, CaseVal}

sealed trait Instantiate[A] {
  protected def toInstance: A => Instance

  def instance(in: A): Instance = toInstance(in)
}

case class Instance(result: Result, typeQualifier: String) {
  def asCaseVal(name: String): CaseVal = CaseVal.rawQualified(name, result.result, typeQualifier).addDependencies(result.dependencies)
}

object Instantiate {
  def apply[A](implicit instance: Instantiate[A]): Instantiate[A] = instance
  def apply[A](f: A => Instance): Instantiate[A] = new Instantiate[A] {
      override protected def toInstance: A => Instance = f
  }

  def withoutQualifier[A](f: A => Result): Instantiate[A] = apply(x => Instance(f(x), ""))

  implicit class InstanceOps[A](val value: A) extends AnyVal {
    def instance(implicit in: Instantiate[A]): Instance = in.instance(value)
  }

  private def prefixName(prefix: String)(name: String): String = s"${prefix}_$name"

  implicit val instantiateNameSpacedIdentifiable: Instantiate[NamespacedIdentifiable] = Instantiate { id =>
    val res: Identifiable = id.identifiable match {
      case x: Operation => x.copy(name = id.name)
      case x: GenThing => x.copy(name = id.name)
      case x => throw new IllegalArgumentException(s"Can not find an Instantiate[_] in scope for $x.")
    }

    res.instance
  }

  implicit val instantiateOperation: Instantiate[Operation] = Instantiate { operation =>
    Instance(operation.generated, "Operation")
  }

  def getCaseClassInstance(namespace: String): Instantiate[CaseClass] = Instantiate { caseClass =>
    val res = Result(s"${caseClass.name}()", caseClass.dependencies + CaseClassDependency(caseClass))
    Instance(res, caseClass.name)
  }

  implicit val caseClassInstance: Instantiate[CaseClass] = Instantiate { caseClass =>
    val res = Result(s"${caseClass.name}()", caseClass.dependencies + CaseClassDependency(caseClass))
    Instance(res, caseClass.name)
  }

  implicit val conditionalInstance: Instantiate[Conditional] = Instantiate { cond =>
    val gen = cond.generated

    Instance(gen, "Conditional")
  }

  implicit val instantiateGenThing: Instantiate[GenThing] = Instantiate { thing =>
    Generate.Implicits.genThingCaseClass(thing).instance
  }

  implicit val instantiateIdentifiable: Instantiate[Identifiable] = Instantiate { id =>
    id match {
      case x: Operation => x.instance
      case x: GenThing => x.instance
      case x => throw new IllegalArgumentException(s"Can not find an Instantiate[_] in scope for $x.")
    }
  }
}