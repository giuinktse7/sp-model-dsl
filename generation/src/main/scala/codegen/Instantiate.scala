package codegen

import codegen.model._
import Generate.GenOps
import codegen.definition.{CaseClass, CaseVal}
import Generate.Implicits._

sealed trait Instantiate[A] {
  protected def toInstance: Namespace[A] => Instance

  def instance(in: Namespace[A]): Instance = toInstance(in)
}

case class Instance(result: Result, typeQualifier: String) {
  def asCaseVal(name: String): CaseVal = CaseVal.rawQualified(name, result.result, typeQualifier).addDependencies(result.dependencies)
}

case class Namespace[A](value: A, namespace: String = "")

object Namespace {
  def empty[A](a: A): Namespace[A] = Namespace(a)
}

object Instantiate {
  def apply[A](implicit instance: Instantiate[A]): Instantiate[A] = instance
  def apply[A](f: Namespace[A] => Instance): Instantiate[A] = new Instantiate[A] {
      override protected def toInstance: Namespace[A] => Instance = f
  }

  def local[A](f: A => Instance): Instantiate[A] = apply(namespace => f(namespace.value))

  implicit class NamespacedInstanceOps[A](val value: Namespace[A]) extends AnyVal {
    def instance(implicit in: Instantiate[A]): Instance = in.instance(value)
  }

  implicit class InstanceOps[A](val value: A) extends AnyVal {
    def localInstance(implicit in: Instantiate[A]): Instance = in.instance(Namespace(value))
  }

  private def prefixName(prefix: String)(name: String): String = s"${prefix}_$name"

  implicit val instantiateOperation: Instantiate[Operation] = local { operation =>
    Instance(operation.generated, "Operation")
  }

  implicit val caseClassInstance: Instantiate[CaseClass] = local { caseClass =>
    val res = Result(s"${caseClass.name}()", caseClass.dependencies + CaseClassDependency(caseClass))
    Instance(res, caseClass.name)
  }

  implicit val conditionalInstance: Instantiate[Conditional] = local { conditional =>
    Instance(conditional.generated, "Conditional")
  }

  implicit val instantiateGenThing: Instantiate[GenThing] = Instantiate { ns =>
    Generate.Implicits.genThingCaseClass(ns).localInstance
  }

  implicit val instantiateIdentifiable: Instantiate[Identifiable] = Instantiate { namespace =>
    val Namespace(value, space) = namespace
    value match {
      case x: Operation => Namespace(x, space).instance
      case x: GenThing => Namespace(x, space).instance
      case x => throw new IllegalArgumentException(s"Can not find an Instantiate[_] in scope for $x.")
    }
  }
}