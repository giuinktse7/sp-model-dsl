package codegen

import codegen.definition.CaseVal
import codegen.model.{GenThing, Identifiable, Operation}
import Generate.GenOps
import Generate.Implicits._

trait CaseClassLike[A] {
  def caseVals(a: Namespace[A]): Seq[CaseVal]
}

object CaseClassLike {
  implicit class CaseClassLikeOps[A](val value: Namespace[A]) extends AnyVal {
    def caseVals(implicit c: CaseClassLike[A]): Seq[CaseVal] = c.caseVals(value)
  }

  def apply[A](f: Namespace[A] => Seq[CaseVal]): CaseClassLike[A] = f(_)
  object Implicits {
    implicit val caseClassLikeOperation: CaseClassLike[Operation] = CaseClassLike { namespace =>
      val Namespace(op, space) = namespace
      val conditionalCaseVal = op.conditions.map(_.generated)
      val condValue = s"List[Conditional](${conditionalCaseVal.map(_.result).mkString(", ")})"
      val condDeps = conditionalCaseVal.flatMap(_.dependencies).toSet

      List(
        CaseVal("name", op.name),
        CaseVal.rawQualified("conditions", condValue, "List[Conditional]").addDependencies(condDeps),
        CaseVal("attributes", op.attributes),
        CaseVal("id", op.id)
      )

    }

    implicit val caseClassLikeGenThing: CaseClassLike[GenThing] = CaseClassLike { ns =>
      val Namespace(thing, space) = ns
      val attrCaseVal = thing.attrObject.toGen("attributes", space)
      List(
        CaseVal("name", thing.name),
        attrCaseVal,
        CaseVal("id", thing.id)
      )
    }

    implicit val caseClassLikeIdentifiable: CaseClassLike[Identifiable] = CaseClassLike { ns =>
      val Namespace(value, space) = ns
      value match {
        case o: Operation => Namespace(o, space).caseVals
        case t: GenThing => Namespace(t, space).caseVals
        case x => throw new IllegalArgumentException(s"Can not find a CaseClassLike[_] in scope for $x.")
      }
    }

  }
}