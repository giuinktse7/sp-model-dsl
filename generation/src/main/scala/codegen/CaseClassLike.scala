package codegen

import codegen.definition.CaseVal
import codegen.model.{GenThing, Identifiable, Operation, Struct}
import Generate.GenOps
import Generate.Implicits._

trait CaseClassLike[A] {
  def caseVals(a: A): Seq[CaseVal]
}

object CaseClassLike {
  implicit class CaseClassLikeOps[A](val value: A) extends AnyVal {
    def caseVals(implicit c: CaseClassLike[A]): Seq[CaseVal] = c.caseVals(value)
  }

  def apply[A](f: A => Seq[CaseVal]): CaseClassLike[A] = f(_)
  object Implicits {
    implicit val caseClassLikeOperation: CaseClassLike[Operation] = CaseClassLike { op =>
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

    implicit val caseClassLikeGenThing: CaseClassLike[GenThing] = CaseClassLike { thing =>
      val attrCaseVal = thing.attrObject.toGen("attributes")
      List(
        CaseVal("name", thing.name),
        attrCaseVal,
        CaseVal("id", thing.id)
      )
    }

    implicit val caseClassLikeStruct: CaseClassLike[Struct] = CaseClassLike { struct =>
      List(
        CaseVal("name", struct.name),
        CaseVal("attributes", struct.attributes),
        CaseVal("id", struct.id)
      )
    }

    implicit val caseClassLikeIdentifiable: CaseClassLike[Identifiable] = CaseClassLike {
      case o: Operation => o.caseVals
      case s: Struct => s.caseVals
      case t: GenThing => t.caseVals
      case x => throw new IllegalArgumentException(s"Can not find a CaseClassLike[_] in scope for $x.")
    }
  }
}