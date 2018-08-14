package codegen.model

import java.util.UUID

import codegen.evaluate.SPStateValue
import codegen.internal.Attribute.AttrObject
import codegen.internal.GeneratedIdentifiable
import codegen.model.Bool.StateComparison.Predicate
import codegen.model.Bool._
import codegen.model.ConditionNode._
import codegen.model.Types.ID
import play.api.libs.json._

case class Action(id: ID = ID(), effect: ActionEffect) {
}

object Action {
  implicit final class IdToActionSyntax(id: ID) {
    def effect(e: ActionEffect): Action = Action(id, effect = e)
    def none(value: SPStateValue): Action = Action(id, effect = NoEffect(value))
    def inc(n: Int): Action = Action(id, effect = Increment(n))
    def dec(n: Int): Action = Action(id, effect = Decrement(n))
    def assign(id: ID): Action = Action(id, effect = Assign(id))
  }

  implicit def identifiableToActionSyntax(identifiable: GeneratedIdentifiable): IdToActionSyntax = IdToActionSyntax(identifiable.id)
  implicit def identifiableToIdSyntax[A <: GeneratedIdentifiable](identifiable: A): ID = identifiable.id
  implicit def genIdentifiablePairToIdSyntax[A <: GeneratedIdentifiable](pair: (A, SPStateValue)): (ID, SPStateValue) = (pair._1.id, pair._2)
  implicit def identifiablePairToIdSyntax[A <: Identifiable](pair: (A, SPStateValue)): (ID, SPStateValue) = (pair._1.id, pair._2)
  implicit def genIdentifiablePairToIdSyntax[A <: GeneratedIdentifiable, B: Writes](pair: (A, B)): (ID, SPStateValue) = (pair._1.id, SPStateValue.Value(pair._2))
  implicit def genIdentifiablePairToIdBoolSyntax[A <: GeneratedIdentifiable](pair: (A, SPStateValue => Boolean)): (ID, SPStateValue => Boolean) = (pair._1.id, pair._2)

  def ifNumber(value: SPStateValue)(f: BigDecimal => BigDecimal): SPStateValue = {
    val result = value.toSPValue.collect { case JsNumber(v) => SPStateValue.Value(JsNumber(f(v))) }

    result.getOrElse(value)
  }
}

sealed trait ActionEffect

/**
  * Value that is independent of a state
  */
case class NoEffect(value: SPStateValue) extends ActionEffect
case class Increment(amount: Int) extends ActionEffect
case class Decrement(amount: Int) extends ActionEffect
case class Assign(id: ID) extends ActionEffect

case class Condition(
                        proposition: Seq[ConditionNode],
                        actions: List[Action] = List(),
                        config: AttrObject = AttrObject()
                      ) {
  def setConfig(config: AttrObject): Condition = copy(config = config)
  def setActions(actions: Action*): Condition = copy(actions = actions.toList)
}

object Condition {

  def apply(nodes: ConditionNode*): Condition = new Condition(nodes, List(), AttrObject())
  def apply(nodes: Seq[ConditionNode], actions: List[Action], config: AttrObject): Condition = {
    new Condition(nodes, actions, config)
  }

  object DSL {
    // implicit class IdentifiableDSL[]

    implicit def IdToNodeConversion(id: ID): IdNode = IdNode(id)
    implicit def valueToJsValueConversion[A: Writes](a: A): Value = Value(Json.toJson(a))


    implicit class DomainDSL[A](a: A)(implicit wa: Writes[A]) {
      def ===[B](b: B)(implicit wb: Writes[B]): Equal = Equal(Value(a), b)
      def !==[B](b: B)(implicit wb: Writes[B]): Bool = Not(===(b))
    }

    implicit class DefinitionSyntaxForId(id: ID) {
      def :=[B](b: B)(implicit wb: Writes[B]): Definition = Definition(id, Json.toJson(b))
    }

    implicit class DefinitionSyntaxForIdentifiable(identifiable: Identifiable) {
      def :=[B](b: B)(implicit wb: Writes[B]): Definition = DefinitionSyntaxForId(identifiable.id) := b
    }
  }

}

sealed trait Bool extends ConditionNode {
  def ||(node: Bool): Bool = Or(this, node)
  def &&(node: Bool): Bool = And(this, node)

  def V(node: Bool): Bool = Or(this, node)
  def Λ(node: Bool): Bool = And(this, node)
}

object Bool {
  def not(node: Bool): Bool = Not(node)
  def ¬(node: Bool): Bool = Not(node)


  case class And(fst: Bool, snd: Bool) extends Bool

  case class Or(fst: Bool, snd: Bool) extends Bool

  case object True extends Bool
  case object False extends Bool

  case class Equal(lhs: ConditionNode, rhs: ConditionNode) extends Bool

  sealed trait Identified {
    def id: UUID
  }
  case class GenGuardId(obj: GeneratedIdentifiable) extends Identified { val id = obj.id }
  case class GuardId(obj: Identifiable) extends Identified { val id = obj.id }

  sealed trait StateComparison extends Bool {
    def id: ID
    def rhs: SPStateValue
    def compare(f: ID => SPStateValue): Boolean
    def compareOption(f: ID => Option[SPStateValue]): Boolean = f(id).exists(lhs => compare(_ => lhs))


    def values(lhs: SPStateValue): Option[(JsValue, JsValue)] = StateComparison.values(lhs, rhs)
    def numbers(lhs: SPStateValue, f: (BigDecimal, BigDecimal) => Boolean): Option[Boolean] = StateComparison.numbers(lhs, rhs)(f)
    def strings(lhs: SPStateValue, f: (String, String) => Boolean): Option[Boolean] = StateComparison.strings(lhs, rhs)(f)
  }

  object StateComparison {
    trait Predicate[F[_]] {
      def test[A](l: A, r: A)(implicit F: F[A]): Boolean
    }

    def orderComp(lhs: SPStateValue, rhs: SPStateValue)(p: Predicate[Ordering]): Boolean = {
      numbers(lhs, rhs)(p.test)
        .orElse(strings(lhs, rhs)(p.test))
        .getOrElse(false)
    }

    def values(lhs: SPStateValue, rhs: SPStateValue): Option[(JsValue, JsValue)] = (lhs, rhs) match {
      case (SPStateValue.Value(v1), SPStateValue.Value(v2)) => Some((v1, v2))
      case _ => None
    }

    def numbers(lhs: SPStateValue, rhs: SPStateValue)(f: (BigDecimal, BigDecimal) => Boolean): Option[Boolean] = values(lhs, rhs).flatMap {
      case (JsNumber(n1), JsNumber(n2)) => Some(f(n1, n2))
      case _ => None
    }

    def strings(lhs: SPStateValue, rhs: SPStateValue)(f: (String, String) => Boolean): Option[Boolean] = values(lhs, rhs).flatMap {
      case (JsString(s1), JsString(s2)) => Some(f(s1, s2))
      case _ => None
    }

    class Ops(id: ID) {
      def >(rhs: SPStateValue) = GT(id, rhs)
      def >=(rhs: SPStateValue) = GT(id, rhs)
      def <(rhs: SPStateValue) = GT(id, rhs)
      def <=(rhs: SPStateValue) = GT(id, rhs)
      def ===(rhs: SPStateValue) = EQ(id, rhs)
    }

    implicit def toStateValue[A: Writes](a: A): SPStateValue = SPStateValue(Json.toJson(a))
    implicit def mkOrderingOps(lhs: Identifiable): Ops = new Ops(lhs.id)
    implicit def mkOrderingOps(lhs: GeneratedIdentifiable): Ops = new Ops(lhs.id)
  }

  trait OrderComparison extends StateComparison {
    def compare(f: ID => SPStateValue): Boolean = StateComparison.orderComp(f(id), rhs)(p)
    val p: Predicate[Ordering]
  }

  case class GT(id: ID, rhs: SPStateValue) extends OrderComparison {
    lazy val p: Predicate[Ordering] = new Predicate[Ordering] {
      override def test[A](l: A, r: A)(implicit F: Ordering[A]) = F.gt(l, r)
    }
  }

  case class LT(id: ID, rhs: SPStateValue) extends OrderComparison {
    lazy val p: Predicate[Ordering] = new Predicate[Ordering] {
      override def test[A](l: A, r: A)(implicit F: Ordering[A]) = F.lt(l, r)
    }
  }

  case class GTE(id: ID, rhs: SPStateValue) extends OrderComparison {
    lazy val p: Predicate[Ordering] = new Predicate[Ordering] {
      override def test[A](l: A, r: A)(implicit F: Ordering[A]) = F.gteq(l, r)
    }
  }

  case class LTE(id: ID, rhs: SPStateValue) extends OrderComparison {
    lazy val p: Predicate[Ordering] = new Predicate[Ordering] {
      override def test[A](l: A, r: A)(implicit F: Ordering[A]) = F.lteq(l, r)
    }
  }

  case class EQ(id: ID, rhs: SPStateValue) extends StateComparison {
    override def compare(f: ID => SPStateValue) = f(id) == rhs
  }

  case class Not(bool: Bool) extends Bool
}

sealed trait ConditionNode

object ConditionNode {
  case class IdNode(id: ID) extends ConditionNode
  case class Definition(lhs: IdNode, rhs: JsValue) extends ConditionNode

  case class Value(a: JsValue) extends ConditionNode {
    def ^==(that: JsValue): Bool = Equal(this, Value(that))
    def ^!=(that: JsValue): Bool = Not(Equal(this, Value(that)))
  }

  object Value {
    def apply[A: Writes](a: A): Value = Value(Json.toJson(a))
  }
}