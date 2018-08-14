package codegen.model

import java.util.UUID

import cats.Eq
import codegen.evaluate.SPStateValue
import codegen.internal.Attribute.AttrObject
import codegen.internal.GeneratedIdentifiable
import codegen.model.Action.IdToActionSyntax
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
/*
  trait Compare[A, B] {
    def compare(a: A, b: B): Int

    def eq(a: A, b: B) = a == b
    def gt(a: A, b: B) = a < b
    def lt[A](a: A, b: B) =
    def gteq[A](a: A, b: B) =
    def lteq[A](a: A, b: B) =
  }
*/
  trait Testable[A] {
    def test(a1: A, a2: A): Boolean
  }

  object Testable {
    def any(f: (Any, Any) => Boolean): Testable[Any] = of[Any](f)
    def of[A](f: (A, A) => Boolean): Testable[A] = f.apply

    // def eq[A] = Testable.of[A](_ == _)
    def eq[A] = Testable.of[A]((a, b) => {
      println(s"$a == $b?")
      a == b
    })
    def gt[A](implicit cmp: Ordering[_ >: A]) = Testable.of[A]((a, b) => cmp.gt(a, b))
    def lt[A](implicit cmp: Ordering[_ >: A]) = Testable.of[A]((a, b) => cmp.lt(a, b))
    def gteq[A](implicit cmp: Ordering[_ >: A]) = Testable.of[A]((a, b) => cmp.gteq(a, b))
    def lteq[A](implicit cmp: Ordering[_ >: A]) = Testable.of[A]((a, b) => cmp.lteq(a, b))
  }

  sealed trait Identified {
    def id: UUID
  }
  case class GenGuardId(obj: GeneratedIdentifiable) extends Identified { val id = obj.id }
  case class GuardId(obj: Identifiable) extends Identified { val id = obj.id }

  case class IdentifiableGuard[B: Writes, R](a: Identified, b: B, T: Testable[R]) extends Bool {
    def test(value: SPStateValue): Boolean = {
      value match {
        case SPStateValue.Value(v) => v match {
          case JsNumber(n) => T.test(n.asInstanceOf[R], b.asInstanceOf[R])
          case JsString(s) => T.test(s.asInstanceOf[R], b.asInstanceOf[R])
          case other => T.test(other.asInstanceOf[R], Json.toJson(b).asInstanceOf[R])
        }
        case other => T.test(other.asInstanceOf[R], b.asInstanceOf[R])
      }
      /*val result = b match {
        case _: BigDecimal =>
           temp match {
            case x: Int => BigDecimal(x).asInstanceOf[B]
            case x: java.lang.Integer => BigDecimal(x).asInstanceOf[B]
            case x: Float => BigDecimal(x.toDouble).asInstanceOf[B]
            case x: Double => BigDecimal(x).asInstanceOf[B]
            case x:  Long => BigDecimal(x).asInstanceOf[B]
            case _ => temp.asInstanceOf[B]
          }
        case _ => temp.asInstanceOf[B]
      }*/
    }
  }

  object IdentifiableGuard {
    class Ops(lhs: Identified) {
      def <[B: Writes](rhs: B)(implicit cmp: Ordering[_ >: B]) = IdentifiableGuard[B, JsValue](lhs, rhs, Testable.lt[JsValue])
      def <=[B: Writes](rhs: B)(implicit cmp: Ordering[_ >: B]) = IdentifiableGuard[B, JsValue](lhs, rhs, Testable.lteq[JsValue])
      def >[B: Writes](rhs: B)(implicit cmp: Ordering[_ >: B]) = IdentifiableGuard[B, JsValue](lhs, rhs, Testable.gt[JsValue])
      def >=[B: Writes](rhs: B)(implicit cmp: Ordering[_ >: B]) = IdentifiableGuard[B, JsValue](lhs, rhs, Testable.gteq[JsValue])
      def ===[B: Writes](rhs: B) = IdentifiableGuard[B, B](lhs, rhs, Testable.eq[B])
    }

    implicit def mkOrderingOps(lhs: Identifiable): IdentifiableGuard.Ops = new IdentifiableGuard.Ops(GuardId(lhs))
    implicit def mkOrderingOps(lhs: GeneratedIdentifiable): IdentifiableGuard.Ops = new IdentifiableGuard.Ops(GenGuardId(lhs))
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