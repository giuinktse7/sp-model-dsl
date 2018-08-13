package codegen.model

import java.util.UUID

import cats.Eq
import codegen.evaluate.SPStateValue
import codegen.internal.Attribute.AttrObject
import codegen.internal.{Attribute, Effect, GeneratedIdentifiable}
import codegen.model.Bool._
import codegen.model.ConditionNode._
import codegen.model.Types.ID
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString}

case class Action(id: ID = ID(), effect: ActionEffect) {
}

object Action {
  implicit final class idToActionSyntax(id: ID) {
    def effect(e: ActionEffect): Action = Action(id, effect = e)
    def none(value: SPStateValue): Action = Action(id, effect = NoEffect(value))
    def inc(n: Int): Action = Action(id, effect = Increment(n))
    def dec(n: Int): Action = Action(id, effect = Decrement(n))
    def assign(id: ID): Action = Action(id, effect = Assign(id))
  }

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

case class EffectConditional[R](
                        proposition: Seq[ConditionNode[R]],
                        actions: List[Action] = List(),
                        config: AttrObject = AttrObject()
                      )(implicit F: Effect[R, EffectConditional]) {
  def useEffect: R = F.effect(this)
  def setConfig(config: AttrObject): EffectConditional[R] = copy(config = config)
  def setActions(actions: Action*): EffectConditional[R] = copy(actions = actions.toList)
}

trait ConditionShape[A, B] {
  def toConditionShape(a: A): B
}


trait ConditionalImplicits {
  implicit val eqInt: Eq[Int] = Eq.fromUniversalEquals
  implicit val eqString: Eq[String] = Eq.fromUniversalEquals
  implicit val eqJsString: Eq[JsString] = Eq.fromUniversalEquals
  implicit val eqJsNumber: Eq[JsNumber] = Eq.fromUniversalEquals
  implicit val eqJsArray: Eq[JsArray] = Eq.fromUniversalEquals
  implicit val eqJsObject: Eq[JsObject] = Eq.fromUniversalEquals
  implicit val eqAttr: Eq[Attribute] = Eq.fromUniversalEquals
  implicit val eqAttrObj: Eq[AttrObject] = Eq.fromUniversalEquals
  implicit def eqIndexedSeq[A <: IndexedSeq[_]]: Eq[A] = (x: A, y: A) => x.equals(y)
  implicit def eqSeq[A <: Seq[_]]: Eq[A] = (x: A, y: A) => x.equals(y)
}

object EqImplicits extends ConditionalImplicits

object EffectConditional extends ConditionalImplicits {
  type Cond = EffectConditional[Unit]

  def apply[R](nodes: ConditionNode[R]*)(implicit F: Effect[R, EffectConditional]): EffectConditional[R] = new EffectConditional(nodes, List(), AttrObject())
  def apply[R](nodes: Seq[ConditionNode[R]], actions: List[Action], config: AttrObject)(implicit F: Effect[R, EffectConditional]): EffectConditional[R] = {
    new EffectConditional(nodes, actions, config)
  }

  object DSL extends ConditionalImplicits {
    // implicit class IdentifiableDSL[]


    implicit class DomainDSL[A, R](a: A)(implicit eqC: Eq[A], F: Effect[R, Bool]) {

      def ===[B](b: B)(implicit shapeB: ConditionShape[B, A], F1: Effect.Partial[R, Equal, A]): Equal[A, R] = {
        Equal(a, shapeB.toConditionShape(b))
      }

/*
      def !==[B](b: B)(implicit shapeB: ConditionShape[B, C], F1: Effect.Partial[R, Equal, C], F2: Effect[R, Not]): Bool[R] = {
        Not(Equal(shapeA.toConditionShape(a), shapeB.toConditionShape(b)))
      }
      */
    }

    implicit class ConditionalDSL[A, B, R](a: A)(implicit F: Effect[R, Bool]) {
      def :=(that: B)(implicit F: Effect.Partial2[R, Definition, A, B]): Definition[A, B, R] = Definition(a, that)
    }
  }

}

sealed trait Bool[R] extends ConditionNode[R] {
  def ||(node: Bool[R])(implicit F: Effect[R, Or]): Bool[R] = Or(this, node)
  def &&(node: Bool[R])(implicit F: Effect[R, And]): Bool[R] = And(this, node)

  def V(node: Bool[R])(implicit F: Effect[R, Or]): Bool[R] = Or(this, node)
  def Λ(node: Bool[R])(implicit F: Effect[R, And]): Bool[R] = And(this, node)

  def eval: Boolean
  def effect: R
}

object Bool {
  def not[R](node: Bool[R])(implicit F: Effect[R, Not]): Bool[R] = Not(node)
  def ¬[R](node: Bool[R])(implicit F: Effect[R, Not]): Bool[R] = Not(node)


  case class And[R](fst: Bool[R], snd: Bool[R])(implicit F: Effect[R, And]) extends Bool[R] {
    def effect: R = F.effect(this)
    override def eval = fst.eval && snd.eval
  }

  case class Or[R](fst: Bool[R], snd: Bool[R])(implicit F: Effect[R, Or]) extends Bool[R] {
    override def eval = fst.eval || snd.eval

    override def effect = F.effect(this)
  }

  case class True[R]()(implicit F: Effect[R, True]) extends Bool[R] {
    override def eval = true

    override def effect: R = F.effect(this)
  }
  case class False[R]()(implicit F: Effect[R, False]) extends Bool[R] {
    override def eval = false

    override def effect: R = F.effect(this)
  }

  case class Equal[A: Eq, R](lhs: A, rhs: A)(implicit F: Effect.Partial[R, Equal, A]) extends Bool[R] {
    override def eval = Eq.eqv(lhs, rhs)
    override def effect = F.effect(this)
  }
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

    def eq[A] = Testable.of[A](_ == _)
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

  case class IdentifiableGuard[B, R](a: Identified, b: B, T: Testable[B])(implicit F: Effect.Partial[R, IdentifiableGuard, B]) extends ConditionNode[R] {
    def test(f: UUID => Any): Boolean = {
      val temp = f(a.id)
      val result = b match {
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
      }

      T.test(result, b)
    }

    override def effect = F.effect(this)
  }

  object IdentifiableGuard {
    class Ops(lhs: Identified) {
      def <[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.lt[B])
      def <=[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.lteq[B])
      def >[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.gt[B])
      def >=[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.gteq[B])
      def ===[B, R](rhs: B)(implicit F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.eq[B])
    }

    implicit def mkOrderingOps(lhs: Identifiable): IdentifiableGuard.Ops = new IdentifiableGuard.Ops(GuardId(lhs))
    implicit def mkOrderingOps(lhs: GeneratedIdentifiable): IdentifiableGuard.Ops = new IdentifiableGuard.Ops(GenGuardId(lhs))
  }

  case class Not[R](bool: Bool[R])(implicit F: Effect[R, Not]) extends Bool[R] {
    override def eval = !bool.eval
    override def effect = F.effect(this)
  }
}

sealed trait ConditionNode[R] {
  def effect: R
}
object ConditionNode {
  def point[A: Eq, R](a: A)(implicit F: Effect.Partial[R, Value, A]): Value[A, R] = Value(a)

  case class Definition[A, B, R](lhs: A, rhs: B)(implicit F: Effect.Partial2[R, Definition, A, B]) extends ConditionNode[R] {
    override def effect = F.effect(this)
  }

  case class Value[A: Eq, R](a: A)(implicit F: Effect.Partial[R, Value, A]) extends ConditionNode[R] {
    def ^==(that: A)(implicit F: Effect.Partial[R, Equal, A]): Bool[R] = Equal(a, that)
    def ^!=(that: A)(implicit F1: Effect.Partial[R, Equal, A], F2: Effect[R, Not]): Bool[R] = Not(Equal(a, that))

    override def effect = F.effect(this)
  }
}