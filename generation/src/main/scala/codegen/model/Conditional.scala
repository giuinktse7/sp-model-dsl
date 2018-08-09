package codegen.model

import cats.Eq
import codegen.internal.Attribute.AttrObject
import codegen.internal.{Attribute, Effect}
import codegen.model.Bool._
import codegen.model.ConditionNode._
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString}

import scala.util.Try

case class Conditional[R](
                        proposition: Seq[ConditionNode[R]],
                        actions: List[Int] = List(), // TODO Change Int to Action
                        config: AttrObject = AttrObject()
                      )(implicit F: Effect[R, Conditional]) {
  def useEffect: R = F.effect(this)
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

object Conditional extends ConditionalImplicits {
  type Cond = Conditional[Unit]

  def apply[R](nodes: ConditionNode[R]*)(implicit F: Effect[R, Conditional]): Conditional[R] = new Conditional(nodes, List(), AttrObject())
  def apply[R](nodes: Seq[ConditionNode[R]], actions: List[Int], config: AttrObject)(implicit F: Effect[R, Conditional]): Conditional[R] = {
    new Conditional(nodes, actions, config)
  }

  object DSL extends ConditionalImplicits {
    // implicit class IdentifiableDSL[]

    implicit class DomainDSL[A, C, R](a: A)(implicit shapeA: ConditionShape[A, C], eqC: Eq[C], F: Effect[R, Bool]) {

      def ===[B: Eq](b: B)(implicit shapeB: ConditionShape[B, C], F1: Effect.Partial[R, Equal, C]): Bool[R] = {
        Equal(shapeA.toConditionShape(a), shapeB.toConditionShape(b))
      }
      def !==[B: Eq](b: B)(implicit shapeB: ConditionShape[B, C], F1: Effect.Partial[R, Equal, C], F2: Effect[R, Not]): Bool[R] = {
        Not(Equal(shapeA.toConditionShape(a), shapeB.toConditionShape(b)))
      }
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

  case class Equal[A: Eq, R](lhs: A, rhs: A)(implicit F: Effect.Partial[R, Equal, A]) extends Bool[R] with Testable[A] {
    override def eval = Eq.eqv(lhs, rhs)

    override def effect = F.effect(this)

    override def test(a1: A, a2: A) = a1 == a2
  }

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



  case class IdentifiableGuard[B, R](a: Identifiable, b: B, T: Testable[B])(implicit F: Effect.Partial[R, IdentifiableGuard, B]) extends ConditionNode[R] {
    def test(f: Identifiable => Any): Boolean = Try(T.test(f(a).asInstanceOf[B], b)).getOrElse(false)

    override def effect = F.effect(this)
  }

  object IdentifiableGuard {
    class Ops(lhs: Identifiable) {
      def <[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.lt[B])
      def <=[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.lteq[B])
      def >[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.gt[B])
      def >=[B, R](rhs: B)(implicit cmp: Ordering[_ >: B], F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.gteq[B])
      def ===[B, R](rhs: B)(implicit F: Effect.Partial[R, IdentifiableGuard, B]) = IdentifiableGuard(lhs, rhs, Testable.eq[B])
    }

    implicit def mkOrderingOps(lhs: Identifiable): IdentifiableGuard.Ops = new IdentifiableGuard.Ops(lhs)
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