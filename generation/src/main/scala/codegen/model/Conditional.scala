package codegen.model

import codegen.Attribute.AttrObject
import codegen.{Generate, Result}
import codegen.Generate._

case class Conditional(
                        proposition: Seq[ConditionNode],
                        actions: List[Int] = List(), // TODO Change Int to Action
                        config: AttrObject = AttrObject()
                      )

object Conditional {
  def apply(nodes: ConditionNode*): Conditional = new Conditional(nodes, List(), AttrObject())
  def apply(nodes: Seq[ConditionNode], actions: List[Int], config: AttrObject): Conditional = {
    new Conditional(nodes, actions, config)
  }
}

object Test {

  implicit class ConditionalDSL[A: Generate](a: A) {
    def ===(that: A): Bool = Equal(a, that)
    def !==(that: A): Bool = Not(Equal(a, that))

    // TODO
    def :=(that: A): Definition[A] = ???
  }
}

sealed trait Bool extends ConditionNode {
  def &&(node: Bool): Bool = And(this, node)
  def V(node: Bool): Bool = Or(this, node)
  def Λ(node: Bool): Bool = And(this, node)
}

object Bool {
  def ¬(node: Bool): Bool = Not(node)
}

sealed trait ConditionNode {
  def gen: Result
}

object ConditionNode {
  def point[A: Generate](a: A): Value[A] = Value(a)
}

case class Value[A: Generate](a: A) extends ConditionNode {
  def gen: Result = {
    val generated = a.generated
    generated.copy(result = s"Value(${generated.result})")
  }

  def ^==(that: A): Bool = Equal(a, that)
  def ^!=(that: A): Bool = Not(Equal(a, that))
}

case class Definition[A: Generate](a: A) extends ConditionNode {
  def gen: Result = {
    val generated = a.generated
    generated.copy(result = s"Definition(${generated.result})")
  }
}

case class And(fst: Bool, snd: Bool) extends Bool {
  override def gen: Result = {
    val (resA, depA) = fst.gen.tupled
    val (resB, depB) = snd.gen.tupled

    Result(s"Or($resA, $resB)", depA ++ depB)
  }
}

case class Or(fst: Bool, snd: Bool) extends Bool {
  override def gen: Result = {
    val (resA, depA) = fst.gen.tupled
    val (resB, depB) = snd.gen.tupled

    Result(s"Or($resA, $resB)", depA ++ depB)
  }
}

case object True extends Bool {
  override def gen: Result = Result("True")
}
case object False extends Bool {
  override def gen: Result = Result("False")
}

case class Equal[A: Generate](fst: A, snd: A) extends Bool {
  override def gen: Result = {
    val (resA, depA) = fst.generated.tupled
    val (resB, depB) = snd.generated.tupled

    Result(s"Equal($resA, $resB)", depA ++ depB)
  }
}
case class Not(bool: Bool) extends Bool {
  override def gen: Result = {
    val (res, deps) = bool.gen.tupled
    Result(s"Not($res)", deps)
  }
}
