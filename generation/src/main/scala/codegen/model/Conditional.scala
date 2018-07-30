package codegen.model

import codegen.Attribute.AttrObject
import codegen.{Generate, Result}
import codegen.Generate._

case class Conditional(
                      proposition: Bool,
                      actions: List[Int] = List(), // TODO Change Int to Action
                      config: AttrObject = AttrObject()
                      )

object Test {

  implicit class ConditionalDSL[A: Generate](a: A) {
    def ^==(that: A): Bool = Equal(a, that)
    def ^!=(that: A): Bool = Not(Equal(a, that))
  }
}

sealed trait Bool {
  def gen: Result

  def &&(node: Bool): Bool = And(this, node)
  def V(node: Bool): Bool = Or(this, node)
  def Λ(node: Bool): Bool = And(this, node)
}

object Bool {
  def ¬(node: Bool): Bool = Not(node)
}

sealed trait Node

object Node {
  def point[A: Generate](a: A): Value[A] = Value(a)
}

case class Value[A: Generate](a: A) extends Node {
  def ^==(that: A): Bool = Equal(a, that)
  def ^!=(that: A): Bool = Not(Equal(a, that))
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
