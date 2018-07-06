package codegen.model

sealed trait Proposition

case class AND(props: List[Proposition]) extends Proposition
case class OR(props: List[Proposition]) extends Proposition
case class NOT(p: Proposition) extends Proposition
case object AlwaysTrue extends Proposition
case object AlwaysFalse extends Proposition

sealed trait PropositionEvaluator extends Proposition {
  val left: StateEvaluator
  val right: StateEvaluator
}
case class EQ(left: StateEvaluator, right: StateEvaluator) extends PropositionEvaluator
case class NEQ(left: StateEvaluator, right: StateEvaluator) extends PropositionEvaluator
case class GREQ(left: StateEvaluator, right: StateEvaluator) extends PropositionEvaluator
case class LEEQ(left: StateEvaluator, right: StateEvaluator) extends PropositionEvaluator
case class GR(left: StateEvaluator, right: StateEvaluator) extends PropositionEvaluator
case class LE(left: StateEvaluator, right: StateEvaluator) extends PropositionEvaluator