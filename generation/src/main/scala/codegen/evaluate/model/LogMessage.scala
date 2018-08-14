package codegen.evaluate.model

import cats.data.NonEmptyList
import codegen.evaluate.{EvalContext, Evaluation}
import codegen.model.{Action, Condition}

sealed trait LogMessage {
  def msg: String
}

case class ValuesNotInDomain(condition: Condition, actions: NonEmptyList[Action]) extends LogMessage {
  private def showActions: String = actions.toList.mkString("\n\t\t")
  def msg: String = s"A condition has the following actions that result in a value outside of the valid domain:\n\t$showActions"
}

case class ConditionGroupMissing(condition: Condition) extends LogMessage {
  import Evaluation.showAsString

  override def msg = condition.config.get("group") match {
    case None => "One of the conditions does not have a group. This condition will not be evaluated."
    case Some(group) => s"The context does not have a group named ${showAsString(group)}."
  }
}