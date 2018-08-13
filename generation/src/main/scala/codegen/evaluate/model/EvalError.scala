package codegen.evaluate.model

import codegen.evaluate.{EvalContext, Evaluation}
import codegen.model.{EffectConditional, EffectOperation}

sealed trait EvalError {
  def msg: String
}


case class ConditionKindMissing(condition: EffectConditional[Unit])(implicit ctx: EvalContext) extends EvalError {
  import Evaluation.showAsString

  override def msg = condition.config.get("kind") match {
    case None => "One of the conditions does not have a kind."
    case Some(kind) => s"The context $ctx does not have a kind named ${showAsString(kind)}."
  }
}



case class OperationMissingFromState(operation: EffectOperation[Unit]) extends EvalError {
  override def msg = s"An operation with ID ${operation.id} is included in the evaluation. This ID must either be present as a key in the state, or that operation must be removed."
}