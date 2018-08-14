package codegen.evaluate

import codegen.evaluate.SPStateValue.Value
import codegen.model.Types.{ID, SPValue}
import play.api.libs.json.JsString

trait ContextGroup {
  def name: String

  def is(value: Value): Boolean = is(value.value)

  def is(value: SPValue): Boolean = value match {
    case JsString(s) => is(s)
    case _ => false
  }

  def is(s: String): Boolean = name == s
}

case object DefaultGroup extends ContextGroup {
  val name = "DefaultGroup"
}

trait DefinitionContext {
  def kinds: Set[ConditionKind]
  def hasKind(value: SPValue): Boolean = value match {
    case JsString(s) => hasKind(s)
    case _ => false
  }

  def hasKind(value: String): Boolean

}

case class EvalContext(
                        groups: Set[ContextGroup],
                        domain: StateDomain
                        // defs: OperationStateDefinition = TwoStateDefinition
                      ) extends DefinitionContext {
  override def kinds = Set(PreCondition, PostCondition, ResetCondition)

  override def hasKind(value: String): Boolean = ConditionKind.fromString(value).exists(kinds.contains)

  def setDomain(newDomain: (ID, SPStateValue => Boolean)*): EvalContext = copy(domain = StateDomain(newDomain.toMap))
}

object EvalContext {
  def apply(groups: Set[ContextGroup]): EvalContext = EvalContext(groups, StateDomain())
}