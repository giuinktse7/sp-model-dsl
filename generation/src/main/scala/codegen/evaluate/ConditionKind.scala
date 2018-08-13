package codegen.evaluate

import codegen.model.Types.SPValue
import play.api.libs.json.JsString

sealed trait ConditionKind {
  def abbreviation: String
  def name: String
  def fromString(s: String): Option[ConditionKind] = if(s == abbreviation || s == name) Some(this) else None
}

object ConditionKind {
  def fromString(s: String): Option[ConditionKind] = {
    PreCondition.fromString(s).orElse(
      PostCondition.fromString(s).orElse(
        ResetCondition.fromString(s)
      )
    )
  }

  def fromString(s: SPValue): Option[ConditionKind] = s match {
    case JsString(value) => fromString(value)
    case _ => None
  }
}

case object PreCondition extends ConditionKind {
  val abbreviation = "pre"
  val name = abbreviation + "condition"
}

case object PostCondition extends ConditionKind {
  val abbreviation = "post"
  val name = abbreviation + "condition"
}

case object ResetCondition extends ConditionKind {
  val abbreviation = "reset"
  val name = abbreviation + "condition"
}