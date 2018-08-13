package codegen.evaluate

import codegen.model.Types.ID
import play.api.libs.json.JsNumber

case class StateDomain(variables: Map[ID, SPStateValue => Boolean] = Map()) {
  def +(id: ID, domain: SPStateValue => Boolean): StateDomain = copy(variables = variables+ (id -> domain))
  def test(id: ID, value: SPStateValue): Boolean = variables.get(id).forall(f => f(value))
}

object StateDomain {
  private def numberDomain[N](p: N => Boolean)(f: BigDecimal => N): SPStateValue => Boolean = {
    case SPStateValue.Value(value) => value match {
      case JsNumber(x) => p(f(x))
      case _ => false
    }
    case _ => false
  }

  def int(p: Int => Boolean): SPStateValue => Boolean = numberDomain(p)(_.toInt)
  def float(p: Float => Boolean): SPStateValue => Boolean = numberDomain(p)(_.toFloat)
  def double(p: Double => Boolean): SPStateValue => Boolean = numberDomain(p)(_.toInt)
  def long(p: Long => Boolean): SPStateValue => Boolean = numberDomain(p)(_.toLong)
}
