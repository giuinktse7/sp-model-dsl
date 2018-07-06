package codegen.model

import Types._

sealed trait StateEvaluator
sealed trait StateUpdater

object StateEvaluator {
  implicit def idToSE(id: ID): SVIDEval = SVIDEval(id)
  implicit def strToSE(value: String): ValueHolder = ValueHolder(SPValue(value))
  implicit def intToSE(value: Int): ValueHolder = ValueHolder(SPValue(value))
}

case class SVIDEval(id: ID) extends StateEvaluator
case class ValueHolder(v: SPValue) extends StateEvaluator with StateUpdater

case class INCR(n: Int) extends StateUpdater
case class DECR(n: Int) extends StateUpdater
case class ASSIGN(id: ID) extends StateUpdater