package codegen.evaluate.model

import cats.data.NonEmptyList
import codegen.evaluate.Evaluation


sealed trait EvalResult {
  def isSuccess: Boolean

  def ++(other: EvalResult): EvalResult = (this, other) match {
    case (EvalSuccess(r1, l1), EvalSuccess(r2, l2)) => EvalSuccess(r1 && r2, l1 ::: l2)
    case (f1@EvalFailure(_), f2@EvalFailure(_)) => f1.combine(f2)
    case (_, f@EvalFailure(_)) => f
    case (f@EvalFailure(_), _) => f
  }
}

case class EvalFailure(errors: NonEmptyList[EvalError]) extends EvalResult {
  def +(e: EvalError) = copy(errors = e :: errors)
  override def isSuccess = false

  override def toString = "Failure:\t\n" + errors.toList.mkString("\t\n")

  def combine(that: EvalFailure) = copy(errors = errors.concatNel(that.errors))
}

object EvalFailure {
  def apply(evalError: EvalError): EvalFailure = EvalFailure(NonEmptyList.of(evalError))
}

case class EvalSuccess(result: Boolean, log: List[LogMessage] = Nil) extends EvalResult {
  val isSuccess = true

  def showLog: String = {
    log.map(entry => s"- ${entry.msg}").mkString("\n\t")
  }
}

object EvalSuccess {
  def fromLogger(logger: Evaluation.Logger[Boolean]): EvalSuccess = EvalSuccess(logger.value, logger.written.toList)
}