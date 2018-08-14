package codegen.evaluate

import codegen.evaluate.SPStateValue._
import codegen.evaluate.model._
import codegen.internal.Attribute
import codegen.internal.Attribute.AttrString
import codegen.model.Bool.{EQ, OrderComparison, StateComparison}
import codegen.model.Types.SPValue
import codegen.model._
import play.api.libs.json.{Json, Writes}

sealed trait SPStateValue {
  def toSPValue: Option[SPValue] = this match {
    case Value(v) => Some(v)
    case _ => None
  }
}


object SPStateValue {
  sealed trait OperationMode extends SPStateValue

  case object Init extends OperationMode
  case object Executing extends OperationMode
  case object Finished extends OperationMode
  case class Value(value: SPValue) extends SPStateValue {
    // def apply(): SPValue =
  }

  def apply(value: SPValue): Value = Value(value)

  object Value {
    def apply[A: Writes](a: A): Value = Value(Json.toJson(a))
  }
}


object Evaluation {
  import cats._
  import cats.data._
  import cats.implicits._

  type Validate[A] = ValidatedNel[EvalError, A]
  type Logger[V] = Writer[Vector[LogMessage], V]
  type Cond = Condition


  def partitionInto[A, B, C](as: List[A])(f: A => Either[B, C]): (List[B], List[C]) = {
    as.foldRight((List[B](), List[C]())) { case (next, (bs, cs)) =>
      f(next) match {
        case Left(b) => (b :: bs, cs)
        case Right(c) => (bs, c :: cs)
      }
    }
  }

  object Infer {
    def sequence[G[_], F[_], A](xs: F[G[A]])(implicit t: Traverse[F], app: Applicative[G]): G[F[A]] = xs.sequence
  }

  def evalOperation(operation: Operation, state: SPState)(implicit ctx: EvalContext): EvalResult = {
    validateEval(operation, state) match {
      case Validated.Valid(logger) => EvalSuccess.fromLogger(logger)
      case Validated.Invalid(es) => EvalFailure(es)
    }
  }

  def validateEval(operation: Operation, state: SPState)(implicit ctx: EvalContext): Validate[Logger[Boolean]] = {
    state.get(operation.id).fold(OperationMissingFromState(operation).invalidNel[Logger[Boolean]]: Validate[Logger[Boolean]]) {
      case Finished => true.pure[Logger].pure[Validate]
      case _: OperationMode =>

        val contextChecks = operation.conditions.map(c => inContextWriter(c))

        Infer.sequence(contextChecks).map { values => {
          val log = values.foldLeft(Vector[Cond]().pure[Logger]) { (w, next) =>
            next match {
              case Left(err) => w.tell(Vector(err))
              case Right(v) => w.map(_ :+ v)
            }
          }

          // TODO Should we not require all conditions to be included in the domain?
          log.flatMap { values =>
            val inDomain = values.map(c => {
              val in = conditionInDomain(c, ctx.domain, state)
              val p = in.map(if (_) Some(c) else None)
              p
            })

            val seq = Infer.sequence(inDomain)
            val allInDomain = seq.value.forall(_.nonEmpty)

            if (!allInDomain) seq.map(_ => false)
            else seq
              .map(_.flatten)
              .map(_.map(evalCondition(_, state)).forall(identity))
          }
        }
          }

      case Value(_) => false.pure[Logger].pure[Validate] // TODO Handle this, what to do?
    }
  }

  def evalStateComparison(comp: StateComparison, s: SPState): Boolean = comp match {
    case ord: OrderComparison => ord.compareOption(s.get)
    case eq: EQ => eq.compareOption(s.get)
  }

  def evalBool(bool: Bool, s: SPState): Boolean = bool match {
    case comp: StateComparison => evalStateComparison(comp, s)
    case Bool.And(fst, snd) => evalBool(fst, s) && evalBool(snd, s)
    case Bool.Or(fst, snd) => evalBool(fst, s) || evalBool(snd, s)
    case Bool.Equal(lhs, rhs) => (lhs, rhs) match {
      case (b1: Bool, b2: Bool) => evalBool(b1, s) == evalBool(b2, s)
      case (a, b) => a == b
    }
    case Bool.Not(expr) => !evalBool(expr, s)
    case Bool.False => false
    case Bool.True => true
  }

  def evalCondition(c: Cond, state: SPState): Boolean = {
    import ConditionNode.{Definition, Value, IdNode}
    c.proposition.forall {
      case bool: Bool => evalBool(bool, state)
      case Definition(_, _) => true
      case Value(_) => true
      case IdNode(_) => true
    }
  }

  def kind(mode: OperationMode): ConditionKind = mode match {
    case Init => PreCondition
    case Executing => PostCondition
    case Finished => ResetCondition
  }


  private def hasValidGroup(condition: Cond)(implicit ctx: EvalContext): Boolean = {
    condition.config.get("group").map(_.toSPValue).exists(v => ctx.groups.exists(_.is(v)))
  }

  private def hasValidKind(condition: Cond)(implicit ctx: EvalContext): Boolean = {
    condition.config.get("kind").map(_.toSPValue).exists(v => ctx.hasKind(v))
  }
  private def inContextWriter(condition: Cond)(implicit ctx: EvalContext): Validate[Either[LogMessage, Cond]] = {
    val validGroup = if (hasValidGroup(condition)) Right(condition) else Left(ConditionGroupMissing(condition))

    if (hasValidKind(condition)) validGroup.validNel else ConditionKindMissing(condition).invalidNel
  }

  /**
    * Checks whether applying the action will keep the value within the state domain
    * Returns true if the applying the action provides a value within the domain, and false otherwise.
    */
  def isValidAction(action: Action, domain: StateDomain, state: SPState): Boolean = {
    val next = action.effect match {
      case NoEffect(v) => v
      case Assign(id) => state.state(id)
      case Increment(amount) => Action.ifNumber(state.state(action.id)) { _ + amount }
      case Decrement(amount) => Action.ifNumber(state.state(action.id)) { _ - amount }
    }

    domain.test(action.id, next)
  }

  def conditionInDomain(c: Cond, domain: StateDomain, state: SPState): Logger[Boolean] = {
    val (_, out) = c.actions.partition(isValidAction(_, domain, state))
    out match {
      case Nil => true.pure[Logger]
      case h :: t => Writer(Vector(ValuesNotInDomain(c, NonEmptyList.of(h).concat(t))), false)

    }
  }

  def showAsString(attr: Attribute): String = attr match {
    case AttrString(s) => s
    case _ => attr.toString
  }
}
