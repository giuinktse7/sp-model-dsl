

import codegen.model._
import IdentifiableGraph.IdentifiableToNodeGraph
import codegen.internal.{Attribute, Effect, Generate, Result}
import codegen.internal.Attribute._
import Generate.Implicits._
import Generate.GenOps
import cats.Eq
import codegen.ExampleModel
import codegen.ExampleModel.TestModel
import codegen.evaluate.SPStateValue.Executing
import codegen.evaluate._
import codegen.evaluate.model.{EvalFailure, EvalSuccess}
import codegen.model.Bool.Equal
import codegen.model.Types.ID
import play.api.libs.json.Json

object Main {
  implicit val genThingShape: ConditionShape[GenThing, Attribute] = _.domain
  implicit val intShape: ConditionShape[Int, Attribute] = AttrInt(_)
  implicit val stringShape: ConditionShape[String, Attribute] = AttrString(_)
  implicit def attrShape[A <: Attribute]: ConditionShape[A, Attribute] = x => x
  implicit def identityShape[A]: ConditionShape[A, A] = x => x


  def main(args: Array[String]): Unit = {
    //create()
    /*
    val subThing = Thing.forGen("subThing", Attribute("domain" -> List("foo", "bar")))
    val condition = Conditional((5 !== 2) && ("foo" === "bar"))
    val someOperation = Operation("someOperation", List(condition))

    val mainThing = Thing.forGen("mainThing", Attribute("someAttr" -> "kalle"))

    val items = mainThing -> (
      someOperation -> subThing
    )

    val model = Model("TestModel", items)

    println(show(model.generated))*/

    // conditionals()
//     create()
    evalTest()
  }

  def show(result: Result): String = {
    Generate.expressionFormatter(result.compile)
  }

/*
  def conditionals(): Unit = {
    val t1 = GenThing("thing", AttrNumber(25))
    val t3 = GenThing("t2", Attribute("domain" -> AttrList(1, 2)))
    val cond1 = t1 === 25 || t1 === 5

    val t2 = GenThing("otherThing", Attribute("status" -> "Running", "value" -> 27))
    val cond2 = t2 === Attribute("status" -> "Running", "value" -> 24) || not(cond1)

    println(t3.domain)
  }
*/

  def evalTest(): Unit = {
    import codegen.internal.Effect.Implicits._
    import codegen.model.EffectConditional.DSL._
    import Action.idToActionSyntax

    val someId = ID()

    val condition = EffectConditional[Unit](5 === 5)
      .setConfig(Attribute(
        "group" -> DefaultGroup.name,
        "kind" -> PostCondition.name
      ))
      .setActions(someId.inc(5))



    val operation = EffectOperation("MyOperation", List(condition))

    val ctx: EvalContext = EvalContext(Set(DefaultGroup))(
      someId -> StateDomain.int(_ < 25)
    )

    val state: SPState = SPState(state = Map(
      operation.id -> Executing,
      someId -> SPStateValue.Value(23)
    ))

    Evaluation.evalOperation(operation, state)(ctx) match {
      case s@EvalSuccess(res, _) => println(s"Result: $res\nLog:\n  ${s.showLog}")
      case EvalFailure(errors) => println(errors)
    }
  }

  def exper(): Unit = {
    import codegen.internal.Effect.Implicits._
    import codegen.model.Bool.IdentifiableGuard.mkOrderingOps

    val m = TestModel()

    val state = Map(m.r1.o1.t1.id -> 5)

    val guard = m.r1.o1.t1 >= m.r1.o1.t1.attributes.domain
    println(guard.test(id => state(id)))
  }

  def create(): Unit = {
    import EffectConditional.DSL._
    import codegen.internal.Effect.Implicits.ForGen._
    // import codegen.internal.Effect.Implicits._

    val r1 = Thing.forGen("r1", Attribute("domain"-> List("foo", "bar")))
    val t1 = Thing.forGen("t1", Attribute("domain"-> 15))


    val k: Equal[Int, Result] = 5 === 2
    val conditionForO1 = EffectConditional((5 === 2) && ("foo" === "bar"))
    val o1 = EffectOperation("o1", List(conditionForO1))

    val conditionForO2 = EffectConditional("k" === "e")
    val o2 = EffectOperation("o2", List(conditionForO2))

    val t2 = Thing.forGen("t2", Attribute("someAttr" -> "kalle"))

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential())

    val items = r1 -> (
      o1 -> (t1, t2),
      o2 -> t2
    )

    val model = Model("TestModel", items)

    println(show(model.generated))
  }

  /*
  def genExample(): Unit = {
    val pointGen = CaseClassDefinition(
      "Point",
      FieldDefinition[Double]("x"),
      FieldDefinition[Double]("y")
    )

    val lineGen = CaseClassDefinition(
      "Line",
      FieldDefinition("from", "Point"),
      FieldDefinition("to", "Point")
    )

    val directory = Paths.get("codegen/generated")

    val files = Stream(lineGen, pointGen) // Stream of Generate[A]
      .map(g => ScalaFile(directory, g)) // Convert each Generate[A] to a ScalaFile[A] to be created in 'directory'
      .flatMap(f => Generate.writeFile(f)) // Write files to disk

    files.compile.drain.unsafeRunSync()
  }
  */
}
