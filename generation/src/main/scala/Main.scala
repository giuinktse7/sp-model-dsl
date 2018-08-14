

import java.nio.file.Paths

import codegen.model._
import codegen.internal.{Attribute, Generate, Result, ScalaFile}
import codegen.internal.Attribute._
import Generate.GenOps
import codegen.evaluate.SPStateValue.Executing
import codegen.evaluate._
import codegen.evaluate.model.{EvalFailure, EvalSuccess}
import codegen.generated.{Generated_mainThing, GeneratedModel}
import codegen.internal.definition.{CaseClassDefinition, FieldDefinition}
import codegen.model.Types.ID

object Main {
  def main(args: Array[String]): Unit = {
    import Condition.DSL._
    import IdentifiableGraph.IdentifiableToNodeGraph
    import Generate.Implicits._

    val subThing = Thing.forGen("subThing")
      .setAttributes("domain" -> List("foo", "bar"))

    val condition = Condition((5 !== 2) && ("foo" === "bar"), subThing := 25)
    val someOperation = Operation("someOperation", List(condition))

    val mainThing = Thing.forGen("mainThing")
      .setAttributes("someAttr" -> "kalle", "numberMaybe" -> 255)

    val items = mainThing -> (
      someOperation -> subThing
    )

    val model = Model("TestModel", items)

    val directory = Paths.get("codegen/generated")

    val saveFile = Generate.saveAsFile(directory, "GeneratedModel")(model)

   // saveFile.compile.drain.unsafeRunSync()
    println(s"File saved to $directory\\GeneratedModel.")
//     create()
//     evalTest()
     exper()

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
    import codegen.model.Condition.DSL._
    import Action.IdToActionSyntax
    import SPState.SPStateValueConversion

    val someId = ID()

    val condition = Condition(5 === 5)
      .setConfig(Attribute(
        "group" -> DefaultGroup.name,
        "kind" -> PostCondition.name
      ))
      .setActions(someId.inc(5))

    val operation = Operation("MyOperation", List(condition))

    val ctx: EvalContext = EvalContext(Set(DefaultGroup)).setDomain(
      someId -> StateDomain.int(_ < 25)
    )

    val state: SPState = SPState(state = Map(
      operation.id -> Executing,
      someId -> 18
    ))

    Evaluation.evalOperation(operation, state)(ctx) match {
      case s@EvalSuccess(res, _) => println(s"Result: $res\nLog:\n  ${s.showLog}")
      case EvalFailure(errors) => println(errors)
    }
  }


  def exper(): Unit = {
    import Condition.DSL._
    import codegen.model.Bool.IdentifiableGuard._
    import Action._

    val m = GeneratedModel()

    val ctx: EvalContext = EvalContext(Set(DefaultGroup)).setDomain(
        m.mainThing.someOperation.subThing -> StateDomain.int(_ < 100)
      )

    val condition = Condition(m.mainThing === Executing && m.mainThing.someOperation.subThing > 40).setActions(
      m.mainThing.someOperation.subThing.inc(2)
    ).setConfig(Attribute(
      "group" -> DefaultGroup.name,
      "kind" -> PostCondition.name
    ))

    val op = Operation("An_Operation", conditions = List(condition))

    implicit val state: SPState = SPState(state = Map(
      m.mainThing -> Executing,
      m.mainThing.someOperation.subThing -> 56,
      op -> Executing
    ))

    Evaluation.evalOperation(op, state)(ctx) match {
      case s@EvalSuccess(res, _) => println(s"Result: $res\nLog:\n  ${s.showLog}")
      case EvalFailure(errors) => println(errors.map(_.msg))
    }
  }


  def create(): Unit = {
    import IdentifiableGraph.IdentifiableToNodeGraph
    import Generate.Implicits.genModel
    import Condition.DSL._

    val r1 = Thing.forGen("r1").setAttributes("domain"-> List("foo", "bar"))
    val t1 = Thing.forGen("t1").setAttributes("domain"-> 15)


    val conditionForO1 = Condition((5 === 2) && ("foo" === "bar"))
    val o1 = Operation("o1", List(conditionForO1))

    val conditionForO2 = Condition("k" === "e")
    val o2 = Operation("o2", List(conditionForO2))

    val t2 = Thing.forGen("t2").setAttributes("someAttr" -> "kalle")

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential())

    val items = r1 -> (
      o1 -> (t1, t2),
      o2 -> t2
    )

    val model = Model("TestModel", items)

    println(show(model.generated))
  }




  def genExample(): Unit = {
    import Generate.Implicits._
    import Generate._

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

    val files = fs2.Stream(lineGen, pointGen) // Stream of Generate[A]
      .map(g => ScalaFile(directory, g)) // Convert each Generate[A] to a ScalaFile[A] to be created in 'directory'
      .flatMap(x => Generate.writeFile(x))

    files.compile.drain.unsafeRunSync()
  }

}
