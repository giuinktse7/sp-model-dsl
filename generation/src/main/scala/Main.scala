

import java.util.UUID

import codegen.model._
import IdentifiableGraph.IdentifiableToNodeGraph
import codegen.internal.{Attribute, Generate, Result}
import codegen.internal.Attribute._
import Generate.Implicits._
import codegen.internal.Transform._
import Generate.GenOps
import codegen.internal.Effect.Partial3
import codegen.model.Bool.{IdentifiableGuard, Equal, Testable}
import play.api.libs.json.Json

object Main {
  implicit val genThingShape: ConditionShape[GenThing, Attribute] = _.domain
  implicit val intShape: ConditionShape[Int, Attribute] = AttrNumber(_)
  implicit val stringShape: ConditionShape[String, Attribute] = AttrString(_)
  implicit def attrShape[A <: Attribute]: ConditionShape[A, Attribute] = x => x


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
    //create()
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

  def exper(): Unit = {
    import codegen.internal.Effect.Implicits._
    import codegen.model.Bool.IdentifiableGuard.mkOrderingOps

    val thing = Thing("thing", Json.obj("domain" -> true))


    val state = Map(thing.id -> Json.obj("domain" -> false))


    val association = thing === thing.attributes
    println(association.test(thing => state(thing.id)))
  }

  def create(): Unit = {
    import Conditional.DSL._
    import codegen.internal.Effect.Implicits.ForGen._
    // import codegen.internal.Effect.Implicits._

    val r1 = Thing.forGen("r1", Attribute("domain"-> List("foo", "bar")))
    val t1 = Thing.forGen("t1", Attribute("domain"-> List(true, false)))


    val conditionForO1 = Conditional((5 === 2) && ("foo" === "bar"))
    val o1 = Operation("o1", List(conditionForO1))

    val conditionForO2 = Conditional("k" === "e")
    val o2 = Operation("o2", List(conditionForO2))

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
