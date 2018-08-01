
import java.nio.file.Paths
import java.util.UUID

import codegen._
import codegen.definition.{CaseClassDefinition, FieldDefinition}
import fs2.Stream
import Generate.Implicits._
import codegen.model._
import org.scalafmt.Scalafmt
import IdentifiableGraph.IdentifiableToNodeGraph
import codegen.model.Test.ConditionalDSL
import Generate._
import play.api.libs.json.JsObject

object Main {

  def main(args: Array[String]): Unit = {
    //create()

    val subThing = Thing.forGen("subThing", Attribute("domain" -> List("foo", "bar")))

    val condition = Conditional((5 !== 2) && ("foo" === "bar"))
    val someOperation = Operation("someOperation", List(condition))

    val mainThing = Thing.forGen("mainThing", Attribute("someAttr" -> "kalle"))

    val items = mainThing -> (
      someOperation -> subThing
    )

    val model = Model("TestModel", items)

    println(show(model.generated))

  }

  def create(): Unit = {
    val redThing = Thing.forGen("redThing", Attribute("domain"-> List("foo", "bar")))
    val blueThing = Thing.forGen("blueThing", Attribute("domain"-> List(true, false)))

    val conditionForO1 = Conditional((5 === 2) && ("foo" === "bar"))
    val someOperation = Operation("someOperation", List(conditionForO1))

    val conditionForO2 = Conditional("k" === "e")
    val o2 = Operation("o2", List(conditionForO2))

    val mainThing = Thing.forGen("mainThing", Attribute("someAttr" -> "kalle"))

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential())

    /*
    val items = r1 has (
      o1 has (t1, t2),
      o2 has t2
    )
    */

    val items = mainThing -> (
      someOperation -> redThing
    )

    val model = Model("TestModel", items)

    println(show(model.generated))
  }

  def show(result: Result): String = {
    Scalafmt.format(result.compile).get
  }

  def show[A: Generate](gen: A*): String = {
    val data = gen.map(_.generated).toList
    val hoists = data.flatMap(_.dependencies).distinct
    val classes = data.map(_.result)

    Scalafmt.format((hoists ::: classes).mkString("\n")).get
  }

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
}
