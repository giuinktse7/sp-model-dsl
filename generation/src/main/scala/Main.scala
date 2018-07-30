
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
    // thingExample()

    create()

    /*
    val rootPath = Paths.get("generation/src/main/scala")
    val relativePath = Paths.get("codegen/generated")

    val file = ScalaFile(rootPath, relativePath, "Model", classes)
    Generate.writeFile(file).compile.drain.unsafeRunSync()
    */
  }

  def create(): Unit = {
    val t1 = Thing.forGen("t1", Attribute("domain"-> List("foo", "bar")))
    val t2 = Thing.forGen("t2", Attribute("domain"-> List(true, false)))

    val conditionForO1 = Conditional((5 === 2) && ("foo" === "bar"))
    val o1 = Operation("o1", List(conditionForO1))


    val conditionForO2 = Conditional("k" === "e")
    val o2 = Operation("o2", List(conditionForO2))

    val r1 = Thing.forGen("r1", Attribute("someAttr" -> "kalle"))

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential())

    val items = r1 has (
      o1 has (t1, t2),
      o2 has t2
    )


    val struct = Struct("struct")(items)
    val model = Model("TestModel", List(t1, t2))

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

    val rootPath = Paths.get("src/main/scala")
    val directory = rootPath.resolve(Paths.get("codegen/generated"))

    val files = Stream(lineGen, pointGen) // Stream of Generate[A]
      .map(g => ScalaFile(directory, g)) // Convert each Generate[A] to a ScalaFile[A] to be created in 'directory'
      .flatMap(f => Generate.writeFile(f)) // Write files to disk

    files.compile.drain.unsafeRunSync()
  }
}
