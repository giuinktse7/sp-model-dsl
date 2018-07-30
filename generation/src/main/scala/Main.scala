
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
  import monocle.macros.Lenses

  @Lenses case class Generated_t2(
                                   domain: Seq[scala.Boolean] = Seq(true, false)
                                 )

  @Lenses case class Gen_t2(
                             name: String = "t2",
                             attrObject: Generated_t2 = Generated_t2(),
                             id: UUID = UUID.fromString("b3ceca11-4989-497c-987e-c12a6c559225")
                           )

  @Lenses case class Gen_t1(
                             name: String = "t1",
                             attrObject: Generated_t1 = Generated_t1(),
                             id: UUID = UUID.fromString("392828ed-8d68-40b4-b430-61375e621429")
                           )

  @Lenses case class Gen_o2(
                             name: String = "o2",
                             conditions: List[Conditional] =
                             List[Conditional](Conditional(Equal("k", "e"))),
                             attributes: JsObject = JsObject.empty,
                             id: UUID = UUID.fromString("d8d7761f-71a2-4ee6-855a-13220a6468c6"),
                             t2: Gen_t2 = Gen_t2()
                           )

  @Lenses case class Gen_r1(
                             name: String = "r1",
                             attrObject: Generated_r1 = Generated_r1(),
                             id: UUID = UUID.fromString("d6e0258c-56b3-41c0-8047-1b00c9c939da"),
                             o1: Gen_o1 = Gen_o1(),
                             o2: Gen_o2 = Gen_o2()
                           )

  @Lenses case class Generated_r1(
                                   someAttr: String = "kalle"
                                 )

  @Lenses case class Gen_o1(
                             name: String = "o1",
                             conditions: List[Conditional] =
                             List[Conditional](Conditional(Or(Equal(5, 2), Equal("foo", "bar")))),
                             attributes: JsObject = JsObject.empty,
                             id: UUID = UUID.fromString("6101cc44-813e-4269-9d90-6bd0cabf7c22"),
                             t1: Gen_t1 = Gen_t1(),
                             t2: Gen_t2 = Gen_t2()
                           )

  @Lenses case class Generated_t1(
                                   domain: Seq[java.lang.String] = Seq("foo", "bar")
                                 )

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

    val conditionForO1 = Conditional((5 ^== 2) && ("foo" ^== "bar"))
    val o1 = Operation("o1", List(conditionForO1))


    val conditionForO2 = Conditional("k" ^== "e")
    val o2 = Operation("o2", List(conditionForO2))

    val r1 = Thing.forGen("r1", Attribute("someAttr" -> "kalle"))

    val sop = OperationOrder.Specification("Sequential", OperationOrder.Sequential())

    val graph = r1 has (
      o1 has (t1, t2),
      o2 has t2
    )

    println(show(graph.generated))
  }

  def show(result: Result): String = {
    Scalafmt.format((result.dependencies + result.result).mkString("\n")).get
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
