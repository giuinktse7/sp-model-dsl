
import java.nio.file.Paths

import codegen._
import codegen.definition.{CaseClassDefinition, FieldDefinition}
import fs2.Stream
import Generate.Implicits._
import Generate._
import codegen.buildermodel.Gen.GenThing
import codegen.model.Types.ID
import org.scalafmt.Scalafmt

object Main {
  import monocle.macros.Lenses
  @Lenses case class MainThing(
                                data: SubThing = SubThing()
                              )

  @Lenses case class SubThing(
                               config: Config = Config(),
                               parentId: String = "abc123"
                             )

  @Lenses case class Config(
                             width: BigDecimal = 28.7,
                             height: BigDecimal = 25.2
                           )

  def main(args: Array[String]): Unit = {
    // thingExample()

    println(MainThing())

    /*
    val rootPath = Paths.get("generation/src/main/scala")
    val relativePath = Paths.get("codegen/generated")

    val file = ScalaFile(rootPath, relativePath, "Model", classes)
    Generate.writeFile(file).compile.drain.unsafeRunSync()
    */
  }

  def thingExample(): Unit = {

    val attributes = Attribute(
      "parentId" -> "abc123",
      "config" -> Attribute("height" -> 25.2, "width" -> 28.7).named("Config")
    ).named("SubThing")

    val thing = GenThing("MainThing", attributes, ID())


    println(show(thing))
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
