
import java.nio.file.Paths

import codegen._
import codegen.definition.{CaseClassDefinition, CaseVal, FieldDefinition}
import fs2.Stream
import Generate.Implicits._
import codegen.model.{Attributes, Operation, Thing}
import Generate._
import play.api.libs.json.{JsObject, JsString}

object Main {
  def main(args: Array[String]): Unit = {
    val operation = Operation("My operation")

    val thing = Thing("Test", JsObject(Seq("a" -> JsString("25"), "b" -> JsObject(Seq("c" -> JsString("10"))))), java.util.UUID.randomUUID())

    val test = Attributes(JsObject(Seq("a" -> JsString("25"), "b" -> JsObject(Seq("c" -> JsString("10"))))))
    println(test.generated)
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
