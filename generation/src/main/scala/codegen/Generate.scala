package codegen

import fs2.{Pipe, Stream, io, text}
import cats.effect.IO
import Generate.Implicits._
import codegen.definition.{CaseClass, CaseClassDefinition, CaseVal, EmptyDefinition}
import codegen.model.Types.ID
import codegen.model.{Attributes, Model, Operation, Thing}
import org.scalafmt.Scalafmt
import play.api.libs.json._

trait Generate[A] {
  protected def format: String => String
  protected def codegen: A => String
  def generated(a: A): String = format(codegen(a))
}

trait HasType {
  def `type`: String
}

object Generate {
  def topLevelFormatter(expr: String): String = Scalafmt.format(expr).get
  def expressionFormatter(expr: String): String = {
    Scalafmt.format(s"object Temp {\n$expr\n}").get.lines.toList.dropRight(1).drop(1).mkString("\n")
  }

  def apply[A](genFunction: A => String, formatter: String => String = topLevelFormatter): Generate[A] = new Generate[A] {
    override protected def format: String => String = formatter
    override protected def codegen: A => String = genFunction
  }

  def expression[A](gen: A => String): Generate[A] = apply(gen, expressionFormatter)
  def obj[A](gen: A => String): Generate[A] = apply(gen)
  def noFormat[A](gen: A => String): Generate[A] = apply(gen, identity)
  def apply[A](implicit gen: Generate[A]): Generate[A] = gen

  implicit class GenOps[A](val a: A) extends AnyVal {
    def generated(implicit gen: Generate[A]): String = gen.generated(a)
  }

  def writeFile[A: Generate](file: ScalaFile[A]): Stream[IO, Unit] = {
    Stream.eval(IO(file.generated))
      .through(text.utf8Encode)
      .through(io.file.writeAll(file.path.resolve(s"${file.name}.scala")))
  }

  def file[A: Generate]: Pipe[IO, ScalaFile[A], Unit] = {
    _.flatMap { x => writeFile(x) }
  }

  def stream[F[_], A: Generate](gs: A*): Stream[F, A] = {
    Stream.emits(gs).covary[F]
  }

  object Implicits {
    implicit val genCaseClass: Generate[CaseClassDefinition] = Generate.obj { c =>
      views.txt.CaseClass(c.clazz, c.fields).body
    }

    implicit def genFile[A: Generate]: Generate[ScalaFile[A]] = Generate.obj { file =>
      views.txt.ScalaFile(file.packageName, file.contents.map(_.generated)).body
    }

    implicit val genEmpty: Generate[EmptyDefinition] = Generate.expression { _ => "" }

    implicit val genFromAttributes: Generate[Attributes] = Generate.expression { attrs =>
      val things = attrs.data.fields.map { case (k, v) =>
          v match {
            case JsString(str) => CaseVal(k, str)
            case JsBoolean(bool) => CaseVal(k, bool)
            case JsArray(data) => CaseVal(k, "TODO[Array]")
            case JsObject(data) => CaseVal(k, "TODO[JsObject]")
          }
      }

      CaseClass("SomeClass$1", things:_*).generated
    }

    implicit val genBoolean: Generate[Boolean] = Generate.noFormat { if(_) "true" else "false" }

    implicit val genCaseClassNew: Generate[CaseClass] = Generate.obj { c =>
      views.txt.NewCaseClass(c.name, c.fields).body
    }

    implicit val genThing: Generate[Thing] = Generate.expression { thing =>
      val name = CaseVal("name", thing.name)
      val attrs = CaseVal("attributes", thing.attributes)
      val id = CaseVal("id", thing.id)
      // views.txt.model.identifiable.Thing(thing).body
      views.txt.NewCaseClass("Thing", Seq(name, attrs, id)).body
    }

    implicit val genString: Generate[String] = Generate.noFormat { s => s""""$s"""" }

    implicit val genID: Generate[ID] = Generate.expression { id =>
      s"""
        |UUID.fromString("${id.toString}")
      """.stripMargin
    }

    implicit val genOperation: Generate[Operation] = Generate.expression { operation =>
      views.txt.model.identifiable.Operation(operation).body
    }

    implicit val genModel: Generate[Model] = Generate.obj { model =>
      views.txt.model.Model(model).body
    }

    implicit val genJsObject: Generate[JsObject] = Generate.expression { obj =>
      s"""
         |Json.parse(\"\"\"${obj.toString()}\"\"\").validate[JsObject].get
       """.stripMargin
    }

    implicit val genCaseVal: Generate[CaseVal] = Generate.noFormat { caseVal =>
      views.txt.CaseVal(caseVal).body
    }
  }
}