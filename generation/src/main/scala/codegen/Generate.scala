package codegen

import fs2.{Pipe, Stream, io, text}
import cats.effect.IO
import Generate.Implicits._
import codegen.buildermodel.Gen
import codegen.definition.{CaseClass, CaseClassDefinition, CaseVal, EmptyDefinition}
import codegen.model.Types.ID
import codegen.model.{Attributes, Model, Operation, Thing}
import org.scalafmt.Scalafmt
import play.api.libs.json._

trait Generate[A] {

  protected def format: String => String
  protected def codegen: A => Result
  def generated(a: A): Result = {
    val Result(result, dependencies) = codegen(a)

    Result(format(result), dependencies)
  }
}

case class Result(result: String, dependencies: Set[String] = Set()) {
  lazy val all: String = dependencies.mkString("\n") + result
}


trait HasType {
  def `type`: String
}

object Generate {
  def topLevelFormatter(expr: String): String = Scalafmt.format(expr).get
  def expressionFormatter(expr: String): String = {
    Scalafmt.format(s"object Temp {\n$expr\n}").get.lines.toList.dropRight(1).drop(1).mkString("\n")
  }



  def apply[A](genFunction: A => Result, formatter: String => String = topLevelFormatter): Generate[A] = new Generate[A] {
    override protected def format: String => String = formatter
    override protected def codegen: A => Result = genFunction
  }

  def expression[A](gen: A => Result): Generate[A] = apply(gen, expressionFormatter)
  def pureExpression[A](gen: A => String): Generate[A] = apply(x => Result(gen(x)), expressionFormatter)

  def obj[A](gen: A => Result): Generate[A] = apply(gen)
  def pureObj[A](gen: A => String): Generate[A] = apply(x => Result(gen(x)))

  def noFormat[A](gen: A => Result): Generate[A] = apply(gen, identity)
  def pureNoFormat[A](gen: A => String): Generate[A] = apply(x => Result(gen(x)), identity)

  def apply[A](implicit gen: Generate[A]): Generate[A] = gen

  implicit class GenOps[A](val a: A) extends AnyVal {
    def generated(implicit gen: Generate[A]): Result = gen.generated(a)
  }

  def writeFile[A: Generate](file: ScalaFile[A]): Stream[IO, Unit] = {
    Stream.eval(IO(file.generated.all))
      .through(text.utf8Encode)
      .through(io.file.writeAll(file.fullPath))
  }

  def file[A: Generate]: Pipe[IO, ScalaFile[A], Unit] = {
    _.flatMap { x => writeFile(x) }
  }

  def stream[F[_], A: Generate](gs: A*): Stream[F, A] = {
    Stream.emits(gs).covary[F]
  }

  object Implicits {
    implicit val genCaseClass: Generate[CaseClassDefinition] = Generate.pureObj { c =>
      views.txt.CaseClass(c.clazz, c.fields).body
    }

    implicit def genFile[A: Generate]: Generate[ScalaFile[A]] = Generate.obj { file =>
      val contentGen = file.contents.map(_.generated)
      val res = views.txt.ScalaFile(file.packageName, contentGen.map(_.result), contentGen.flatMap(_.dependencies).distinct).body
      Result(res)
    }

    implicit val genEmpty: Generate[EmptyDefinition] = Generate.pureExpression { _ => "" }

    implicit val genFromAttributes: Generate[Attributes] = Generate.pureExpression { attrs =>
      val things = attrs.data.fields.map { case (k, v) =>
          v match {
            case JsString(str) => CaseVal(k, str)
            case JsBoolean(bool) => CaseVal(k, bool)
            case JsArray(data) => CaseVal(k, "TODO[Array]")
            case JsObject(data) => CaseVal(k, "TODO[JsObject]")
          }
      }

      CaseClass("SomeClass$1", things:_*).generated.result
    }

    implicit val genBigDecimal: Generate[BigDecimal] = Generate.pureNoFormat { _.toString() }

    implicit val genBoolean: Generate[Boolean] = Generate.pureNoFormat { if(_) "true" else "false" }

    implicit val genCaseClassNew: Generate[CaseClass] = Generate.obj { c =>
      Result(views.txt.NewCaseClass(c.name, c.fields).body, Set("import monocle.macros.Lenses"))
    }

    implicit val genThing: Generate[Gen.GenThing] = Generate.expression { thing =>
      val (topVal, restClasses) = thing.attrObject.parse("data")
      val classes = CaseClass(thing.name, topVal) :: restClasses

      classes.generated
    }

    implicit val genString: Generate[String] = Generate.pureNoFormat { s => s""""$s"""" }

    implicit val genID: Generate[ID] = Generate.pureExpression { id =>
      s"""
        |UUID.fromString("${id.toString}")
      """.stripMargin
    }

    implicit val genOperation: Generate[Operation] = Generate.pureExpression { operation =>
      views.txt.model.identifiable.Operation(operation).body
    }

    implicit val genModel: Generate[Model] = Generate.pureObj { model =>
      views.txt.model.Model(model).body
    }

    implicit val genJsObject: Generate[JsObject] = Generate.pureExpression { obj =>
      s"""
         |Json.parse(\"\"\"${obj.toString()}\"\"\").validate[JsObject].get
       """.stripMargin
    }

    implicit val genCaseVal: Generate[CaseVal] = Generate.pureNoFormat { caseVal =>
      views.txt.CaseVal(caseVal).body
    }

    implicit def genSeq[A: Generate]: Generate[List[A]] = Generate.expression { seq =>
      val data = seq.map(_.generated)
      val hoists = data.flatMap(_.dependencies)
      val classes = data.map(_.result).mkString("\n")

      Result(classes, hoists.toSet)
    }

  }
}