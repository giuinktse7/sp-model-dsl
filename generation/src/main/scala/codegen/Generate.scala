package codegen

import fs2.{Pipe, Stream, io, text}
import cats.effect.IO
import Generate.Implicits._
import codegen.definition._
import codegen.model.Types.ID
import codegen.model._
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
  def tupled = (result, dependencies)
}


trait HasType {
  def `type`: String
}

object Generate {
  type Dependency = CaseClass

  def topLevelFormatter(expr: String): String = {
    Scalafmt.format(expr).get
  }
  def expressionFormatter(expr: String): String = {
    Scalafmt.format(s"object Temp {\n$expr\n}").get.lines.toList.dropRight(1).drop(1).mkString("\n")
  }

  def aggregateResults(results: Result*): Result = {
    Result(
      results.map(_.result).mkString("\n"),
      results.map(_.dependencies).reduce(_ ++ _)
    )
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

    implicit def genIndexedSeq[A]: Generate[IndexedSeq[A]] = Generate.pureExpression { values =>
     s"Vector(${values.mkString(",")})"
    }

    implicit val genBigDecimal: Generate[BigDecimal] = Generate.pureNoFormat { _.toString() }

    implicit val genBoolean: Generate[Boolean] = Generate.pureNoFormat { if(_) "true" else "false" }

    implicit val genCaseClassNew: Generate[CaseClass] = Generate.obj { c =>
      Result(views.txt.NewCaseClass(c.name, c.fields).body, Set("import monocle.macros.Lenses") ++ c.dependencies ++ c.fields.flatMap(_.dependencies))
    }

    implicit val genConditional: Generate[Conditional] = Generate.expression { c =>
      val (res, deps) = c.proposition.generated.tupled
      Result(s"Conditional($res)", deps)
    }

    implicit val genBool: Generate[Bool] = Generate.expression(_.gen)

    def genThingCaseClasses(thing: GenThing): CaseClass = {
      val (topVal, restClasses) = thing.attrObject.toGen("data")
      val relevantDependencies = (
        restClasses.flatMap(_.dependencies)
          ++ restClasses.flatMap { x =>
          val (res, deps) = x.generated.tupled
          deps + res
        }
        )

      CaseClass.withDependencies(topVal.className, relevantDependencies.toSet, (topVal :: genThingCaseVals(thing)):_*)
    }

    implicit val genThing: Generate[GenThing] = Generate.expression { thing =>
      genThingCaseClasses(thing).generated
    }

    implicit val genInt: Generate[Int] = Generate.pureNoFormat { s => s.toString }

    implicit val genString: Generate[String] = Generate.pureNoFormat { s => s""""$s"""" }

    implicit val genID: Generate[ID] = Generate.pureExpression { id =>
      s"""
        |UUID.fromString("${id.toString}")
      """.stripMargin
    }

    implicit val genOperation: Generate[Operation] = Generate.expression { case Operation(name, conditions, attributes, id) =>
      val condGen = conditions.generated
      val attrGen = attributes.generated
      val deps = condGen.dependencies ++ attrGen.dependencies

      Result(s"""
         |Operation(
         |\"$name\",
         |List[Conditional](${condGen.result}),
         |${attrGen.result},
         |${id.generated.result}
         )
       """.stripMargin, deps)
    }


    def operationCaseVals(operation: Operation): List[CaseVal] = {
      val operationConditions = operation.conditions.generated
      val opValue = s"List[Conditional](${operationConditions.result})"
      List(
        CaseVal("name", operation.name),
        CaseVal.rawQualified("conditions", opValue, "List[Conditional]").addDependencies(operationConditions.dependencies),
        CaseVal("attributes", operation.attributes),
        CaseVal("id", operation.id)
      )
    }

    def genThingCaseVals(thing: GenThing): List[CaseVal] = {
      val (attrCaseVal, deps) = thing.attrObject.toGen("attrObject")
      List(
        CaseVal("name", thing.name),
        attrCaseVal.addDependencies(deps.flatMap(_.generated.dependencies).toSet),
        CaseVal("id", thing.id)
      )
    }

    /**
      * case class Struct(
      * name: String,
      * items: IdentifiableGraph,
      * attributes: AttributeMap = AttributeMap(),
      * id: ID = ID()
      */
    implicit val genStruct: Generate[Struct] = Generate.expression { struct =>
      Result("")
    }

    implicit val genIdentifiable: Generate[Identifiable] = Generate.expression {
      case x: Operation => x.generated
      case x: GenThing => x.generated
      case x: Struct => x.generated
    }

    def genIdentifiableGraph(node: IdentifiableGraph): Either[CaseClass, Identifiable] = {
      if (node.nodes.isEmpty) Right(node.self)
      else {
        val result = node.nodes.map { n =>
          genIdentifiableGraph(n) match {
            case Left(caseClass) =>
              val deps: Set[String] = caseClass.generated.dependencies + caseClass.generated.result
              (CaseVal.defaultInstance(n.name, caseClass.name), deps)

            case Right(identifiable) =>
              identifiable match {
                case _: Operation =>
                  val (res, deps) = identifiable.generated.tupled
                  (CaseVal(n.name, "Operation", res), deps)
                case _: Struct =>
                  val (res, deps) = identifiable.generated.tupled
                  (CaseVal.defaultInstance(n.name, "Struct"), deps)
                case thing: GenThing =>
                  val dependencies = thing.generated.dependencies
                  val caseClass = CaseClass.withDependencies(s"Gen_${thing.name}", dependencies, genThingCaseVals(thing):_*)
                  val genClass = genThingCaseClasses(thing)

                  if (thing.name == "t1") println(genClass.dependencies ++ caseClass.dependencies)
                  (CaseVal.defaultInstance(n.name, caseClass.name), genClass.dependencies ++ caseClass.dependencies ++ caseClass.generated.dependencies + caseClass.generated.result)
              }
          }
        }

        // val className = s"${ID.validIdentifier(length = 5)}_GenFor_${node.name}"
        val className = s"Gen_${node.name}"
        val ownCaseVals = node.self match {
          case o: Operation => operationCaseVals(o)
          case t: GenThing => genThingCaseVals(t)
          case _ => List()
        }

        val selfGen = node.self.generated.dependencies

        val deps = result.foldLeft(Set[String]()) { case (acc, (_, nextDeps)) => acc ++ nextDeps } ++ ownCaseVals.flatMap(_.dependencies) ++ selfGen
        val caseVals = ownCaseVals ++ result.map { case (caseVal, _) => caseVal }.toSeq

        Left(CaseClass.withDependencies(className, deps, caseVals:_*))
      }
    }

    implicit val genIdentifiableGraph: Generate[IdentifiableGraph] = Generate.expression { graph =>
      genIdentifiableGraph(graph) match {
        case Left(caseClass) => caseClass.generated
        case Right(identifiable) => identifiable.generated
      }
    }

    implicit val genModel: Generate[Model] = Generate.pureObj { model =>
      views.txt.model.Model(model).body
    }

    implicit val genJsObject: Generate[JsObject] = Generate.pureExpression { obj =>
      if (obj.toString() == "{}") "JsObject.empty"
      else s"""
         |Json.parse(\"\"\"${obj.toString()}\"\"\").validate[JsObject].get
         |""".stripMargin
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