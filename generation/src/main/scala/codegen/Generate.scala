package codegen

import fs2.{Pipe, Stream, io, text}
import cats.effect.IO
import Generate.Implicits._
import codegen.definition._
import codegen.model.Types.ID
import codegen.model._
import org.scalafmt.Scalafmt
import Generate.GenOps
import play.api.libs.json.JsObject
import util.Utils
import CaseClassLike._
import CaseClassLike.Implicits._

trait Generate[A] {

  protected def format: String => String
  protected def codegen: A => Result
  def generated(a: A): Result = {
    val Result(result, dependencies) = codegen(a)

    Result(format(result), dependencies)
  }
}

case class Result(result: String, dependencies: Set[Dependency] = Set()) {
  def compile: String = List(dependencies.generated.result, result).mkString("\n")
  def tupled: (String, Set[Dependency]) = (result, dependencies)
}


trait HasType {
  def `type`: String
}

object Generate {
  import Instantiate._

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
    Stream.eval(IO(file.generated.compile))
      .through(text.utf8Encode)
      .through(io.file.writeAll(file.fullPath))
  }

  def file[A: Generate]: Pipe[IO, ScalaFile[A], Unit] = {
    _.flatMap { x => writeFile(x) }
  }

  def stream[F[_], A: Generate](gs: A*): Stream[F, A] = {
    Stream.emits(gs).covary[F]
  }

  def parseIdentifiableGraph(node: IdentifiableGraph, classPrefix: String = ""): Either[CaseClass, Identifiable] = {
    if (node.nodes.isEmpty) Right(node.self)
    else {
      val nextPrefix = if (classPrefix nonEmpty) classPrefix + node.name else node.name + "_"
      val result = node.nodes.map { n =>
        parseIdentifiableGraph(n, nextPrefix) match {
          case Left(caseClass) => caseClass.localInstance.asCaseVal(n.name)
          case Right(identifiable) =>
            println(s"innermost with ${identifiable.name}")
            Namespace(identifiable, nextPrefix).instance.asCaseVal(identifiable.name)
        }
      }

      val selfGen = node.self.generated.dependencies
      val caseVals = Namespace(node.self, classPrefix).caseVals ++ result

      Left(CaseClass.withDependencies(s"${classPrefix}_${node.className}", selfGen, caseVals:_*))
    }
  }

  object Implicits {
    implicit val genCaseClassDefinition: Generate[CaseClassDefinition] = Generate.obj { c =>
      val monocle = ImportDependency("monocle.macros.Lenses")
      val res = s"@Lenses case class ${c.clazz}(${c.fields.map(x => s"${x.name}: ${x.className}").mkString(", ")})"

      Result(res, Set(monocle))
    }

    implicit def genFile[A: Generate]: Generate[ScalaFile[A]] = Generate.obj { file =>
      val contentGen = file.contents.map(_.generated)
      val code = contentGen.map(_.result).mkString("\n")

      Result(code, contentGen.flatMap(_.dependencies).toSet + PackageDependency(file.packageName))
    }

    implicit val genEmpty: Generate[EmptyDefinition] = Generate.pureExpression { _ => "" }

    implicit def genIndexedSeq[A]: Generate[IndexedSeq[A]] = Generate.pureExpression { values =>
     s"Vector(${values.mkString(",")})"
    }

    implicit val genBigDecimal: Generate[BigDecimal] = Generate.pureNoFormat { _.toString() }

    implicit val genBoolean: Generate[Boolean] = Generate.pureNoFormat { if(_) "true" else "false" }

    implicit val genCaseClass: Generate[CaseClass] = {
      Generate.obj { c =>
        val monocle = ImportDependency("monocle.macros.Lenses")
        val res = s"@Lenses case class ${c.name}(${c.caseVals.map(_.generated.result).mkString(", ")})"

        Result(res, c.dependencies + monocle)
      }
    }

    implicit val genConditional: Generate[Conditional] = Generate.expression { c =>
      val gen = c.proposition.map(_.gen)
      val deps = gen.flatMap(_.dependencies).toSet
      Result(s"Conditional(${gen.map(_.result).mkString(", ")})", deps)
    }

    implicit val genConditionNode: Generate[ConditionNode] = Generate.expression(_.gen)

    def genThingCaseClass(thing: Namespace[GenThing]): CaseClass = {
      val qualifiedName = thing.namespace + thing.value.name
      CaseClass(Utils.generateName("Thing", qualifiedName), thing.caseVals:_*)
    }

    implicit val genThing: Generate[GenThing] = Generate.expression { thing =>
      genThingCaseClass(Namespace.empty(thing)).generated
    }

    implicit val genInt: Generate[Int] = Generate.pureNoFormat { s => s.toString }

    implicit val genString: Generate[String] = Generate.pureNoFormat { s => s""""$s"""" }

    implicit val genID: Generate[ID] = Generate.pureExpression { id =>
      s"""
        |UUID.fromString("${id.toString}")
      """.stripMargin
    }

    implicit val genOperation: Generate[Operation] = Generate.expression { case Operation(name, conditions, attributes, id) =>
      val condGen = conditions.fill("List[Conditional]")
      val attrGen = attributes.generated
      val deps = condGen.dependencies ++ attrGen.dependencies

      Result(s"""
         |Operation(
         |\"$name\",
         |${condGen.result},
         |${attrGen.result},
         |${id.generated.result}
         )
       """.stripMargin, deps)
    }

    private def identifiableGraphToCaseVal(graph: IdentifiableGraph): CaseVal = {
      parseIdentifiableGraph(graph) match {
        case Left(caseClass) => CaseVal.defaultInstance(graph.name, caseClass.name).addDependencies(CaseClassDependency(caseClass))
        case Right(identifiable) => identifiable.localInstance.asCaseVal(identifiable.name)
      }
    }

    implicit val genIdentifiable: Generate[Identifiable] = Generate.expression {
      case x: Operation => x.generated
      case x: GenThing => x.generated
      case x => throw new IllegalArgumentException(s"There is no implicit Generate type class for $x.")
    }

    implicit val genIdentifiableGraph: Generate[IdentifiableGraph] = Generate.expression { graph =>
      parseIdentifiableGraph(graph) match {
        case Left(caseClass) => Set[Dependency](CaseClassDependency(caseClass)).generated
        case Right(identifiable) => identifiable.generated
      }
    }

    implicit val genDependencies: Generate[Set[Dependency]] = Generate.pureExpression { Dependency.fold }


    implicit val genModel: Generate[Model] = Generate.expression { model =>
      //val graphs = model.items.flatMap(_.collectNodes)
      val graphs = model.items.toList
      val instances = graphs.map(x => (x, parseIdentifiableGraph(x))).map {
        case (x, Left(caseClass)) => (x, caseClass.localInstance)
        case (x, Right(identifiable)) => (x, identifiable.localInstance)
      }

      val itemList = instances.map { case (_, instance) => instance.result.result }.mkString(", ")
      val items = CaseVal.rawQualified("items", s"List($itemList)", "List[Any]")

      val caseVals = items :: instances.map { case (node, instance) => instance.asCaseVal(node.name) }

      CaseClass(model.className, caseVals:_*).generated
    }

    implicit val genJsObject: Generate[JsObject] = Generate.pureExpression { obj =>
      if (obj.toString == "{}") "JsObject.empty"
      else s"""
         |Json.parse(\"\"\"${obj.toString}\"\"\").validate[JsObject].get
         |""".stripMargin
    }

    implicit val genCaseVal: Generate[CaseVal] = Generate.pureNoFormat { caseVal =>
      s"${caseVal.name}: ${caseVal.className} = ${caseVal.value}"
    }

    implicit def genList[A: Generate]: Generate[List[A]] = Generate.expression { seq =>
      val data = seq.map(_.generated)
      val hoists = data.flatMap(_.dependencies)
      val classes = data.map(_.result).mkString("\n")

      Result(classes, hoists.toSet)
    }

    implicit class GenListFolds[A: Generate](list: List[A]) {
      def fill(container: String): Result = {
        val data = list.map(_.generated)
        val hoists = data.flatMap(_.dependencies)
        val code = data.map(_.result).mkString(", ")

        Result(s"$container($code)", hoists.toSet)
      }
    }
  }
}