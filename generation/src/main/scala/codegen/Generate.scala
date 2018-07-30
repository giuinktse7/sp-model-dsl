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

trait Generate[A] {

  protected def format: String => String
  protected def codegen: A => Result
  def generated(a: A): Result = {
    val Result(result, dependencies) = codegen(a)

    Result(format(result), dependencies)
  }
}

sealed trait Dependency {
  /**
    *
    * @return The code for this dependency
    */
  def code: String
  def dependencies: Set[Dependency]

  /**
    * @return -1 if this dependency is prioritized, 0 if they are equally prioritized, 1 if this dependency is not prioritized
    */
  def comparePriority(that: Dependency): Int

  /**
    * Order of priority when comparing different Dependencies.
    * Dependencies with lower numbers appear higher up in the generated String.
    */
  def order: Int
}

object Dependency {
  val ImportOrder = 0
  val ClassOrder = 10

  def extractDependencies(dependency: Dependency): Set[Dependency] = {
    dependency.dependencies.flatMap(extractDependencies) + dependency
  }

  def fold(dependencies: Set[Dependency]): String = {
    val allDependencies = dependencies.flatMap(extractDependencies).toIndexedSeq.distinct
    val sortedDependencies = sortByPriority(allDependencies)


    sortedDependencies.map(_.code).mkString("\n")
  }

  def sortByPriority(dependencies: Seq[Dependency]): Seq[Dependency] = {
    dependencies.sortWith((a, b) => a.comparePriority(b) < 0)
  }
}

case class CaseClassDependency(clazz: CaseClass) extends Dependency {
  lazy val gen: Result = clazz.generated
  override def code: String = gen.result

  override def dependencies: Set[Dependency] = gen.dependencies

  /**
    * @return -1 if this dependency is prioritized, 0 if they are equally prioritized, 1 if this dependency is not prioritized
    */
  override def comparePriority(that: Dependency): Int = that match {
    case CaseClassDependency(c) => this.clazz.name.compareTo(c.name)
    case other => order.compareTo(other.order)
  }

  override def order: Int = Dependency.ClassOrder
}

case class ImportDependency(text: String) extends Dependency {
  override def code: String = s"import $text"

  override def dependencies: Set[Dependency] = Set()
  override def comparePriority(that: Dependency): Int = that match {
    case ImportDependency(p) => this.text.compareTo(p)
    case other => order.compareTo(other.order)
  }
  override def order: Int = Dependency.ImportOrder
}

case class Result(result: String, dependencies: Set[Dependency] = Set()) {
  def compile: String = List(Dependency.fold(dependencies), result).mkString("\n")
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

  object Implicits {
    implicit val genCaseClass: Generate[CaseClassDefinition] = Generate.pureObj { c =>
      views.txt.CaseClass(c.clazz, c.fields).body
    }

    implicit def genFile[A: Generate]: Generate[ScalaFile[A]] = Generate.obj { file =>
      val contentGen = file.contents.map(_.generated)
      val res = views.txt.ScalaFile(file.packageName, contentGen.map(_.result), Dependency.fold(contentGen.flatMap(_.dependencies).toSet)).body
      Result(res)
    }

    implicit val genEmpty: Generate[EmptyDefinition] = Generate.pureExpression { _ => "" }

    implicit def genIndexedSeq[A]: Generate[IndexedSeq[A]] = Generate.pureExpression { values =>
     s"Vector(${values.mkString(",")})"
    }

    implicit val genBigDecimal: Generate[BigDecimal] = Generate.pureNoFormat { _.toString() }

    implicit val genBoolean: Generate[Boolean] = Generate.pureNoFormat { if(_) "true" else "false" }

    implicit val genCaseClassNew: Generate[CaseClass] = {
      Generate.obj { c =>
        val monocle = ImportDependency("monocle.macros.Lenses")
        Result(views.txt.NewCaseClass(c.name, c.fields).body, c.dependencies ++ c.fields.flatMap(_.dependencies) + monocle)
      }
    }

    implicit val genConditional: Generate[Conditional] = Generate.expression { c =>
      val gen = c.proposition.map(_.gen)
      val deps = gen.flatMap(_.dependencies).toSet
      Result(s"Conditional(${gen.map(_.result).mkString(", ")})", deps)
    }

    implicit val genConditionNode: Generate[ConditionNode] = Generate.expression(_.gen)

    def genThingCaseClass(thing: GenThing): CaseClass = {
      val caseVals = genThingCaseVals(thing)
      CaseClass(Utils.generateName("Thing", thing.name), caseVals:_*)
    }

    implicit val genThing: Generate[GenThing] = Generate.expression { thing =>
      genThingCaseClass(thing).generated
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
      val conditionalCaseVal = operation.conditions.map(_.generated)
      val condValue = s"List[Conditional](${conditionalCaseVal.map(_.result).mkString(", ")})"
      val condDeps = conditionalCaseVal.flatMap(_.dependencies).toSet

      List(
        CaseVal("name", operation.name),
        CaseVal.rawQualified("conditions", condValue, "List[Conditional]").addDependencies(condDeps),
        CaseVal("attributes", operation.attributes),
        CaseVal("id", operation.id)
      )
    }

    def genThingCaseVals(thing: GenThing): List[CaseVal] = {
      val attrCaseVal = thing.attrObject.toGen("attrObject")
      List(
        CaseVal("name", thing.name),
        attrCaseVal,
        CaseVal("id", thing.id)
      )
    }

    private def identifiableGraphToCaseVal(graph: IdentifiableGraph): CaseVal = {
      genIdentifiableGraph(graph) match {
        case Left(caseClass) => CaseVal.defaultInstance(graph.name, caseClass.name).addDependencies(CaseClassDependency(caseClass))
        case Right(identifiable) => identifiableCaseVal(identifiable, Utils.generateName("Attr", identifiable.name))
      }
    }

    def structCaseClass(struct: Struct): CaseClass = {
      val idGraphVals = struct.items.map(identifiableGraphToCaseVal)

      val caseVals = List(
        CaseVal("name", struct.name),
        CaseVal("attributes", struct.attributes),
        CaseVal("id", struct.id)
      ) ++ idGraphVals

      CaseClass(Utils.generateName("Struct", struct.name), caseVals:_*)
    }

    implicit val genStruct: Generate[Struct] = Generate.expression { struct =>
      structCaseClass(struct).generated
    }

    implicit val genIdentifiable: Generate[Identifiable] = Generate.expression {
      case x: Operation => x.generated
      case x: GenThing => x.generated
      case x: Struct => x.generated
      case x => throw new IllegalArgumentException(s"There is no implicit Generate type class for $x.")
    }

    def identifiableCaseVal(identifiable: Identifiable, className: String): CaseVal = identifiable match {
      case _: Operation =>
        val (res, deps) = identifiable.generated.tupled
        CaseVal(identifiable.name, "Operation", res).addDependencies(deps)
      case _: Struct =>
        val (res, deps) = identifiable.generated.tupled
        CaseVal.defaultInstance(identifiable.name, "Struct").addDependencies(deps)
      case thing: GenThing =>
        val caseClass = CaseClassDependency(CaseClass(className, genThingCaseVals(thing):_*))
        val classDep = CaseClassDependency(genThingCaseClass(thing))

        CaseVal.defaultInstance(identifiable.name, caseClass.clazz.name).addDependencies(classDep, caseClass)
      case x => throw new IllegalArgumentException(s"$x can not be used in an IdentifiableGraph.")
    }

    def genIdentifiableGraph(node: IdentifiableGraph): Either[CaseClass, Identifiable] = {
      if (node.nodes.isEmpty) Right(node.self)
      else {
        val result = node.nodes.map { n =>
          genIdentifiableGraph(n) match {
            case Left(caseClass) =>
              CaseVal.defaultInstance(n.name, caseClass.name).addDependencies(CaseClassDependency(caseClass))

            case Right(identifiable) =>
              identifiableCaseVal(identifiable, n.className)
          }
        }

        // val className = s"${ID.validIdentifier(length = 5)}_GenFor_${node.name}"
        val ownCaseVals = node.self match {
          case o: Operation => operationCaseVals(o)
          case t: GenThing => genThingCaseVals(t)
          case _ => List()
        }

        val selfGen = node.self.generated.dependencies
        val caseVals = ownCaseVals ++ result

        Left(CaseClass.withDependencies(node.className, selfGen, caseVals:_*))
      }
    }

    implicit val genIdentifiableGraph: Generate[IdentifiableGraph] = Generate.expression { graph =>
      genIdentifiableGraph(graph) match {
        case Left(caseClass) => Set[Dependency](CaseClassDependency(caseClass)).generated
        case Right(identifiable) => identifiable.generated
      }
    }

    implicit val genDependencies: Generate[Set[Dependency]] = Generate.pureExpression { deps =>
      Dependency.fold(deps)
    }

    implicit val genModel: Generate[Model] = Generate.expression { model =>
      val caseVals = model.items.map(x => x.instance.caseVal(x.name))

      CaseClass(model.className, caseVals:_*).generated
    }

    implicit val genJsObject: Generate[JsObject] = Generate.pureExpression { obj =>
      if (obj.toString == "{}") "JsObject.empty"
      else s"""
         |Json.parse(\"\"\"${obj.toString}\"\"\").validate[JsObject].get
         |""".stripMargin
    }

    implicit val genCaseVal: Generate[CaseVal] = Generate.pureNoFormat { caseVal =>
      views.txt.CaseVal(caseVal).body
    }

    implicit def genList[A: Generate]: Generate[List[A]] = Generate.expression { seq =>
      val data = seq.map(_.generated)
      val hoists = data.flatMap(_.dependencies)
      val classes = data.map(_.result).mkString("\n")

      Result(classes, hoists.toSet)
    }

    implicit def genSeq[A: Generate]: Generate[Seq[A]] = Generate.expression { seq =>
      val data = seq.map(_.generated)
      val hoists = data.flatMap(_.dependencies)
      val classes = data.map(_.result).mkString("\n")

      Result(classes, hoists.toSet)
    }

  }
}