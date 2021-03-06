package codegen.internal

import java.nio.file.Path
import java.util.UUID

import fs2.{Pipe, Stream, io, text}
import cats.effect.IO
import Generate.Implicits._
import codegen.model.Types.ID
import codegen.model.{ConditionNode, _}
import org.scalafmt.Scalafmt
import Generate.GenOps
import codegen.internal.Attribute.AttrObject
import play.api.libs.json._
import codegen.internal.definition._
import codegen.model.ConditionNode.{Definition, IdNode, Value}
import codegen.internal.definition.CaseClassLike.CaseClassLikeOps
import codegen.internal.definition.CaseClassLike.Implicits._
import codegen.model.Bool.{Equal, Not}
import monocle.macros.Lenses
import names.NameOf.{qualifiedNameOfType => typeName}

trait Generate[A] { self =>
  protected def format: String => String
  protected def generate: A => Result

  def generated(a: A): Result = {
    val Result(result, dependencies) = generate(a)

    Result(format(result), dependencies)
  }

  def mapDependencies(f: Set[Dependency] => Set[Dependency]): Generate[A] = new Generate[A] {
    override protected def format = self.format

    override protected def generate = self.generate.andThen(res => res.mapDependencies(f))
  }
}

// TODO Implement code Regions based on dependency type.
/*
 * This will provide more fine-grained control over file structure at the definition site of a Generate[A].
 * Eg. to enclose part of the generated code in an object {}, one could:
 * Generate.obj { o =>
 * val r1 = Region(PackageDependency, ImportDependency) // (Maybe define regions by dependency priority?)
 * val r2 = Region.allExcept(r1)
 * CompilationOrder(
 *   r1,
 *   r2.map(s => s"object ${o.name} { $s }")
 * )
 *
 * If we allow definition of regions by priority, we could do eg.Region.range(0, 10)
 * instead of specifying explicit region classes.
 *
 * The CompilationOrder would be used similarly to how Result.compile is currently used.
 */
case class Result(result: String, dependencies: Set[Dependency] = Set()) {
  def compile: String = {
    List(dependencies.generated.result, result).mkString("\n")
  }
  def tupled: (String, Set[Dependency]) = (result, dependencies)

  def combine(other: Result)(f: (String, String) => String) = Result(
    f(result, other.result),
    dependencies ++ other.dependencies
  )

  def requires[A <: Dependency](deps: Set[A]): Result = copy(dependencies = this.dependencies ++ deps)
  def mapDependencies(f: Set[Dependency] => Set[Dependency]): Result = copy(dependencies = f(this.dependencies))

  def map(f: String => String): Result = copy(result = f(result))
}

/**
  * Models the result of generating code for an object, for instance
  * through obj.generated.
  */
object Result {

  def foldSeq(rs: Seq[Result]): Result = Result(
    rs.map(_.result).mkString(", "),
    rs.flatMap(_.dependencies).toSet
  )

  def map2(r1: Result, r2: Result)(f: (String, String) => String): Result = r1.combine(r2)(f)
  def map3(r1: Result, r2: Result, r3: Result)(f: (String, String, String) => String): Result = {
    Result(
      f(r1.result, r2.result, r3.result),
      r1.dependencies ++ r2.dependencies ++ r3.dependencies
    )
  }
}


trait HasType {
  def `type`: String
}

final case class GenException(
                               private val message: String = "",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


trait GeneratedIdentifiable {
  def name: String
  def id: UUID
}


object Generate {
  def saveAsFile[A: Generate](path: Path, fileName: String)(a: A): Stream[IO, Unit] = writeFile(ScalaFile(path, a, fileName))
  type Kind[F[_]] = Generate[F[Result]]
  type Kind2[F[_, _], A] = Generate[F[A, Result]]
  type Kind3[F[_, _, _], A, B] = Generate[F[A, B, Result]]

  import Instantiate._

  def topLevelFormatter(expr: String): String = {
    Scalafmt.format(expr).get
  }
  def expressionFormatter(expr: String): String = {
    Scalafmt.format(s"object Temp {\n$expr\n}").get.lines.toList.dropRight(1).drop(1).mkString("\n").trim()
  }

  def aggregateResults(results: Result*): Result = {
    Result(
      results.map(_.result).mkString("\n"),
      results.map(_.dependencies).reduce(_ ++ _)
    )
  }

  def apply[A](genFunction: A => Result, formatter: String => String = topLevelFormatter): Generate[A] = new Generate[A] {
    override protected def format: String => String = formatter
    override protected def generate: A => Result = genFunction

  }

  def expression[A](gen: A => Result): Generate[A] = apply(gen, expressionFormatter)
  def pureExpression[A](gen: A => String): Generate[A] = apply(x => Result(gen(x)), expressionFormatter)
  def constant[A](gen: => String): Generate[A] = apply(_ => Result(gen), expressionFormatter)

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

  def parseIdentifiableGraph(parent: IdentifiableGraph, classPrefix: String = "Generated_"): Either[CaseClass, Identifiable] = {
    if (parent.nodes.isEmpty) Right(parent.self)
    else {
      val newPrefix = if (classPrefix.nonEmpty) classPrefix + parent.name + "_" else parent.name + "_"
      val result = parent.nodes.map { child =>
        parseIdentifiableGraph(child, newPrefix) match {
          case Left(caseClass) => caseClass.localInstance.asCaseVal(child.name)
          case Right(identifiable) => Namespace(identifiable, newPrefix).instance.asCaseVal(identifiable.name)
        }
      }

      val caseVals = Namespace(parent.self, classPrefix).caseVals ++ result
      val genImport: Dependency = ImportDependency(typeName[GeneratedIdentifiable])

      Left(CaseClass(classPrefix + parent.name, Set(genImport), caseVals, List("GeneratedIdentifiable")))
    }
  }

  object Implicits {
    implicit val genCaseClassDefinition: Generate[CaseClassDefinition] = Generate.obj { c =>
      val monocle = ImportDependency("monocle.macros.Lenses")
      val res = s"@Lenses case class ${c.clazz}(${c.fields.map(x => s"${x.name}: ${x.className}").mkString(", ")})"

      Result(res, Set(monocle))
    }

    implicit def genFile[A: Generate]: Generate[ScalaFile[A]] = Generate.obj { file =>
      Result
        .foldSeq(file.contents.map(_.generated))
        .requires(Set(PackageDependency(file.packageName)))
    }

    implicit val genEmpty: Generate[EmptyDefinition] = Generate.pureExpression { _ => "" }

    implicit def genIndexedSeq[A]: Generate[IndexedSeq[A]] = Generate.pureExpression { values =>
     s"Vector(${values.mkString(",")})"
    }


    implicit val genBoolean: Generate[Boolean] = Generate.pureNoFormat { if(_) "true" else "false" }

    implicit val genCaseClass: Generate[CaseClass] = Generate.obj { c =>
      val monocle = ImportDependency(typeName[Lenses])
      Result
        .foldSeq(c.caseVals.map(_.generated)).map(r => s"@Lenses case class ${c.name}($r) ${c.genExtends}")
        .requires(c.dependencies + monocle)
    }

    def genThingCaseClass(thing: Namespace[GenThing]): CaseClass = {
      val qualifiedName = thing.namespace + thing.value.name
      val identifiable: Dependency = ImportDependency(typeName[GeneratedIdentifiable])
      CaseClass(qualifiedName, Set(identifiable), thing.caseVals, List("GeneratedIdentifiable"))
    }

    implicit val genThing: Generate[GenThing] = Generate.expression { thing =>
      genThingCaseClass(Namespace.empty(thing)).generated
    }

    implicit val genInt: Generate[Int] = Generate.pureNoFormat { s => s.toString }
    implicit val genDouble: Generate[Double] = Generate.pureNoFormat { s => s.toString }
    implicit val genBigDecimal: Generate[BigDecimal] = Generate.pureNoFormat { _.toString() }
    implicit val genLong: Generate[Long] = Generate.pureNoFormat { s => s.toString }

    implicit val genString: Generate[String] = Generate.pureNoFormat { s => s""""$s"""" }

    implicit val genID: Generate[ID] = Generate.expression { id =>
      Result(
        s"""UUID.fromString("${id.toString}")""",
        Set(ImportDependency(typeName[UUID]))
      )
    }

    implicit val genOperation: Generate[Operation] = Generate.expression { case Operation(name, conditions, attributes, id) =>
      val condGen = conditions.fill("List[Conditional]")
      val attrGen = attributes.generated

      Result.map3(condGen, attrGen, id.generated)((cond, attr, id) => s"Operation($name, $cond, $attr, $id)")
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

    implicit val genDependencies: Generate[Set[Dependency]] = Generate.pureNoFormat { Dependency.fold }


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

    implicit val genJsObject: Generate[JsObject] = Generate.expression { obj =>
      val res = {
        if (obj.toString == "{}") "JsObject.empty"
        else s"""Json.parse("${obj.toString}").validate[JsObject].get"""
      }

      val playJson = ImportDependency("play.api.libs.json._")
      Result(res, Set(playJson))
    }

    implicit val genAttrObject: Generate[AttrObject] = Generate.expression { _.toSPValue.generated }

    implicit val genJsValue: Generate[JsValue] = Generate.expression { value =>
      val res = value match {
        case x@JsObject(_) => x.generated.result
        case JsString(v) => s"""JsString("$v")"""
        case JsNumber(v) => s"""JsNumber($v)"""
        case JsBoolean(bool) => s"""JsBoolean($bool)"""
        case array@JsArray(_) => s"""Json.parse("${array.toString()}").validate[JsArray].get"""
        case _ => throw new NotImplementedError()
      }

      val playJson = ImportDependency("play.api.libs.json._")
      Result(res, Set(playJson))
    }

    implicit val genCaseVal: Generate[CaseVal] = Generate.pureNoFormat { caseVal =>
      s"${caseVal.name}: ${caseVal.className} = ${caseVal.value}"
    }

    implicit val genAttribute: Generate[Attribute] = Generate.pureExpression { attr =>
      attr.toSPValue.generated.result
    }
    implicit def genConditional: Generate[Condition] = Generate.expression { c =>
      val gen = c.proposition.map(_.generated)
      val conditional = Set(ImportDependency(typeName[Condition]))

      Result.foldSeq(gen).map(r => s"Conditional($r)").requires(conditional)
    }

    implicit def genStateId: Generate[IdNode] = Generate.expression(_.id.generated.map(id => s"StateID($id)"))


    private lazy val genConditionNodeBase: Generate[ConditionNode] = Generate.expression {
      case bool: Bool => bool.generated
      case Definition(lhs, rhs) => Result.map2(lhs.generated, rhs.generated)((l, r) => s"Definition($l, $r)")
      case Value(v) => v.generated.map(res => s"Value($res)")
      case x => throw GenException(s"Code can not be generated for the condition value $x.")
    }
    implicit def genConditionNode: Generate[ConditionNode] = {
      genConditionNodeBase.mapDependencies(_ + ImportDependency("codegen.model.ConditionNode._"))
    }


    implicit def genBool: Generate[Bool] = Generate.expression { bool =>
      val res = bool match {
        case and: Bool.And => and.generated
        case or: Bool.Or => or.generated
        case Equal(lhs, rhs) => Result.map2(lhs.generated, rhs.generated)((l, r) => s"Equal($l, $r)")
        case not@Not(_) => not.generated
        // case or: Bool.Or[A, _, B, _] => or.generated
        case Bool.True => Result("True")
        case Bool.False => Result("False")
        case x => throw GenException(s"The Bool $x can not be used for generation.")
      }

      res.requires(Set(ImportDependency(typeName[Bool] + "._")))
    }

    implicit def genConditionDefinition: Generate[ConditionNode.Definition] = Generate.expression {
      case ConditionNode.Definition(lhs, rhs) => lhs.generated.combine(rhs.generated)((l, r) => s"Definition($l, $r)")
    }

    implicit def genBoolAnd: Generate[Bool.And] = Generate.expression {
      case Bool.And(fst, snd) => fst.generated.combine(snd.generated)((f, s) => s"And($f, $s)")
    }

    implicit val genBoolOr: Generate[Bool.Or] = Generate.expression {
      case Bool.Or(fst, snd) => fst.generated.combine(snd.generated)((f, s) => s"Or($f, $s)")
    }

    implicit def genBoolEqual: Generate[Bool.Equal] = Generate.expression {
      case Bool.Equal(lhs, rhs) => lhs.generated.combine(rhs.generated)((l, r) => s"Equal($l, $r)")
    }
    implicit val genBoolNot: Generate[Bool.Not] = Generate.expression {
      case Bool.Not(bool) => bool.generated.map(r => s"Not($r)")
    }

    implicit def genList[A: Generate]: Generate[List[A]] = Generate.expression { seq =>
      val data = seq.map(_.generated)
      val hoists = data.flatMap(_.dependencies)
      val classes = data.map(_.result).mkString("\n")

      Result(classes, hoists.toSet)
    }

    implicit def genSeq[A: Generate]: Generate[Seq[A]] = Generate.expression { seq =>
      seq.toList.generated
    }

    implicit class GenListFolds[A: Generate](list: List[A]) {
      def fill(container: String): Result = {
        Result.foldSeq(list.map(_.generated)).map(r => s"$container($r)")
      }
    }
  }
}