package codegen

import codegen.definition.CaseClass
import Generate.GenOps
import Generate.Implicits.genCaseClass



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
  val PackageOrder = 0
  val ImportOrder = 1
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

case class PackageDependency(name: String) extends Dependency {
  override def code: String = s"package $name"
  override def dependencies: Set[Dependency] = Set()
  override def comparePriority(that: Dependency): Int = order.compareTo(that.order)
  override def order: Int = Dependency.PackageOrder
}