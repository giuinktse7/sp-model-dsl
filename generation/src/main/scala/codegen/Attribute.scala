package codegen

import codegen.definition.{CaseClass, CaseVal}
import codegen.model.Types.SPValue
import play.api.libs.json._
import Generate.Implicits._
import codegen.Attribute._

import scala.collection.Seq

sealed trait Attribute {
  def toSPValue: SPValue


  /**
    * Creates case classes with the same structure as the Attribute
    * @param key key to work from
    * @return A pair of (CaseVal, List[[[Dependency]]])
    */
  def toGen(key: String): CaseVal = {
    this match {
      case AttrString(str) => CaseVal(key, str)
      case AttrBoolean(bool) => CaseVal(key, bool)
      case AttrNumber(n) => CaseVal(key, n)
      case AttrList(_, generatedValues, qualifier) =>
        val result = s"Seq(${generatedValues.map(_.result).mkString(", ")})"
        val deps = generatedValues.flatMap(_.dependencies).toSet

        CaseVal.rawQualified(key, result, qualifier).addDependencies(deps)
      // case obj: AttrObject => toCaseVal(key, obj, obj.values, s"${ID.validIdentifier(length = 5)}_GenFor_$key")
      case obj: AttrObject => toCaseVal(key, obj, obj.values, s"GeneratedAttributes_$key")
      case obj: NamedAttrObject => toCaseVal(key, obj, obj.values, obj.name)
    }
  }
}

object Attribute {
  import scala.reflect.runtime.universe.typeOf

  implicit def listToValid[A: Manifest: Generate](list: Seq[A]): ValidAttr = TypedAttr(list, s"Seq[${typeOf[A].typeSymbol.fullName}]")
  implicit def valueToValid(value: Any): ValidAttr = AnyAttr(value)

  /**
    * Required to allow code generation to access generic types of eg. Lists
    */
  sealed trait ValidAttr
  case class AnyAttr(value: Any) extends ValidAttr
  case class TypedAttr[A: Generate](values: Seq[A], name: String) extends ValidAttr {
    import Generate.GenOps
    val generatedValues: IndexedSeq[Result] = values.map(_.generated).toIndexedSeq
  }

  def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

  def testCreate(attrs: (String, ValidAttr)*): String = attrs.map {
    case (k, AnyAttr(v)) => v.toString
    case (k, Attribute.TypedAttr(_, name))  => name
  }.mkString(", ")

  def valueToAttribute(v: Any): Attribute = v match {
    case v: String => AttrString(v)
    case v: BigDecimal => AttrNumber(v)
    case v: Int => AttrNumber(BigDecimal(v))
    case v: Double => AttrNumber(BigDecimal(v))
    case v: Float => AttrNumber(BigDecimal(v))
    case v: Long => AttrNumber(BigDecimal(v))
    case v: Boolean => AttrBoolean(v)
    case v: Attribute => v
  }

  def apply(attrs: (String, ValidAttr)*): AttrObject = {
    AttrObject(attrs.map { case (k, value) => k -> (value match {
      case AnyAttr(v) => valueToAttribute(v)
      case typed@TypedAttr(values, name) => AttrList(values.toIndexedSeq, typed.generatedValues, name)
    })}:_*)
  }

  case class AttrString(value: String) extends Attribute {
    override def toSPValue: SPValue = JsString(value)
  }

  // TODO Should perhaps not always use BigDecimal
  case class AttrNumber(value: BigDecimal) extends Attribute {
    override def toSPValue: SPValue = JsNumber(value)
  }
  case class AttrBoolean(value: Boolean) extends Attribute {
    override def toSPValue: SPValue = JsBoolean(value)
  }

  case class AttrList[A](values: IndexedSeq[A], generatedValues: IndexedSeq[Result], typeQualifier: String) extends Attribute {
    override def toSPValue: SPValue = JsArray(values.map(valueToAttribute(_).toSPValue))
  }

  case class AttrObject(values: (String, Attribute)*) extends Attribute {
    def named(name: String): NamedAttrObject = NamedAttrObject(name, values:_*)
    // def nameByKey(key: String): NamedAttrObject = named(s"${ID.validIdentifier(length = 5)}_GenFor_$key")
    def nameByKey(key: String): NamedAttrObject = named(s"Generated_$key")

    override def toSPValue: JsObject = JsObject(values.map { case (k, v) => k -> v.toSPValue })
  }
  case class NamedAttrObject(name: String, values: (String, Attribute)*) extends Attribute {
    override def toSPValue: JsObject = JsObject(values.map { case (k, v) => k -> v.toSPValue })
  }

  private def toCaseVal(key: String, attribute: Attribute, values: Seq[(String, Attribute)], className: String): CaseVal = {
    val instance = CaseVal.defaultInstance(key, className)

    val caseVals = values.map { case (k, v) => v.toGen(k) }
    val newClassDependency = CaseClassDependency(CaseClass(instance.className, caseVals:_*))

    instance.addDependencies(newClassDependency)
  }
}