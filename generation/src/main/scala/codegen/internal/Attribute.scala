package codegen.internal

import codegen.internal.Attribute.{AttrDouble, _}
import codegen.internal.definition.{CaseClass, CaseVal}
import codegen.model.Types.SPValue
import play.api.libs.json._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import Generate.Implicits._

import scala.collection.Seq

sealed trait Attribute {
  def toSPValue: SPValue

  /**
    * Creates case classes with the same structure as the Attribute
    * @param key key to work from
    * @return A pair of (CaseVal, List[[[Dependency]]])
    */
  def toCaseVals(key: String, namespace: String = ""): CaseVal = {
    this match {
      case AttrString(str) => CaseVal(key, str)
      case AttrBoolean(bool) => CaseVal(key, bool)
      case AttrInt(n) => CaseVal(key, n)
      case AttrDouble(n) => CaseVal(key, n)
      case AttrLong(n) => CaseVal(key, n)
      case AttrNumber(n) => CaseVal(key, n)
      case AttrList(_) => throw new NotImplementedException

      case AttrListForGen(_, generatedValues, qualifier) =>
        val gen = Result.foldSeq(generatedValues)
        CaseVal.rawQualified(key, s"Seq(${gen.result})", qualifier).addDependencies(gen.dependencies)

      case obj: AttrObject => toCaseVal(key, obj, obj.values, s"${namespace}_${key}_attributes")
      case obj: NamedAttrObject => toCaseVal(key, obj, obj.values, namespace + obj.name + "_attributes")
    }
  }
}

object Attribute {
  /**
    * Used to wrap a JsValue into a JsObject to comply with the Identifiable structure.
    */
  val ValuePrefix = "__value"
  val DomainKey = "domain"

  def fromSPValue(value: SPValue): Attribute = value match {
    case JsBoolean(bool) => AttrBoolean(bool)
    case JsString(v) => AttrString(v)
    case JsNumber(v) => AttrNumber(v)
    case JsFalse => AttrBoolean(false)
    case JsTrue => AttrBoolean(true)
    case JsArray(v) =>
      AttrList(v.map(fromSPValue))
    case JsObject(data) =>
      val isWrappedValue = data.size == 1 && data.head._1 == ValuePrefix

      if (isWrappedValue) fromSPValue(data.head._2)
      else {
        val attrs = data.map { case (k, v) => k -> fromSPValue(v) }.toSeq
        AttrObject(attrs:_*)
      }
  }

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

  def valueToAttribute(v: Any): Attribute = v match {
    case v: String => AttrString(v)
    case v: BigDecimal => AttrNumber(v)
    case v: Int => AttrInt(v)
    case v: Double => AttrDouble(v)
    case v: Float => AttrNumber(BigDecimal(v.toDouble))
    case v: Long => AttrLong(v)
    case v: Boolean => AttrBoolean(v)
    case v: Seq[Any] => AttrList(v.map(valueToAttribute).toIndexedSeq)
    case v: Attribute => v
  }

  def apply(attrs: (String, ValidAttr)*): AttrObject = {
    AttrObject(attrs.map { case (k, value) => k -> (value match {
      case AnyAttr(v) => valueToAttribute(v)
      case typed@TypedAttr(values, name) => AttrListForGen(values.map(valueToAttribute).toIndexedSeq, typed.generatedValues, name)
    })}:_*)
  }

  case class AttrString(value: String) extends Attribute {
    override def toSPValue: SPValue = JsString(value)
  }

  case class AttrInt(value: Int) extends Attribute {
    override def toSPValue: SPValue = JsNumber(value)
  }

  case class AttrDouble(value: Double) extends Attribute {
    override def toSPValue: SPValue = JsNumber(value)
  }

  case class AttrLong(value: Long) extends Attribute {
    override def toSPValue: SPValue = JsNumber(value)
  }

  case class AttrNumber(value: BigDecimal) extends Attribute {
    override def toSPValue: SPValue = JsNumber(value)
  }

  case class AttrBoolean(value: Boolean) extends Attribute {
    override def toSPValue: SPValue = JsBoolean(value)
  }

  case class AttrList(values: IndexedSeq[Attribute]) extends Attribute {
    override def toSPValue: SPValue = JsArray(values.map(valueToAttribute(_).toSPValue))
  }

  object AttrList {
    def apply(values: Any*): AttrList = new AttrList(values.map(valueToAttribute).toIndexedSeq)
  }

  case class AttrListForGen(values: IndexedSeq[Attribute], generatedValues: IndexedSeq[Result], typeQualifier: String) extends Attribute {
    override def toSPValue: SPValue = JsArray(values.map(valueToAttribute(_).toSPValue))
  }

  case class AttrObject(values: IndexedSeq[(String, Attribute)]) extends Attribute {
    def named(name: String): NamedAttrObject = NamedAttrObject(name, values:_*)
    // def nameByKey(key: String): NamedAttrObject = named(s"${ID.validIdentifier(length = 5)}_GenFor_$key")
    def nameByKey(key: String): NamedAttrObject = named(key)

    override def toSPValue: JsObject = JsObject(values.map { case (k, v) => k -> v.toSPValue })

    def get(key: String): Option[Attribute] = values.find(_._1 == key).map(_._2)

  }

  object AttrObject {
    def apply(values: (String, Attribute)*): AttrObject = new AttrObject(values.toIndexedSeq)
    def fromSeq(xs: Seq[(String, JsValue)]): AttrObject = AttrObject(xs.map { case (k, v) => k -> valueToAttribute(v) }:_*)
  }

  case class NamedAttrObject(name: String, values: IndexedSeq[(String, Attribute)]) extends Attribute {
    override def toSPValue: JsObject = JsObject(values.map { case (k, v) => k -> v.toSPValue })
    def get(key: String): Option[Attribute] = values.find(_._1 == key).map(_._2)
  }

  object NamedAttrObject {
    def apply(name: String, values: (String, Attribute)*): NamedAttrObject = new NamedAttrObject(name, values.toIndexedSeq)
  }

  private def toCaseVal(key: String, attribute: Attribute, values: Seq[(String, Attribute)], className: String): CaseVal = {
    val instance = CaseVal.defaultInstance(key, className)

    val caseVals = values.map { case (k, v) => v.toCaseVals(k) }
    val newClassDependency = CaseClassDependency(CaseClass(instance.className, caseVals:_*))

    instance.addDependencies(newClassDependency)
  }
}