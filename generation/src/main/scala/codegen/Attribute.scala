package codegen

import codegen.definition.{CaseClass, CaseVal}
import codegen.model.Types.{ID, SPValue}
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString}
import Generate.Implicits._
import codegen.Attribute._

sealed trait Attribute {
  def toSPValue: SPValue

  def parse(key: String): (CaseVal, List[CaseClass]) = {
    this match {
      case AttrString(str) => (CaseVal(key, str), List())
      case AttrBoolean(bool) => (CaseVal(key, bool), List())
      case AttrNumber(n) => (CaseVal(key, n), List())
      case obj: AttrObject => parseAttributeObject(key, obj, obj.values, s"${ID.validIdentifier(length = 5)}_GenFor_$key")
      case obj: NamedAttrObject => parseAttributeObject(key, obj, obj.values, obj.name)
    }
  }
}

object Attribute {
  def apply(attrs: (String, Any)*): AttrObject = {
    AttrObject(attrs.map { case (k, value) =>
      val newValue = value match {
        case v: String => AttrString(v)
        case v: BigDecimal => AttrNumber(v)
        case v: Int => AttrNumber(BigDecimal(v))
        case v: Double => AttrNumber(BigDecimal(v))
        case v: Float => AttrNumber(BigDecimal(v))
        case v: Long => AttrNumber(BigDecimal(v))
        case v: Boolean => AttrBoolean(v)
        case v: Attribute => v
      }

      k -> newValue
    }:_*)
  }

  case class AttrString(value: String) extends Attribute {
    override def toSPValue: SPValue = JsString(value)
  }
  case class AttrNumber(value: BigDecimal) extends Attribute {
    override def toSPValue: SPValue = JsNumber(value)
  }
  case class AttrBoolean(value: Boolean) extends Attribute {
    override def toSPValue: SPValue = JsBoolean(value)
  }

  case class AttrObject(values: (String, Attribute)*) extends Attribute {
    def named(name: String): NamedAttrObject = NamedAttrObject(name, values:_*)
    def nameByKey(key: String): NamedAttrObject = named(s"${ID.validIdentifier(length = 5)}_GenFor_$key")

    override def toSPValue: JsObject = JsObject(values.map { case (k, v) => k -> v.toSPValue })
  }
  case class NamedAttrObject(name: String, values: (String, Attribute)*) extends Attribute {
    override def toSPValue: JsObject = JsObject(values.map { case (k, v) => k -> v.toSPValue })
  }

  def parseAttributeObject(key: String, attribute: Attribute, values: Seq[(String, Attribute)], className: String): (CaseVal, List[CaseClass]) = {
    val instance = CaseVal.instance(key, className)
    val initialFoldValue = (List[CaseVal](), List[CaseClass]())

    val (caseVals, caseClasses) = values
      .map { case (k, v) => v.parse(k) }
      .foldLeft(initialFoldValue) { case ((cvs, prevHoists), (cv, hoists)) =>
        (cv :: cvs, (hoists ++ prevHoists).distinct)
      }

    val newClass = CaseClass(instance.className, caseVals:_*)
    (instance, newClass :: caseClasses)
  }
}