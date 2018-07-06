package codegen.model

import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

import scala.util.Try

object Types {
  type SPValue = JsValue
  object SPValue {
    def apply[T](v: T)(implicit fjs: Writes[T]): SPValue = {
      Json.toJson(v)
    }

    def fromJson(json: String): Try[SPValue] = {
      Try { Json.parse(json) }
    }
    def empty: SPValue = JsObject.empty
  }
  type AttributeMap = JsObject

  object AttributeMap {
    def apply(pairs: (String, JsValueWrapper)*): AttributeMap = Json.obj(pairs:_*)
  }

  type ID = java.util.UUID

  object ID {
    def apply(): java.util.UUID = java.util.UUID.randomUUID()
  }

  implicit def stringToSPValue(x: String): SPValue = SPValue(x)
  implicit def intToSPValue(x: Int): SPValue = SPValue(x)
  implicit def boolToSPValue(x: Boolean): SPValue = SPValue(x)
}