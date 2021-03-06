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
    def validIdentifier(length: Int): String = "Generated_" + ID().toString.take(length).replaceAll("-", "_")
  }
}