package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class DependenciesValidator extends SchemaValidator {
  val property: String = "dependencies"

  private def validArray(items: List[JValue]) = items.nonEmpty && items.forall(_.getClass == classOf[JString])

  private def isValid(value: JField): ValidationNEL[ValidationError, JValue] = value match {
    case JField(_, s: JString) if JsonSchema.format("uri").get(s).isSuccess => value.success
    case JField(_, m: JObject) if JsonSchema.validateSyntax(m).isSuccess => value.success
    case JField(_, JArray(objects)) if validArray(objects) => value.success
    case JField(name, _) => ValidationError("The dependencies `%s` property is invalid" % name, property).failNel
  }

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
      case JNull | JUndefined => value.successNel
      case JObject(fields) if fields forall (isValid(_).isSuccess) => value.successNel
      case _ => ValidationError("The dependencies property is invalid", property).failNel
    }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): ValidationNEL[ValidationError, Json.JValue] =
    value.success
}
