package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class DependenciesValidator extends SchemaValidator {
  val property: String = "dependencies"

  private def validArray(items: List[JValue]) = items.nonEmpty && items.forall(_.getClass == classOf[JString])


  private def isValid(value: JValue): Validation[ValidationError, JValue] = value match {
    case s: JString if JsonSchema.format("uri").get(s).isSuccess => value.success
    case m: JObject if JsonSchema.validateSyntax(m).isSuccess => value.success
    case JArray(objects) if validArray(objects) => value.success
    case _ => ValidationError("The dependencies property is invalid", property).fail
  }

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
      case JNull | JUndefined => value.success
      case JObject(fields) if fields forall (f => isValid(f.value).isSuccess) => value.success
      case _ => ValidationError("The dependencies property is invalid", property).fail
    }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
