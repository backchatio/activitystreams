package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class DependenciesValidator extends SchemaValidator {
  val property: String = "dependencies"

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case _: JString | _: JArray | JNull | JUndefined => value.success
    case m: JObject if JsonSchema.validateSyntax(m).isSuccess => value.success
    case _ => ValidationError("The dependencies property is invalid", property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
