package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class DependenciesValidator extends SchemaValidator {
  val property: String = "dependencies"

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
