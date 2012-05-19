package io.backchat.jsonschema
package validator

import Json.JValue
import scalaz.Validation

trait SchemaValidator {
  def property: String
  def validateSyntax(value: JValue): Validation[ValidationError, JValue]
  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue]
}