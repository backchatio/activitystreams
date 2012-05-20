package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

trait SchemaValidator {
  def property: String
  def validateSyntax(value: JValue): ValidationNEL[ValidationError, JValue]
  def validateValue(fieldName: String, value: JValue, schema: JValue): ValidationNEL[ValidationError, JValue]
}