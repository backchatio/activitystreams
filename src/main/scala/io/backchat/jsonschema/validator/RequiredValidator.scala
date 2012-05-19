package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class RequiredValidator extends SchemaValidator {
  val property: String = "required"

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case _: JBoolean | JNull | JUndefined => value.success
    case _ => ValidationError("The `%s` can only contain boolean values." % property, property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
