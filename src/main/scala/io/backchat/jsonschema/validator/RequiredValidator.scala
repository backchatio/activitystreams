package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class RequiredValidator extends SchemaValidator {
  val property: String = "required"

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case _: JBoolean | JNull | JUndefined => value.successNel
    case _ => ValidationError("The `%s` can only contain boolean values." % property, property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    value.successNel
}
