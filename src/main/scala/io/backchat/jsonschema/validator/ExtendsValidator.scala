package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class ExtendsValidator extends SchemaValidator {
  val property: String = "extends"


  private def isValid(value: JValue): Validation[ValidationError, JValue] = value match {
    case JArray(Nil) | JNull | JUndefined => value.success
    case s: JString if JsonSchema.format("uri").get(s).isSuccess => value.success
    case m: JObject if JsonSchema.validateSyntax(m).isSuccess => value.success
    case JArray(objects) if objects forall (isValid(_).isSuccess) => value.success
    case _ => ValidationError("The extends property is invalid", property).fail
  }
  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = isValid(value \ property)

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
