package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class ExtendsValidator extends SchemaValidator {
  val property: String = "extends"


  private def isValid(value: JValue): ValidationNEL[ValidationError, JValue] = value match {
    case JArray(Nil) | JNull | JUndefined => value.successNel
    case s: JString if JsonSchema.format("uri").get(s).isSuccess => value.successNel
    case m: JObject if JsonSchema.validateSyntax(m).isSuccess => value.successNel
    case JArray(objects) if objects forall (isValid(_).isSuccess) => value.successNel
    case _ => ValidationError("The extends property is invalid", property).failNel
  }
  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = isValid(value \ property)

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    value.successNel
}
