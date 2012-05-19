package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class PatternPropertiesValidator extends SchemaValidator {
  val property: String = "patternProperties"

  def isValid(fields: List[JField]): Boolean = fields forall {
    case JField(nm, JNull | JUndefined | JObject(Nil)) if EcmaRegex.isValid(nm) => true
    case JField(nm, m: JObject) if EcmaRegex.isValid(nm) && JsonSchema.validateSyntax(m).isSuccess => true
    case _ => false
  }

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.success
    case m: JObject if isValid(m.fields) => value.success // TODO: Add better error messages
    case _ => ValidationError("There are some problems with the pattern property definitions.", property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
