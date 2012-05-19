package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class PropertiesValidator extends SchemaValidator {
  val property: String = "properties"

  private def isValid(fields: List[JField]) = fields forall {
    case JField(_, JObject(Nil) | JNull | JUndefined) => true
    case JField(_, s: JObject) => JsonSchema.validateSyntax(s).isSuccess
    case _ => false
  }

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.success
    case m: JObject if isValid(m.fields) => value.success // TODO: Add better error messages
    case _ => ValidationError("There are some problems with the property definitions.", property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
