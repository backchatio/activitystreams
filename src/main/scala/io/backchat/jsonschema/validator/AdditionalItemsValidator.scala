package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class AdditionalItemsValidator extends SchemaValidator {
  val property: String = "additionalItems"

  def isValid(fields: List[JField]): Boolean = fields forall {
    case JField(_, JObject(Nil) | JNull | JUndefined) => true
    case JField(_, s: JObject) => JsonSchema.validateSyntax(s).isSuccess
    case _ => false
  }

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case _: JBoolean | JNull | JUndefined | JObject(Nil) => value.success
    case JObject(fields) if isValid(fields) => value.success // TODO: Add better error messages
    case _ =>  ValidationError("There are some problems with the property definitions.", property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
