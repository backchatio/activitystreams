package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class ItemsValidator extends SchemaValidator {
  val property: String = "items"

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined | JObject(Nil) => value.success
    case m: JObject if JsonSchema.validateSyntax(m).isSuccess => value.success
    case JArray(objects) if objects forall (JsonSchema.validateSyntax(_).isSuccess) => value.success
    case _ => ValidationError("The items property is invalid.", property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
}
