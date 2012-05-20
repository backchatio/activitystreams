package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class ItemsValidator extends SchemaValidator {
  val property: String = "items"

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined | JObject(Nil) => value.successNel
    case m: JObject if JsonSchema.validateSyntax(m).isSuccess => value.successNel
    case JArray(objects) if objects forall (JsonSchema.validateSyntax(_).isSuccess) => value.successNel
    case _ => ValidationError("The items property is invalid.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    value.success
}
