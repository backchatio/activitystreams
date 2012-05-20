package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class PropertiesValidator extends SchemaValidator {
  val property: String = "properties"

  def isValid(obj: JObject): List[ValidationNEL[ValidationError, JValue]] = obj.fields collect {
    case JField(_, JObject(Nil) | JNull | JUndefined) => obj.successNel
    case JField(_, s: JObject) => JsonSchema.validateSyntax(s)
    case JField(nm, _) => ValidationError("The value of `%s` is invalid." % nm, property).failNel
  }

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.success
    case m: JObject => JsonSchema.flattenErrors(isValid(m))
    case _ => ValidationError("There are some problems with the property definitions.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    value.success
}
