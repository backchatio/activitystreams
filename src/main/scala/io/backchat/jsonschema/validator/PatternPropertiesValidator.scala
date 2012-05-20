package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class PatternPropertiesValidator extends SchemaValidator {
  val property: String = "patternProperties"

  def isValid(obj: JObject): List[ValidationNEL[ValidationError, JValue]] = obj.fields collect {
    case JField(nm, JNull | JUndefined | JObject(Nil)) if EcmaRegex.isValid(nm) => obj.success
    case JField(nm, JNull | JUndefined | JObject(Nil)) =>
      ValidationError("The regex `%s` as property matcher is invalid." % nm, property).failNel
    case JField(nm, m: JObject) =>
      if (EcmaRegex.isValid(nm)) {
        JsonSchema.validateSyntax(m)
      } else ValidationError("The regex `%s` as property matcher is invalid." % nm, property).failNel
    case JField(nm, _) => ValidationError("The property `%s` is invalid." % nm, property).failNel
  }

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.successNel
    case m: JObject =>
      val validations = isValid(m)
      if (validations forall (_.isSuccess))
        value.successNel
      else JsonSchema.flattenErrors(validations)
    case _ => ValidationError("There are some problems with the pattern property definitions.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    value.success
}
