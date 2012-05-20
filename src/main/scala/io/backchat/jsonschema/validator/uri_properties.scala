package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

abstract class UriPropertyValidator extends SchemaValidator {

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.successNel
    case s: JString if JsonSchema.format("uri").get(s).isSuccess => value.successNel
    case _ => ValidationError("The value of `%s` must be a URI." % property, property).failNel
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): ValidationNEL[ValidationError, Json.JValue] =
    value.successNel
}

class IdValidator extends UriPropertyValidator {
  val property: String = "id"
}
class JsonRefValidator extends UriPropertyValidator {
  val property: String = "$ref"
}
class BaseSchemaValidator extends UriPropertyValidator {
  val property: String = "$schema"
}