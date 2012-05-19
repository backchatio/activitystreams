package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

abstract class UriPropertyValidator extends SchemaValidator {

  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.success
    case s: JString if JsonSchema.format("uri").get(s).isSuccess => value.success
    case _ => ValidationError("The value of `%s` must be a URI." % property, property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success
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