package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

abstract class StringValidator extends SchemaValidator {
  def validateSyntax(value: Json.JValue): Validation[ValidationError, Json.JValue] = value \ property match {
    case _: JString | JNull | JUndefined => value.success
    case _ => ValidationError("The value of title must be a string.", property).fail
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: Json.JValue): Validation[ValidationError, Json.JValue] =
    value.success

}

class TitleValidator extends StringValidator {
  val property: String = "title"

}
class DescriptionValidator extends StringValidator {
  val property: String = "description"

}
