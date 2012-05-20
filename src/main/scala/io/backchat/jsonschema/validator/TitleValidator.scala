package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

abstract class StringValidator extends SchemaValidator {
  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case _: JString | JNull | JUndefined => value.successNel
    case _ => ValidationError("The value of title must be a string.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    value.successNel

}

class TitleValidator extends StringValidator {
  val property: String = "title"

}
class DescriptionValidator extends StringValidator {
  val property: String = "description"

}
