package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class EnumValidator extends SchemaValidator {
  val property: String = "enum"

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = value \ property match {
    case JArray(_) => value.success
    case _ => ValidationError("The value of `%s` must be an array." % property, property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val avj = schema \ property
    val allowableValues = avj match {
      case JArray(v) => v
      case _ => Nil
    }

    if (allowableValues contains (value \ fieldName)) value.success
    else
      ValidationError(
        "The value %s was not contained in %s" % (generate(value), generate(avj)), fieldName).fail
  }
}
