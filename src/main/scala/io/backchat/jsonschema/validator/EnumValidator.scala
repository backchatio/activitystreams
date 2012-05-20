package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class EnumValidator extends SchemaValidator {
  val property: String = "enum"

  def validateSyntax(value: JValue): ValidationNEL[ValidationError, JValue] = value \ property match {
    case JArray(it) if it.size == it.distinct.size => value.successNel
    case _ => ValidationError("The value of `%s` must be an array." % property, property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): ValidationNEL[ValidationError, JValue] = {
    val avj = schema \ property
    val allowableValues = avj match {
      case JArray(v) => v
      case _ => Nil
    }


    if (allowableValues contains (value \ fieldName)) value.successNel
    else
      ValidationError(
        "The value %s was not contained in %s" % (generate(value), generate(avj)), fieldName).failNel
  }
}
