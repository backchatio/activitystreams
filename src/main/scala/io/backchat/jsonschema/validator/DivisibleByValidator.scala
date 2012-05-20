package io.backchat
package jsonschema
package validator

import scalaz._
import Scalaz._
import io.backchat.jsonschema.ValidationError
import Json._

class DivisibleByValidator extends SchemaValidator {
  val property = "divisibleBy"

  def validateSyntax(value: JValue) = value \ property match {
    case JInt(i) if i == 0 => ValidationError("Can't divide by 0.", property).failNel
    case JInt(i) => value.successNel
    case _ => ValidationError("The value of %s must be an integer." % property, property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema) = {
    value \ fieldName match {
      case JInt(i) if ((i % (schema \ property).valueAs[BigInt]) == 0) =>
         value.successNel
      case JFloat(i) if ((i % (schema \ property).valueAs[BigInt].toLong) == 0) =>
        value.successNel
      case JDecimal(i) if ((i % (schema \ property).valueAs[BigInt].toLong) == 0) =>
        value.successNel
      case _ =>
        ValidationError(
          "The value %s is not divisible by %s" format (generate(value), generate(schema \ property)), fieldName).failNel
    }
  }
}
