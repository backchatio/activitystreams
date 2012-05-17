package io.backchat
package jsonschema
package validator

import scalaz._
import Scalaz._
import io.backchat.jsonschema.ValidationError
import com.codahale.jerkson.AST._
import Json._

class DivisibleByValidator extends SchemaValidator {
  val property = "divisibleBy"

  def validateSyntax(value: JValue) = (value \ property) match {
    case JInt(i) if i == 0 => ValidationError("Can't divide by 0.", property).fail
    case JInt(i) => value.success
    case _ => ValidationError("The value of %s must be an integer." % property, property).fail
  }

  def validateValue(value: JValue, schema: JValue) = {
    value match {
      case JInt(i) if ((i % (schema \ property).valueAs[BigInt]) == 0) =>
         value.success
      case JFloat(i) if ((i % (schema \ property).valueAs[BigInt].toLong) == 0) =>
        value.success
      case JDecimal(i) if ((i % (schema \ property).valueAs[BigInt].toLong) == 0) =>
        value.success
      case _ =>
        ValidationError(
          "The value %s is not divisible by %s" format (generate(value), generate(schema \ property)), property).fail
    }
  }
}
