package io.backchat.jsonschema
package validator

import com.codahale.jerkson.AST._
import Json._
import scalaz._
import Scalaz._

class PatternValidator extends SchemaValidator {
  val property: String = "pattern"

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = (value \ property) match {
    case JString(re) if EcmaRegex.isValid(re) => value.success
    case JString(re) => ValidationError("Regex %s is invalid.", property).fail
    case _ =>  ValidationError("Regex is of an invalid type.", property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val regex = (schema \ property).valueAs[String]
    value \ fieldName match {
      case JString(v) if EcmaRegex.matches(regex, v) => value.success
      case JString(v) => ValidationError("The value %s doesn't match regex: %s." % (v, regex), fieldName).fail
      case JNull =>
        ValidationError("The value null doesn't match regex: %s." % regex, fieldName).fail
      case JUndefined =>
        ValidationError("Missing value doesn't match regex: %s." % regex, fieldName).fail
      case m =>
        ValidationError(
          "The value %s of type %s doesn't match regex: %s." % (generate(m), TypeValidator.jsTypeNameFor(m), regex),
          fieldName).fail
    }

  }
}
