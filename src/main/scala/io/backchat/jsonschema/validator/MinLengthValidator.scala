package io.backchat
package jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

abstract class MinMaxLengthValidator(val property: String, modifier: String) extends SchemaValidator {

  def validateSyntax(value: JValue) = value \ property match {
    case JInt(v) if v >= 0 && v <= Int.MaxValue => value.success
    case _ => ValidationError("The value of %s must be an integer" % property, property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue) = (value \ fieldName) match {
    case JString(str) =>
      val size = (schema \ property).valueAs[BigInt]
      if (isValid(BigInt(str.length), size))
        value.success
      else
        ValidationError("The value %s at %s %d characters." format (str, modifier, size), fieldName).fail
    case _ =>
      ValidationError("The value must be a string.", fieldName).fail
  }

  def isValid(left: BigInt, right: BigInt): Boolean
}

class MinLengthValidator extends MinMaxLengthValidator("minLength", "least") {
  def isValid(left: BigInt, right: BigInt): Boolean = left >= right
}

class MaxLengthValidator extends MinMaxLengthValidator("maxLength", "least") {
  def isValid(left: BigInt, right: BigInt): Boolean = left <= right
}
