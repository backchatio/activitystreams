package io.backchat
package jsonschema
package validator

import com.codahale.jerkson.AST._
import scalaz._
import Scalaz._

abstract class MinMaxLengthValidator(val property: String, modifier: String) extends SchemaValidator {

  def validateSyntax(value: JValue) = (value \ property) match {
    case _: JInt => value.success
    case _ => ValidationError("The value of %s must be an integer" % property, property).fail
  }

  def validateValue(value: JValue, schema: JValue) = value match {
    case JString(str) =>
      val size = (schema \ property).valueAs[BigInt]
      if (isValid(BigInt(str.length), size))
        value.success
      else
        ValidationError("The value %s at %s %d characters." format (str, modifier, size), property).fail
    case _ =>
      ValidationError("The value must be a string.", property).fail
  }

  def isValid(left: BigInt, right: BigInt): Boolean
}

class MinLengthValidator extends MinMaxLengthValidator("minLength", "least") {
  def isValid(left: BigInt, right: BigInt): Boolean = left >= right
}

class MaxLengthValidator extends MinMaxLengthValidator("maxLength", "least") {
  def isValid(left: BigInt, right: BigInt): Boolean = left <= right
}
