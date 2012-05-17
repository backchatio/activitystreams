package io.backchat
package jsonschema
package validator

import com.codahale.jerkson.AST._
import com.codahale.jerkson.Json._
import scalaz._
import Scalaz._

class MinLengthValidator extends SchemaValidator {
  val property = "minLength"

  def validateSyntax(value: JValue) = (value \ property) match {
    case _: JInt => value.success
    case _ => ValidationError("The value of %s must be an integer" % property, property).fail
  }

  def validateValue(value: JValue, schema: JValue) = value match {
    case JString(str) =>
      val size = (schema \ property).valueAs[BigInt]
      if (str.size >= size)
        value.success
      else
        ValidationError("The value %s is too short, it requires at least %d characters." format (str, size), property).fail
    case _ =>
      ValidationError("The value must be a string.", property).fail
  }
}
