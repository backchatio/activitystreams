package io.backchat.jsonschema
package validator

import com.codahale.jerkson.AST._
import Json._
import scalaz._
import Scalaz._

abstract class MinMaxItemsValidator(val property: String, modifier: String) extends SchemaValidator {

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = value match {
    case _: JInt => value.success
    case _ => ValidationError("The value of %s must be an integer" % property, property).fail
  }

  def validateValue(value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val bound = (schema \ property).valueAs[Int]
    value match {
      case JArray(sz)  =>
        if (isValid(sz.size, bound)) value.success
        else
          ValidationError(
            "There should be at %s %d items in %s, there are %d." % (bound, modifier, property, sz.size), property).fail
      case _ =>
        ValidationError("`%s` is only allowed for arrays." % property, property).fail
    }
  }

  def isValid(left: Int, right: Int): Boolean
}

class MinItemsValidator extends MinMaxItemsValidator("minItems", "least") {
  def isValid(left: Int, right: Int): Boolean = left >= right
}

class MaxItemsValidator extends MinMaxItemsValidator("maxItems", "most") {
  def isValid(left: Int, right: Int): Boolean = left <= right
}