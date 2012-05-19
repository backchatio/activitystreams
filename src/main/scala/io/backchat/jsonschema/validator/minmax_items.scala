package io.backchat.jsonschema
package validator

import Json._
import scalaz._
import Scalaz._

abstract class MinMaxItemsValidator(val property: String, modifier: String) extends SchemaValidator {

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = value \ property match {
    case _: JInt => value.success
    case _ => ValidationError("The value of %s must be an integer" % property, property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val bound = (schema \ property).valueAs[Int]
    value \ fieldName match {
      case JArray(sz)  =>
        if (isValid(sz.size, bound)) value.success
        else
          ValidationError(
            "There should be at %s %d items in %s, there are %d." % (bound, modifier, property, sz.size), fieldName).fail
      case _ =>
        ValidationError("`%s` is only allowed for arrays." % property, fieldName).fail
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