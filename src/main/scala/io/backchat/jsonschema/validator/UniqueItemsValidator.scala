package io.backchat.jsonschema
package validator

import com.codahale.jerkson.AST._
import Json._
import scalaz._
import Scalaz._
import com.codahale.jerkson.Diff

class UniqueItemsValidator extends SchemaValidator {
  val property: String = "uniqueItems"

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = value match {
    case _: JBoolean | JUndefined | JNull => value.success
    case _ => ValidationError("The `%s` property needs to be a boolean.", property).fail
  }

  def validateValue(value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val directive = (schema \ property) match {
      case JBoolean(v) => v
      case _ => false
    }
    value match {
      case _ if !directive => value.success
      case JArray(Nil) => value.success
      case JArray(items) if items.toSet.size == items.size => value.success
      case JArray(_) => ValidationError("The items in are not unique.", property).fail
      case _ => ValidationError("`%s` is only allowed for arrays." % property, property).fail
    }
  }
}
