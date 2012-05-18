package io.backchat.jsonschema
package validator

import com.codahale.jerkson.AST._
import Json._
import scalaz._
import Scalaz._

class UniqueItemsValidator extends SchemaValidator {
  val property: String = "uniqueItems"

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = value match {
    case _: JBoolean | JUndefined | JNull => value.success
    case _ => ValidationError("The `%s` property needs to be a boolean.", property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val directive = (schema \ property) match {
      case JBoolean(v) => v
      case _ => false
    }
    value \ fieldName match {
      case _ if !directive => value.success
      case JArray(Nil) => value.success
      case JArray(items) if items.toSet.size == items.size => value.success
      case JArray(_) => ValidationError("The items in %s are not unique." % property, fieldName).fail
      case _ => ValidationError("`%s` is only allowed for arrays." % property, fieldName).fail
    }
  }
}
