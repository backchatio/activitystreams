package io.backchat.jsonschema
package validator

import Json._
import scalaz._
import Scalaz._

class UniqueItemsValidator extends SchemaValidator {
  val property: String = "uniqueItems"

  def validateSyntax(value: JValue): ValidationNEL[ValidationError, JValue] = value \ property match {
    case _: JBoolean | JUndefined | JNull => value.success
    case _ => ValidationError("The `%s` property needs to be a boolean.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): ValidationNEL[ValidationError, JValue] = {
    val directive = schema \ property match {
      case JBoolean(v) => v
      case _ => false
    }
    value \ fieldName match {
      case _ if !directive => value.success
      case JArray(Nil) => value.success
      case JArray(items) if items.toSet.size == items.size => value.success
      case JArray(_) => ValidationError("The items in %s are not unique." % property, fieldName).failNel
      case _ => ValidationError("`%s` is only allowed for arrays." % property, fieldName).failNel
    }
  }
}
