package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class AdditionalItemsValidator extends SchemaValidator {
  val property: String = "additionalItems"
  override val fieldName: String = "items"

  def isValid(obj: JObject): List[ValidationNEL[ValidationError, JValue]] = obj.fields collect {
    case JField(_, JObject(Nil) | JNull | JUndefined) => obj.successNel
    case JField(_, s: JObject) => JsonSchema.validateSyntax(s)
    case JField(nm, _) => ValidationError("The value of `%s` is invalid." % nm, property).failNel
  }

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case _: JBoolean | JNull | JUndefined | JObject(Nil) => value.successNel
    case m: JObject => JsonSchema.flattenErrors(isValid(m))
    case _ => ValidationError("There are some problems with the property definitions.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] =
    schema \ property match {
      case JBoolean(false) =>
        schema \ "items" match {
          case JArray(lst) =>
            val size = value \ fieldName match {
              case JObject(fields) => fields.size
              case JArray(items) => items.size
              case _ => 0
            }
            if (size > lst.size) ValidationError("%s are not permitted." % property, fieldName).failNel
            else value.successNel
          case _ => value.successNel
        }
      case _ => value.successNel
    }
}
