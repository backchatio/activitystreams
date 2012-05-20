package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class AdditionalPropertiesValidator extends SchemaValidator {
  val property: String = "additionalProperties"

  def isValid(obj: JObject): List[ValidationNEL[ValidationError, JValue]] = obj.fields collect {
    case JField(_, JObject(Nil) | JNull | JUndefined) => obj.successNel
    case JField(_, s: JObject) => JsonSchema.validateSyntax(s)
    case JField(nm, _) => ValidationError("The value of `%s` is invalid." % nm, property).failNel
  }

  def validateSyntax(value: JValue): ValidationNEL[ValidationError, JValue] = value \ property match {
    case _: JBoolean | JNull | JUndefined | JObject(Nil) => value.successNel
    case m: JObject => JsonSchema.flattenErrors(isValid(m))
    case _ => ValidationError("There are some problems with the property definitions.", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, JValue] =
    schema \ property match {
      case JBoolean(false) =>
        val fields = schema \ "properties" match {
          case JObject(f) => f.map(_.name)
          case _ => Nil
        }
        val regexes = schema \ "patternProperties" match {
          case JObject(f) => f.map(_.name)
          case _ => Nil
        }
        val instanceFields = value \ fieldName match {
          case JObject(f) => f.map(_.name)
          case _ => Nil
        }
        val allFields = fields ++ instanceFields.filter(f => regexes.any(EcmaRegex.matches(_, f)))
        val remaining = instanceFields filterNot allFields.contains
        if (remaining.isEmpty)
          value.successNel
        else
          ValidationError(
            "%s are not permitted. These properties shouldn't exist: %s" % (property.underscore.humanize, remaining.mkString(",")),
            fieldName).failNel
      case _ => value.successNel
    }
}
