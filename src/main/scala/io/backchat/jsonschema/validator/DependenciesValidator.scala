package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class DependenciesValidator extends SchemaValidator {
  val property: String = "dependencies"

  private def validArray(items: List[JValue]) = items.nonEmpty && items.forall(_.getClass == classOf[JString])

  private def isValid(value: JField): ValidationNEL[ValidationError, JValue] = value match {
    case JField(_, s: JString) if JsonSchema.format("uri").get(s).isSuccess => value.success
    case JField(_, m: JObject) if JsonSchema.validateSyntax(m).isSuccess => value.success
    case JField(_, JArray(objects)) if validArray(objects) => value.success
    case JField(name, _) => ValidationError("The dependencies `%s` property is invalid" % name, property).failNel
  }

  def validateSyntax(value: Json.JValue): ValidationNEL[ValidationError, Json.JValue] = value \ property match {
    case JNull | JUndefined => value.successNel
    case JObject(fields)  => JsonSchema.flattenErrors(fields map (isValid(_)))
    case _ => ValidationError("The dependencies property is invalid", property).failNel
  }

  def validateValue(fieldName: String, value: JValue, schema: JsonSchema): ValidationNEL[ValidationError, Json.JValue] = {
    schema \ property match {
      case JObject(fields) =>
        JsonSchema.flattenErrors(fields map (validateItem(fieldName, value \ fieldName, _))) map (_ => value)
      case _ => ValidationError("The dependencies property is invalid", property).failNel // we should really never get here
    }
  }

  private def validateItem(parentField: String, value: JValue, field: JField): ValidationNEL[ValidationError, JValue] = {
    val instanceFields = value match {
      case JObject(fields) => fields.map(_.name)
      case _ => Nil
    }
    field match {
      case JField(nm, JString(s)) => fieldNamesValidate(parentField+"."+nm, instanceFields, List(s)) map (_ => value)
      case JField(nm, JArray(s)) => fieldNamesValidate(parentField+"."+nm, instanceFields, s map (_.valueAs[String])) map (_ => value)
      case JField(nm, JObject(fields)) => value.successNel
      case JField(nm, _) => ValidationError("The `%s.%s` property is invalid." % (parentField, nm), property).failNel
    }
  }

  private def fieldNamesValidate(fieldName: String, instanceFields: List[String], lst: List[String]): ValidationNEL[ValidationError, List[String]] = {
    val missing = lst filterNot instanceFields.contains
    if (missing.isEmpty) instanceFields.success
    else {
      ValidationError("The following fields %s are missing according to the property dependencies." % missing, fieldName).failNel
    }
  }
}
