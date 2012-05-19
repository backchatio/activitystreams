package io.backchat.jsonschema
package validator

import io.backchat.jsonschema.ValidationError
import scalaz._
import Scalaz._
import annotation.tailrec
import Json._

object TypeValidator {
  val types: Seq[String] = List("string", "number", "integer", "boolean", "object", "array", "null", "any")

  @tailrec
  def jsTypeNameFor(v: JValue): String = v match {
    case _: JArray => "array"
    case _: JString => "string"
    case JNull | null | JUndefined => "null"
    case _: JInt => "integer"
    case _: JFloat | _: JDecimal => "number"
    case _: JObject => "object"
    case _: JBoolean => "boolean"
    case JField(_, jv) => jsTypeNameFor(jv)
  }
  @tailrec
  def jsTypeNamesFor(v: JValue): Seq[String] = (v match {
    case _: JArray => Seq("array")
    case _: JString => Seq("string")
    case JNull | null | JUndefined => Seq("null")
    case _: JInt => Seq("integer", "number")
    case _: JFloat | _: JDecimal => Seq("number")
    case _: JObject => Seq("object")
    case _: JBoolean => Seq("boolean")
    case JField(_, jv) => jsTypeNamesFor(jv)
  })
}
class TypeValidator extends SchemaValidator {
  import TypeValidator._
  val property = "type"

  private def allStrings(eles: List[JValue]) = eles.nonEmpty && {
    eles forall {
      case JString(s) if "^[A-Za-z]+$".r.findFirstIn(s).isDefined => true
      case _: JObject => true
      case _ => false
    }
  }

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] = value match {
    case JArray(elements) if allStrings(elements) => value.success
    case JString(s) if "^[A-Za-z]+$".r.findFirstIn(s).isDefined => value.success
    case _: JObject => value.success
    case _ => ValidationError("The type of the `"+property+"` property is valid", property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue] =
    validateType(fieldName, value, schema \ property)

  protected def validateType(fieldName: String, value: JValue, schemaType: JValue): Validation[ValidationError, JValue] = {
    val extracted = value \ fieldName
    schemaType  match {
      case JArray(elements) if elements.nonEmpty =>
        val validations = elements.map(validateType(fieldName, value, _))
        validations.find(_.isSuccess) getOrElse {
          ValidationError(
            ("The expected value %s of the `%s` property is invalid for the given value: %s.") format (
              generate(schemaType), property, jsTypeNameFor(extracted)),
            fieldName).fail
        }
      case JString(s) if (Seq("any") ++ jsTypeNamesFor(extracted)).contains(s) => value.success
      case JString(jsType) =>
        ValidationError(
          ("The expected value [%s] of the `%s` property is invalid for the given value: %s.") format (jsType, property, jsTypeNameFor(extracted)),
          fieldName).fail
      case m: JObject => // TODO: use an actual schema validator once all the validations have been implemented
        (m \ property, m \ "minLength", m \ "divisibleBy") match {
          case (JString("string") | JNull | JUndefined, JInt(_), JNull | JUndefined) => (new MinLengthValidator).validateValue(fieldName, value, m)
          case (JString("number"), JNull | JUndefined, JInt(_)) => (new DivisibleByValidator).validateValue(fieldName, value, m)
          case (JString("string"), JNull | JUndefined, JNull | JUndefined) => validateType(fieldName, value, m \ property)
          case _ => value.success
        }

      case m => ValidationError(
        ("The expected value [%s] of the `%s` property is invalid for the given value: %s.") format (generate(m), property, jsTypeNameFor(extracted)),
        fieldName).fail
    }
  }
}

