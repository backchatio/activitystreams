package io.backchat.jsonschema
package validator

import io.backchat.jsonschema.ValidationError
import com.codahale.jerkson.AST._
import com.codahale.jerkson.Json._
import scalaz._
import Scalaz._
import annotation.tailrec

object TypeValidator {
  val types: Seq[String] = List("string", "number", "integer", "boolean", "object", "array", "null", "any")

  @tailrec
  def jsTypeNameFor(v: JValue): String = v match {
    case _: JArray => "array"
    case _: JString => "string"
    case JNull | null => "null"
    case _: JInt => "integer"
    case _: JFloat => "number"
    case _: JObject => "object"
    case _: JBoolean => "boolean"
    case JField(_, jv) => jsTypeNameFor(jv)
  }
  @tailrec
  def jsTypeNamesFor(v: JValue): Seq[String] = (v match {
    case _: JArray => Seq("array")
    case _: JString => Seq("string")
    case JNull | null => Seq("null")
    case _: JInt => Seq("integer", "number")
    case _: JFloat => Seq("number")
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

  def validateValue(value: JValue, schema: JValue): Validation[ValidationError, JValue] =
    validateType(value, schema \ property)

  protected def validateType(value: JValue, schemaType: JValue): Validation[ValidationError, JValue] = schemaType  match {
    case JArray(elements) if elements.nonEmpty =>
      val validations = elements.map(validateType(value, _))
       validations.find(_.isSuccess) getOrElse {
        ValidationError(
          ("The expected value %s of the `%s` property is invalid for the given value: %s.") format (
            generate(schemaType), property, jsTypeNameFor(value)),
          "type").fail
      }
    case JString(s) if (Seq("any") ++ jsTypeNamesFor(value)).contains(s) => value.success
    case JString(jsType) =>
      ValidationError(
        ("The expected value [%s] of the `%s` property is invalid for the given value: %s.") format (jsType, property, jsTypeNameFor(value)),
        property).fail
    case m: JObject => // TODO: use an actual schema validator once all the validations have been implemented
      (m \ property, m \ "minLength", m \ "divisibleBy") match {
        case (JString("string") | JNull, JInt(_), JNull) => (new MinLengthValidator).validateValue(value, m)
        case (JString("number"), JNull, JInt(_)) => (new DivisibleByValidator).validateValue(value, m)
        case (JString("string"), JNull, JNull) => validateType(value, m \ property)
        case _ => value.success
      }

    case m => ValidationError(
              ("The expected value [%s] of the `%s` property is invalid for the given value: %s.") format (generate(m), property, jsTypeNameFor(value)),
              property).fail
  }
}

