package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class FormatValidator extends SchemaValidator {
  val property = "format"

  def validateSyntax(value: Json.JValue) = value \ property match {
    case JString(s) if JsonSchema.format(s).isDefined => value.success
    case _: JString =>
      ValidationError("Only %s are allowed in the format property" % JsonSchema.formats.keys.toString, property).failNel
    case _ =>
      ValidationError("The formats property must be a string.", property).failNel
  }

  def validateValue(fieldName: String, value: Json.JValue, schema: JsonSchema) = value \ fieldName match {
    case s: JString => JsonSchema.format((schema \ property).valueAs[String]).get(s).liftFailNel map (_ => value)
    case _ => ValidationError("Not valid for `%s` format." % (schema \ property).valueAs[String], property).failNel
  }
}
