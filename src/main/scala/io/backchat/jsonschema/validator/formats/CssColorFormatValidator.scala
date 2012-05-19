package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import Json._
import io.backchat.CssColorParser

class CssColorFormatValidator extends Format {
  val key: String = "color"
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(v) if CssColorParser.parseColor(v).isDefined => v1.success
    case JString(v) => ValidationError("%s is not a valid CSS 2.1 color." % v, "").fail
    case _ => ValidationError("Only strings are allowed for css colors", "").fail
  }

  def types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
