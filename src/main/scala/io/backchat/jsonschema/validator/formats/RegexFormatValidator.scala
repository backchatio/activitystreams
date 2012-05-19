package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import Json._

class RegexFormatValidator extends Format {
  val key: String = "regex"
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(re) if EcmaRegex.isValid(re) => v1.success
    case JString(re) => ValidationError("Regex %s is invalid.", "").fail
    case _ =>  ValidationError("Regex is of an invalid type.", "").fail
  }

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
