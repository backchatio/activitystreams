package io.backchat.jsonschema
package validator
package formats

import scalaz.Validation
import Json.JValue

trait Format extends (JValue => Validation[ValidationError, JValue]) {
  def key: String
  def types: Set[Class[_ <: JValue]]
}
