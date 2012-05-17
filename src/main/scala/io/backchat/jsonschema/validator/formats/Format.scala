package io.backchat.jsonschema
package validator
package formats

import com.codahale.jerkson.AST._
import scalaz.Validation

trait Format extends (JValue => Validation[ValidationError, JValue]) {
  def key: String
  def types: Set[Class[_ <: JValue]]
}
