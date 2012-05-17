package io.backchat
package jsonschema
package validator

import com.codahale.jerkson.AST._
import scalaz._
import Scalaz._
import Json._

class DisallowValidator extends TypeValidator {

  override val property = "disallow"

  override def validateValue(value: JValue, schema: JValue) = {
    if (super.validateValue(value, schema).isSuccess)
      ValidationError("The value %s is disallowed" % generate(value), property).fail
    else value.success
  }
}
