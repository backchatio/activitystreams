package io.backchat
package jsonschema
package validator

import com.codahale.jerkson.AST._
import scalaz._
import Scalaz._
import Json._

class DisallowValidator extends TypeValidator {

  override val property = "disallow"

  override def validateValue(fieldName: String, data: JValue, schema: JValue) = {
    if (super.validateValue(fieldName, data, schema).isSuccess)
      ValidationError("The value %s is disallowed" % generate(data \ fieldName), fieldName).fail
    else data.success
  }
}
