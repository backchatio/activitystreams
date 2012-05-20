package io.backchat
package jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

class DisallowValidator extends TypeValidator {

  override val property = "disallow"

  override def validateValue(fieldName: String, data: JValue, schema: JValue) = {
    if (super.validateValue(fieldName, data, schema).isSuccess)
      ValidationError("The value %s is disallowed" % generate(data \ fieldName), fieldName).failNel
    else data.successNel
  }
}
