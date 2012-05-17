package io.backchat.jsonschema
package validator
package tests

import io.backchat.SpecificationBase

class MaximumValidatorSpec extends SpecificationBase with ValidatorSpec {
  val className: String = "MaximumValidator"
  val validator: SchemaValidator = new MaximumValidator

  val file: String = "/keyword/maximum.json"
}
