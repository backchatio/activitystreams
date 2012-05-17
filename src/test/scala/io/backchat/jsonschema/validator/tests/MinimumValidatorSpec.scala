package io.backchat.jsonschema
package validator
package tests

import io.backchat.SpecificationBase

class MinimumValidatorSpec extends SpecificationBase with ValidatorSpec {

  val className: String = "MinimumValidator"
  val validator: SchemaValidator = new MinimumValidator

  val file: String = "/keyword/minimum.json"
}
