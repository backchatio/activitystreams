package io.backchat.jsonschema
package validator
package tests

import io.backchat.SpecificationBase

class EnumValidatorTest extends SpecificationBase with ValidatorSpec {
  val className: String = "EnumValidator"

  def validator: SchemaValidator = new EnumValidator

  val file: String = "/keyword/enum.json"
}
