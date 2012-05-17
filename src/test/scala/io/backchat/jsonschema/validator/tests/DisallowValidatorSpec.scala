package io.backchat
package jsonschema
package validator
package tests

class DisallowValidatorSpec extends SpecificationBase with ValidatorSpec {

  val className: String = "DisallowValidator"

  val validator: SchemaValidator = new DisallowValidator

  val file: String = "/keyword/disallow.json"
}
