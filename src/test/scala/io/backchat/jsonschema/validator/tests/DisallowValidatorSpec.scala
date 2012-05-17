package io.backchat
package jsonschema
package validator
package tests

class DisallowValidatorSpec extends SpecificationBase with ValidatorSpec { def is =

  "A DisallowValidator should" ^
    "succeeds for valid syntax" ! validatesSyntax ^
    "fails for valid syntax" ! validatesInvalidSyntax ^
    "when validates data" ! validatesData ^
  end

  val validator: SchemaValidator = new DisallowValidator

  val file: String = "/keyword/disallow.json"
}
