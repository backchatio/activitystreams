package io.backchat.jsonschema
package validator.tests

import io.backchat.SpecificationBase
import validator.{AdditionalPropertiesValidator, SchemaValidator}

class AdditionalPropertiesValidatorSpec extends SpecificationBase with ValidatorSpec {

  val className: String = "AdditionalPropertiesValidator"

  val validator: SchemaValidator = new AdditionalPropertiesValidator

  val file: String = "/keyword/additionalProperties.json"

  override def validatesInvalidSyntax = success
}
